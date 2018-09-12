%% =============================================================================
%%  bondy_session.erl -
%%
%%  Copyright (c) 2016-2017 Ngineo Limited t/a Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================

%% =============================================================================
%% @doc
%% A Session (wamp session) is a transient conversation between two
%% WAMP Peers attached to a Realm and running over a Transport.
%%
%% Bondy implementation ties the lifetime of the underlying transport connection
%% for a WAMP connection to that of a WAMP Session
%% i.e. establish a new transport-layer connection as part of each new
%% session establishment.
%%
%% A Bondy Session is a not an application Session and is not a store for
%% application specific content (an application session store should be
%% implemented as a service i.e. a Callee).
%%
%% Currently sessions are not persistent i.e. if the connection closes the
%% session data will be lost.
%%
%% @end
%% =============================================================================
-module(bondy_session).
-include("bondy.hrl").
-include_lib("wamp/include/wamp.hrl").

-define(SESSION_SPACE_NAME, ?MODULE).
-define(SESSION_SEQ_POS, #session.seq).
-define(DEFAULT_RATE, #rate_window{limit = 1000, duration = 1}).
-define(DEFAULT_QUOTA, #quota_window{limit = 1000, duration = 1}).% TODO


-record(quota_window, {
    limit                           ::  pos_integer(),
    %% time when the quota resets in secs
    renews                          ::  pos_integer(),
    %% number of requests remaining in quota
    remaining                       ::  pos_integer(),
    %% time in seconds during which quota is valid e.g.
    %% the length of the window
    duration                        ::  pos_integer()
}).
-type quota_window()                ::  #quota_window{}.

-record(rate_window, {
    %% max number of messages allowed during window
    limit                           ::  pos_integer(),
    %% duration of window in seconds
    duration                        ::  pos_integer()
}).
-type rate_window()                 ::  #rate_window{}.


%% -record(oauth2_token, {
%%     authid                          ::  binary(),
%%     access_token                    ::  binary(),
%%     refresh_token                   ::  binary()
%% }).

%% -record(wamp_credential, {
%%     %% The authentication ID of the session that joined
%%     authid                          ::  binary(),
%%     %% The authentication role of the session that joined
%%     authrole                        ::  binary(),
%%     %% The authentication method that was used for authentication
%%     authmethod                      ::  binary(),
%%     %% The provider that performed the authentication of the session that joined
%%     authprovider = <<"bondy">>       ::  binary()
%% }).

%% -type credential()                  ::  #oauth2_token{} | #wamp_credential{}.

% THE NEW SESSION
% -record(session, {
%     id                              ::  id(),
%     realm_uri                       ::  uri(),
%     expires_in                      ::  pos_integer(),
%     seq = 0                         ::  non_neg_integer(),
%     rate = ?DEFAULT_RATE            ::  rate(),
%     quota = ?DEFAULT_QUOTA          ::  quota(),
%     is_active = true                ::  boolean(),
%     %% The credential used to establish the session
%     credential                      ::  credential(),
%     rate_window = ?DEFAULT_RATE     ::  rate_window(),
%     quota_window = ?DEFAULT_QUOTA   ::  quota_window(),
%     created                         ::  pos_integer(),
%     last_updated                    ::  pos_integer(),
%     resumed                         ::  boolean(),
%     resumable                       ::  boolean(),
%     resumed_token                   ::  binary(),
%     metadata = #{}                  ::  map()
% }).

-record(session, {
    id                              ::  id(),
    realm_uri                       ::  uri(),
    node                            ::  atom(),
    pid                             ::  pid(),
    %% If owner of the session.
    %% This is either pid of the TCP or WS handler process or
    %% the cowboy handler.
    peer                            ::  bondy_wamp_peer:local() | undefined,
    %% User-Agent HTTP header or WAMP equivalent
    agent                           ::  binary(),
    %% Sequence number used for ID generation
    seq = 0                         ::  non_neg_integer(),
    %% Peer WAMP Roles played by peer
    roles                           ::  map() | undefined,
    %% WAMP Auth
    authid                          ::  binary() | undefined,
    authrole                        ::  binary() | undefined,
    authmethod                      ::  binary() | undefined,
    %% Expiration and Limits
    created                         ::  calendar:date_time(),
    expires_in                      ::  pos_integer() | infinity,
    rate = ?DEFAULT_RATE            ::  rate_window(),
    quota = ?DEFAULT_QUOTA          ::  quota_window()
}).

-type session()                 ::  #session{}.
-type session_opts()            ::  #{roles => map()}.
-type details()                 ::  #{
                                        session => id(),
                                        authid => id(),
                                        authrole => binary(),
                                        authmethod => binary(),
                                        authprovider => binary(),
                                        transport => #{
                                            peername => binary()
                                        }
                                    }.

-export([close/1]).
-export([created/1]).
-export([fetch/1]).
-export([id/1]).
-export([incr_seq/1]).
-export([list/0]).
-export([list/1]).
-export([list_peers/1]).
-export([list_peers/2]).
-export([lookup/1]).
-export([new/2]).
-export([new/3]).
-export([open/3]).
-export([open/4]).
-export([set_peer/2]).
-export([peer/1]).
-export([realm_uri/1]).
-export([roles/1]).
-export([size/0]).
-export([to_details_map/1]).
-export([update/1]).
% -export([stats/0]).

%% -export([features/1]).
%% -export([subscriptions/1]).
%% -export([registrations/1]).



%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc Creates a new transient session (not persisted)
%% @end
%% -----------------------------------------------------------------------------
-spec new(uri() | bondy_realm:realm(), session_opts()) ->
    session() | no_return().

new(RealmUri, Opts) when is_binary(RealmUri) ->
    new(bondy_utils:get_id(global), bondy_realm:fetch(RealmUri), Opts);

new(Realm, Opts) when is_map(Opts) ->
    new(bondy_utils:get_id(global), Realm, Opts).


-spec new(id(), uri() | bondy_realm:realm(), session_opts()) ->
    session() | no_return().

new(Id, RealmUri, Opts) when is_binary(RealmUri) ->
    new(Id, bondy_realm:fetch(RealmUri), Opts);

new(Id, Realm, Opts) when is_map(Opts) ->
    RealmUri = bondy_realm:uri(Realm),
    S0 = #session{
        id = Id,
        realm_uri = RealmUri,
        created = calendar:local_time()
    },
    parse_details(Opts, S0).


%% -----------------------------------------------------------------------------
%% @doc
%% Creates a new session provided the RealmUri exists or can be dynamically
%% created. It assigns a new Id.
%% It calls {@link bondy_utils:get_realm/1} which will fail with an exception
%% if the realm does not exist or cannot be created
%% -----------------------------------------------------------------------------
-spec open(
    bondy_wamp_peer:local(), uri() | bondy_realm:realm(), session_opts()) ->
    session() | no_return().

open(Peer, RealmUri, Opts) when is_binary(RealmUri) ->
    open(bondy_utils:get_id(global), Peer, bondy_realm:fetch(RealmUri), Opts);

open(Peer, Realm, Opts) when is_map(Opts) ->
    open(bondy_utils:get_id(global), Peer, Realm, Opts).


%% -----------------------------------------------------------------------------
%% @doc
%% Creates a new session provided the RealmUri exists or can be dynamically
%% created.
%% It calls {@link bondy_utils:get_realm/1} which will fail with an exception
%% if the realm does not exist or cannot be created
%% -----------------------------------------------------------------------------
-spec open(
    id(),
    bondy_wamp_peer:local(),
    uri() | bondy_realm:realm(),
    session_opts()
    ) -> session() | no_return().

open(Id, Peer, RealmUri, Opts) when is_binary(RealmUri) ->
    open(Id, Peer, bondy_realm:fetch(RealmUri), Opts);

open(Id, Peer, Realm, Opts) when is_map(Opts) ->
    RealmUri = bondy_realm:uri(Realm),
    S1 = new(Id, Realm, Opts),
    S2 = set_peer(S1, Peer),
    {IP, _} = bondy_wamp_peer:peername(Peer),

    case ets:insert_new(table(Id), S2) of
        true ->
            ok = bondy_stats:update({session_opened, RealmUri, Id, IP}),
            S2;
        false ->
            error({integrity_constraint_violation, Id})
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec update(session()) -> ok.
update(#session{id = Id} = S) ->
    true = ets:insert(table(Id), S),
    ok.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec close(id() | session()) -> ok.
close(#session{id = Id} = S) ->
    {IP, _} = bondy_wamp_peer:peername(S#session.peer),
    Realm = S#session.realm_uri,
    Secs = calendar:datetime_to_gregorian_seconds(calendar:local_time()) - calendar:datetime_to_gregorian_seconds(S#session.created),
    ok = bondy_stats:update(
        {session_closed, Id, Realm, IP, Secs}),
    true = ets:delete(table(Id), Id),
    _ = lager:debug("Session closed; session_id=~p, realm=~s", [Id, Realm]),
    ok;

close(Id) ->
    case lookup(Id) of
        {error, not_found} -> ok;
        Session -> close(Session)
    end.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec id(session()) -> id().

id(#session{id = Id}) ->
    Id.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec realm_uri(id() | session()) -> uri().
realm_uri(#session{realm_uri = Val}) ->
    Val;
realm_uri(Id) ->
    #session{realm_uri = Val} = fetch(Id),
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec roles(id() | session()) -> map().
roles(#session{roles = Val}) ->
    Val;
roles(Id) ->
    #session{roles = Val} = fetch(Id),
    Val.


%% -----------------------------------------------------------------------------
%% @doc Returns the identifier for the peer currently associated with this
%% session
%% @end
%% -----------------------------------------------------------------------------
-spec peer(id() | session()) -> bondy_wamp_peer:local().

peer(#session{peer = Val}) ->
    Val;

peer(SessionId) ->
    peer(fetch(SessionId)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set_peer(id() | session(), bondy_wamp_peer:local()) -> session().
set_peer(#session{} = Session, Peer) ->
    Session#session{
        peer = Peer,
        %% For indexing
        node = bondy_wamp_peer:node(Peer),
        pid = bondy_wamp_peer:pid(Peer)
    };

set_peer(Id, Peer) ->
    set_peer(fetch(Id), Peer).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec created(id() | session()) -> calendar:date_time().

created(#session{created = Val}) -> Val;
created(Id) -> created(fetch(Id)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec incr_seq(id() | session()) -> map().

incr_seq(#session{id = Id}) ->
    incr_seq(Id);

incr_seq(SessionId) when is_integer(SessionId), SessionId >= 0 ->
    Tab = tuplespace:locate_table(?SESSION_SPACE_NAME, SessionId),
    ets:update_counter(Tab, SessionId, {?SESSION_SEQ_POS, 1, ?MAX_ID, 0}).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the number of sessions in the tuplespace.
%% @end
%% -----------------------------------------------------------------------------
-spec size() -> non_neg_integer().

size() ->
    tuplespace:size(?SESSION_SPACE_NAME).


%% -----------------------------------------------------------------------------
%% @doc
%% Retrieves the session identified by Id from the tuplespace or 'not_found'
%% if it doesn't exist.
%% @end
%% -----------------------------------------------------------------------------
-spec lookup(id()) -> session() | {error, not_found}.

lookup(Id) ->
    case do_lookup(Id) of
        #session{} = Session ->
            Session;
        Error ->
            Error
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% Retrieves the session identified by Id from the tuplespace. If the session
%% does not exist it fails with reason '{badarg, Id}'.
%% @end
%% -----------------------------------------------------------------------------
-spec fetch(id()) -> session() | no_return().

fetch(Id) ->
    case lookup(Id) of
        {error, not_found} ->
            error({badarg, Id});
        Session ->
            Session
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
%% @TODO provide a limit and itereate on each table providing a custom
%% continuation
list() ->
    list(#{}).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
list(#{return := details_map}) ->
    Tabs = tuplespace:tables(?SESSION_SPACE_NAME),
    [to_details_map(X) || T <- Tabs, X <- ets:tab2list(T)];

list(_) ->
    Tabs = tuplespace:tables(?SESSION_SPACE_NAME),
    lists:append([ets:tab2list(T) || T <- Tabs]).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
list_peers(N) ->
    list_peers('_', N).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
list_peers(RealmUri, N) when is_integer(N), N >= 1 ->
    Tabs = tuplespace:tables(?SESSION_SPACE_NAME),
    do_list_peers(Tabs, RealmUri, N).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_details_map(id() | session()) -> details().

to_details_map(#session{} = S) ->
    Transport = case peer(S) of
        undefined ->
            #{};
        Peer ->
            Peername = bondy_wamp_peer:peername(Peer),
            #{
                peername => inet_utils:peername_to_binary(Peername)
            }
    end,

    #{
        session => S#session.id,
        authid => S#session.authid,
        authrole => S#session.authrole,
        authmethod => S#session.authmethod,
        authprovider => <<"com.leapsight.bondy">>,
        transport => Transport
    };

to_details_map(Id) ->
    to_details_map(fetch(Id)).



%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
parse_details(Opts, Session0)  when is_map(Opts) ->
    maps:fold(fun parse_details/3, Session0, Opts).


%% @private

parse_details(roles, Roles, Session) when is_map(Roles) ->
    length(maps:keys(Roles)) > 0 orelse
    error({invalid_options, missing_client_role}),
    Session#session{roles = parse_roles(Roles)};

parse_details(authid, V, Session) when is_binary(V) ->
    Session#session{authid = V};

parse_details(agent, V, Session) when is_binary(V) ->
    Session#session{agent = V};

parse_details(_, _, Session) ->
    Session.



%% ------------------------------------------------------------------------
%% private
%% @doc
%% Merges the client provided role features with the ones provided by
%% the router. This will become the feature set used by the router on
%% every session request.
%% @end
%% ------------------------------------------------------------------------
parse_roles(Roles) ->
    parse_roles(maps:keys(Roles), Roles).


%% @private
parse_roles([], Roles) ->
    Roles;

parse_roles([caller|T], Roles) ->
    F = bondy_utils:merge_map_flags(
        maps:get(caller, Roles), ?CALLER_FEATURES),
    parse_roles(T, Roles#{caller => F});

parse_roles([callee|T], Roles) ->
    F = bondy_utils:merge_map_flags(
        maps:get(callee, Roles), ?CALLEE_FEATURES),
    parse_roles(T, Roles#{callee => F});

parse_roles([subscriber|T], Roles) ->
    F = bondy_utils:merge_map_flags(
        maps:get(subscriber, Roles), ?SUBSCRIBER_FEATURES),
    parse_roles(T, Roles#{subscriber => F});

parse_roles([publisher|T], Roles) ->
    F = bondy_utils:merge_map_flags(
        maps:get(publisher, Roles), ?PUBLISHER_FEATURES),
    parse_roles(T, Roles#{publisher => F});

parse_roles([_|T], Roles) ->
    parse_roles(T, Roles).

%% @private
table(Id) ->
    tuplespace:locate_table(?SESSION_SPACE_NAME, Id).


%% @private
-spec do_lookup(id()) -> session() | {error, not_found}.

do_lookup(Id) ->
    Tab = table(Id),
    case ets:lookup(Tab, Id)  of
        [#session{} = Session] ->
            Session;
        [] ->
            {error, not_found}
    end.



%% @private
do_list_peers([], _, _) ->
    ?EOT;

do_list_peers([Tab | Tabs], RealmUri, N)
when is_binary(RealmUri) orelse RealmUri == '_' ->
    Pattern = #session{
        id = '$3',
        realm_uri = '$1',
        node = '$2',
        pid = '$4',
        peer = '_',
        agent = '_',
        seq = '_',
        roles = '_',
        authid = '_',
        authrole = '_',
        authmethod = '_',
        created = '_',
        expires_in = '_',
        rate = '_',
        quota = '_'
    },
    Conds = case RealmUri of
        '_' -> [];
        _ ->  [{'=:=', '$1', RealmUri}]
    end,
    Projection = [{{'$1', '$2', '$3', '$4'}}],
    MS = [{Pattern, Conds, Projection}],

    case ets:select(Tab, MS, N) of
        {L, Cont} ->
            FunCont = fun() ->
                do_list_peers({continuation, Tabs, RealmUri, N, Cont})
            end,
           {L, FunCont};
        ?EOT ->
            do_list_peers(Tabs, RealmUri, N)
    end.


%% @private
do_list_peers({continuation, [], _, _, ?EOT}) ->
    ?EOT;

do_list_peers({continuation, Tabs, RealmUri, N, ?EOT}) ->
    do_list_peers(Tabs, RealmUri, N);

do_list_peers({continuation, Tabs, RealmUri, N, Cont}) ->
    case ets:select(Cont) of
        ?EOT ->
            ?EOT;
        {L, Cont} ->
            FunCont = fun() ->
                do_list_peers({continuation, Tabs, RealmUri, N, Cont})
            end,
            {L, FunCont}
    end.