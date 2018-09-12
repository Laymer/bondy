%% =============================================================================
%%  bondy_registry_backend.erl -
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
-module(bondy_registry_entry).
-include_lib("wamp/include/wamp.hrl").
-include("bondy.hrl").

%% An entry denotes a registration or a subscription.
%% Entries are immutable.
-record(entry, {
    key                     ::  entry_key(),
    peer                    ::  bondy_wamp_peer:t(),
    uri                     ::  uri() | atom(),
    match_policy            ::  binary(),
    created                 ::  calendar:date_time() | atom(),
    options                 ::  map() | atom()
}).

-record(entry_key, {
    realm_uri               ::  uri() | '_',
    node                    ::  node(),
    session_id              ::  id() | '_' | undefined,   % the owner
    entry_id                ::  id() | '_',
    type                    ::  entry_type()
}).


-opaque t()                 ::  #entry{}.
-type entry_key()           ::  #entry_key{}.
-type entry_type()          ::  registration | subscription.
-type details_map()         ::  #{
    id => id(),
    created => calendar:date(),
    uri => uri(),
    match => binary()
}.

-export_type([t/0]).
-export_type([entry_type/0]).
-export_type([details_map/0]).

-export([created/1]).
-export([get_option/3]).
-export([id/1]).
-export([is_entry/1]).
-export([key/1]).
-export([key_pattern/3]).
-export([key_pattern/5]).
-export([match_policy/1]).
-export([new/4]).
-export([new/5]).
-export([node/1]).
-export([options/1]).
-export([pattern/4]).
-export([pattern/6]).
-export([peer/1]).
-export([pid/1]).
-export([realm_uri/1]).
-export([session_id/1]).
-export([to_details_map/1]).
-export([to_map/1]).
-export([type/1]).
-export([uri/1]).


%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(entry_type(), bondy_wamp_peer:t(), uri(), map()) -> t().

new(Type, Peer, Uri, Options) ->
    RegId = bondy_utils:get_id(global),
    new(Type, RegId, Peer, Uri, Options).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(entry_type(), id(), bondy_wamp_peer:t(), uri(), map()) -> t().

new(Type, RegId, Peer, Uri, Options) ->
    MatchPolicy = validate_match_policy(Options),
    RealmUri = bondy_wamp_peer:realm_uri(Peer),
    Node = bondy_wamp_peer:node(Peer),
    SessionId = bondy_wamp_peer:session_id(Peer),

    Key = #entry_key{
        realm_uri = RealmUri,
        node = Node,
        session_id = SessionId,
        entry_id = RegId,
        type = Type
    },
    #entry{
        key = Key,
        peer = Peer,
        uri = Uri,
        match_policy = MatchPolicy,
        created = calendar:local_time(),
        options = parse_options(Type, Options)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec pattern(entry_type(), uri(), id(), map()) -> t().

pattern(Type, RealmUri, EntryId, Options) ->
    MatchPolicy = validate_match_policy(pattern, Options),
    #entry{
        key = key_pattern(Type, RealmUri, '_', '_', EntryId),
        peer = '_',
        uri = '_',
        match_policy = MatchPolicy,
        created = '_',
        options = '_'
    }.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec pattern(entry_type(), uri(), atom(), id(), uri(), map()) -> t().

pattern(Type, RealmUri, Node, SessionId, Uri, Options) ->
    MatchPolicy = validate_match_policy(pattern, Options),
    #entry{
        key = key_pattern(Type, RealmUri, Node, SessionId, '_'),
        peer = '_',
        uri = Uri,
        match_policy = MatchPolicy,
        created = '_',
        options = '_'
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
key_pattern(Type, RealmUri, SessionId) ->
    key_pattern(Type, RealmUri, '_', SessionId, '_').


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
key_pattern(Type, RealmUri, Node, SessionId, EntryId)
when (Type =:= subscription orelse Type =:= registration)
andalso is_atom(Node)
andalso (is_binary(RealmUri) orelse RealmUri == '_')
andalso (
    is_integer(SessionId) orelse SessionId == '_' orelse SessionId == undefined
)
andalso (is_integer(EntryId) orelse EntryId == '_') ->
    #entry_key{
        realm_uri = RealmUri,
        node = Node,
        session_id = SessionId,
        entry_id = EntryId,
        type = Type
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
is_entry(#entry{}) -> true;
is_entry(_) -> false.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the subscription's or registration's realm_uri property.
%% @end
%% -----------------------------------------------------------------------------
-spec key(t()) -> uri().
key(#entry{key = Key}) ->
    Key.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the subscription's or registration's realm_uri property.
%% @end
%% -----------------------------------------------------------------------------
-spec realm_uri(t() | entry_key()) -> uri() | undefined.
realm_uri(#entry{key = Key}) ->
    Key#entry_key.realm_uri;

realm_uri(#entry_key{} = Key) ->
    Key#entry_key.realm_uri.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the subscription's or registration's session_id
%% property.
%% @end
%% -----------------------------------------------------------------------------
-spec node(t() | entry_key()) -> atom().
node(#entry{key = Key}) ->
    Key#entry_key.node;

node(#entry_key{} = Key) ->
    Key#entry_key.node.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the subscription's or registration's session_id
%% property.
%% @end
%% -----------------------------------------------------------------------------
-spec pid(t() | entry_key()) -> pid().
pid(#entry{peer = Peer}) ->
    bondy_wamp_peer:pid(Peer).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the subscription's or registration's session_id
%% property.
%% @end
%% -----------------------------------------------------------------------------
-spec session_id(t() | entry_key()) -> id() | undefined.
session_id(#entry{key = Key}) ->
    Key#entry_key.session_id;

session_id(#entry_key{} = Key) ->
    Key#entry_key.session_id.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the peer() of the subscription or registration
%% @end
%% -----------------------------------------------------------------------------
-spec peer(t() | entry_key()) -> bondy_wamp_peer:t().
peer(#entry{peer = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the subscription's or registration's id
%% property.
%% @end
%% -----------------------------------------------------------------------------
-spec id(t() | entry_key()) -> id().
id(#entry{key = Key}) ->
    Key#entry_key.entry_id;

id(#entry_key{} = Key) ->
    Key#entry_key.entry_id.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the type of the entry, the atom 'registration' or 'subscription'.
%% @end
%% -----------------------------------------------------------------------------
-spec type(t() | entry_key()) -> entry_type().
type(#entry{key = Key}) ->
    Key#entry_key.type;

type(#entry_key{} = Key) ->
    Key#entry_key.type.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the uri this entry is about i.e. either a subscription topic_uri or
%% a registration procedure_uri.
%% @end
%% -----------------------------------------------------------------------------
-spec uri(t()) -> uri().
uri(#entry{uri = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the match_policy used by this subscription or regitration.
%% @end
%% -----------------------------------------------------------------------------
-spec match_policy(t()) -> binary().
match_policy(#entry{match_policy = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the time when this entry was created.
%% @end
%% -----------------------------------------------------------------------------
-spec created(t()) -> calendar:date_time().
created(#entry{created = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the value of the 'options' property of the entry.
%% @end
%% -----------------------------------------------------------------------------
-spec options(t()) -> map().
options(#entry{options = Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_option(t(), any(), any()) -> any().
get_option(#entry{options = Opts}, Key, Default) ->
    maps:get(Key, Opts, Default).


%% -----------------------------------------------------------------------------
%% @doc
%% Converts the entry into a map according to the WAMP protocol Details
%% dictionary format.
%% @end
%% -----------------------------------------------------------------------------
-spec to_details_map(t()) -> details_map().

to_details_map(#entry{key = Key} = E) ->
    #{
        id =>  Key#entry_key.entry_id,
        created => E#entry.created,
        uri => E#entry.uri,
        match => E#entry.match_policy,
        invoke => maps:get(invoke, E#entry.options, ?INVOKE_SINGLE)
    }.

%% -----------------------------------------------------------------------------
%% @doc
%% Converts the entry into a map according to the WAMP protocol Details
%% dictionary format.
%% @end
%% -----------------------------------------------------------------------------
-spec to_map(t()) -> details_map().

to_map(#entry{key = Key} = E) ->
    #{
        id =>  Key#entry_key.entry_id,
        session_id => Key#entry_key.session_id,
        node => Key#entry_key.node,
        created => E#entry.created,
        uri => E#entry.uri,
        pid => list_to_binary(pid_to_list(pid(E))),
        match => E#entry.match_policy,
        options => E#entry.options
    }.




%% =============================================================================
%% PRIVATE
%% =============================================================================

validate_match_policy(Options) ->
    validate_match_policy(key, Options).

%% @private
-spec validate_match_policy(map()) -> binary().

validate_match_policy(pattern, '_') ->
    '_';

validate_match_policy(_, Options) when is_map(Options) ->
    case maps:get(match, Options, ?EXACT_MATCH) of
        ?EXACT_MATCH = P -> P;
        ?PREFIX_MATCH = P -> P;
        ?WILDCARD_MATCH = P -> P;
        P ->
            error({invalid_match_policy, P})
    end.


%% @private
parse_options(subscription, Opts) ->
    parse_subscription_options(Opts);

parse_options(registration, Opts) ->
    parse_registration_options(Opts).


%% @private
parse_subscription_options(Opts) ->
    maps:without([match], Opts).


%% @private
parse_registration_options(Opts) ->
    maps:without([match], Opts).