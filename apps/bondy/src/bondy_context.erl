
%% =============================================================================
%%  bondy_context.erl -
%%
%%  Copyright (c) 2016-2019 Ngineo Limited t/a Leapsight. All rights reserved.
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
%% A Bondy Context lets you access information that defines the state of an
%% interaction. In a typical interacion, several actors or objects have a hand
%% in what is going on e.g. bondy_session, wamp_realm, etc.
%%
%% The Bondy Context is passed as an argument through the whole request-response
%%  loop to provide access to that information.
%% @end
%% =============================================================================
-module(bondy_context).
-include("bondy.hrl").
-include_lib("wamp/include/wamp.hrl").

-type subprotocol_2()        ::  subprotocol()
                                | {http, text, json | msgpack}.


-type t()       ::  #{
    %% Mandatory
    id := id(),
    node := atom(),
    request_id := maybe_undefined(id()),
    request_timeout := non_neg_integer(),
    request_timestamp := maybe_undefined(integer()),
    request_details := maybe_undefined(map()),
    request_bytes := maybe_undefined(non_neg_integer()),
    call_timeout := non_neg_integer(),
    %% Optional
    realm_uri => uri(),
    session => maybe_undefined(bondy_session:t()), %% TODO Remove, keep session_id
    session_id => id(),
    peer => inet:peername(),
    peername => binary(),
    transport => transport() | http,
    frame_type => frame_type(),
    encoding => encoding(),
    authmethod => binary(),
    authid => binary(),
    roles => map()
}.
-export_type([t/0]).

-export([agent/1]).
-export([authid/1]).
-export([call_timeout/1]).
-export([close/1]).
-export([encoding/1]).
-export([has_session/1]).
-export([is_feature_enabled/3]).
-export([local_context/1]).
-export([new/0]).
-export([new/2]).
-export([node/1]).
-export([peer/1]).
-export([peer_id/1]).
-export([peername/1]).
-export([realm_uri/1]).
-export([request_bytes/1]).
-export([request_details/1]).
-export([request_id/1]).
-export([request_timeout/1]).
-export([request_timestamp/1]).
-export([reset/1]).
-export([roles/1]).
-export([session/1]).
-export([session_id/1]).
-export([set_call_timeout/2]).
-export([set_peer/2]).
-export([set_realm_uri/2]).
-export([set_request_bytes/2]).
-export([set_request_id/2]).
-export([set_request_timeout/2]).
-export([set_request_timestamp/2]).
-export([set_session/2]).
-export([set_subprotocol/2]).
-export([subprotocol/1]).
-export([telemetry_metadata/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% Initialises a new context.
%% @end
%% -----------------------------------------------------------------------------
-spec new() -> t().

new() ->
    #{
        id => bondy_utils:get_id(global),
        node => bondy_peer_service:mynode(),
        call_timeout => bondy_config:get(wamp_call_timeout),
        request_details => undefined,
        request_timestamp => undefined,
        request_id => undefined,
        request_bytes => undefined,
        request_timeout => 0
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
local_context(RealmUri) when is_binary(RealmUri) ->
    Ctxt = new(),
    Ctxt#{realm_uri => RealmUri}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(inet:peername(), subprotocol_2()) -> t().

new(Peer, Subprotocol) ->
    set_subprotocol(set_peer(new(), Peer), Subprotocol).


%% -----------------------------------------------------------------------------
%% @doc
%% Resets the context. Returns a copy of Ctxt where the following attributes
%% have been reset: request_id, request_timeout, request_timestamp
%% @end
%% -----------------------------------------------------------------------------
-spec reset(t()) -> t().

reset(Ctxt) ->
    Ctxt#{
        request_details => undefined,
        request_timestamp => undefined,
        request_id => undefined,
        request_timeout => 0
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
telemetry_metadata(Ctxt) ->
    Keys = [
        %% node,
        realm_uri, session_id, peername, agent, authmethod,
        transport, frame_type, encoding
    ],
    maps:with(Keys, Ctxt).


%% -----------------------------------------------------------------------------
%% @doc
%% Closes the context. This function calls {@link bondy_session:close/1}
%% and {@link bondy_router:close_context/1}.
%% @end
%% -----------------------------------------------------------------------------
-spec close(t()) -> ok.

close(Ctxt0) ->
    %% We cleanup router first as cleanup requires the session
    try session(Ctxt0) of
        Session ->
            _ = bondy_router:close_context(Ctxt0),
            bondy_session:close(Session)
    catch
        _:_ ->
            ok
    end.





%% -----------------------------------------------------------------------------
%% @doc
%% Returns the peer of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec node(t()) -> atom().

node(#{node := Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the peer of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec peer(t()) -> inet:peername().

peer(#{peer := Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Set the peer to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_peer(t(), inet:peername()) -> t().

set_peer(Ctxt, {{_, _, _, _}, _Port} = Peer) when is_map(Ctxt) ->
    Ctxt#{
        peer => Peer,
        peername => inet_utils:peername_to_binary(Peer)
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the peer of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec agent(t()) -> maybe_undefined(binary()).

agent(#{agent := Val}) -> Val;
agent(_) -> undefined.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the peer of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec peername(t()) -> binary().

peername(#{peername := Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the subprotocol of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec subprotocol(t()) -> subprotocol_2().

subprotocol(#{subprotocol := Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the encoding used by the peer of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec encoding(t()) -> encoding().

encoding(#{subprotocol := {_, _, Val}}) -> Val.



%% -----------------------------------------------------------------------------
%% @doc
%% Set the peer to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_subprotocol(t(), subprotocol_2()) -> t().

set_subprotocol(Ctxt, {T, FT, E}) when is_map(Ctxt) ->
    Ctxt#{
        transport => T,
        frame_type => FT,
        encoding => E
    }.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the roles of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec roles(t()) -> maybe_none(map()).

roles(Ctxt) ->
    bondy_session:roles(session(Ctxt)).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the realm uri of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec realm_uri(t()) -> uri().

realm_uri(#{realm_uri := Val}) -> Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the realm uri of the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_realm_uri(t(), uri()) -> t().

set_realm_uri(Ctxt, Uri) ->
    Ctxt#{realm_uri => Uri}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec authid(t()) -> maybe_undefined(binary()).

authid(#{authid := Val}) -> Val;
authid(#{}) -> undefined.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the sessionId of the provided context or 'undefined'
%% if there is none.
%% @end
%% -----------------------------------------------------------------------------
-spec session_id(t()) -> maybe_undefined(id()).

session_id(#{session_id := Id}) ->
    Id;
session_id(#{}) ->
    undefined.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns true if the context is associated with a session,
%% false otherwise.
%% @end
%% -----------------------------------------------------------------------------
-spec has_session(t()) -> boolean().

has_session(#{session_id := _}) -> true;
has_session(#{}) -> false.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the sessionId to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_session(t(), bondy_session:t()) -> t().

set_session(Ctxt, S) ->
    Ctxt#{
        %% TODO Do not set session, just some properties
        session => S,
        agent => bondy_session:agent(S),
        session_id => bondy_session:id(S),
        peer_id => bondy_session:peer_id(S)
    }.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec peer_id(t()) -> peer_id().

peer_id(#{peer_id := PeerId}) ->
    PeerId.


%% -----------------------------------------------------------------------------
%% @doc
%% Fetches and returns the bondy_session for the associated sessionId.
%% @end
%% -----------------------------------------------------------------------------
-spec session(t()) -> maybe_none(bondy_session:t()).

session(#{session_id := Id}) ->
    bondy_session:fetch(Id).


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current request id.
%% @end
%% -----------------------------------------------------------------------------
-spec request_id(t()) -> id().

request_id(#{request_id := Val}) ->
    Val.

%% -----------------------------------------------------------------------------
%% @doc
%% Sets the current request id to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_request_id(t(), id()) -> t().

set_request_id(Ctxt, ReqId) ->
    Ctxt#{set_request_id => ReqId}.



%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current request id.
%% @end
%% -----------------------------------------------------------------------------
-spec request_bytes(t()) -> maybe_undefined(non_neg_integer()).

request_bytes(#{request_bytes := Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the current request id to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_request_bytes(t(), non_neg_integer()) -> t().

set_request_bytes(Ctxt, Bytes) ->
    Ctxt#{set_request_bytes => Bytes}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current request timeout.
%% @end
%% -----------------------------------------------------------------------------
-spec request_timeout(t()) -> non_neg_integer().

request_timeout(#{request_timeout := Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the current request timeout to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_request_timeout(t(), non_neg_integer()) -> t().

set_request_timeout(Ctxt, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    Ctxt#{request_timeout => Timeout}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current WAMP call timeout.
%% @end
%% -----------------------------------------------------------------------------
-spec call_timeout(t()) -> non_neg_integer().

call_timeout(#{call_timeout := Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the current WAMP call timeout to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_call_timeout(t(), non_neg_integer()) -> t().

set_call_timeout(Ctxt, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    Ctxt#{call_timeout => Timeout}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current request timestamp in native unit
%% @end
%% -----------------------------------------------------------------------------
-spec request_timestamp(t()) -> integer().

request_timestamp(#{request_timestamp := Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Sets the current request timeout to the provided context.
%% @end
%% -----------------------------------------------------------------------------
-spec set_request_timestamp(t(), integer()) -> t().

set_request_timestamp(Ctxt, Timestamp) when is_integer(Timestamp) ->
    Ctxt#{request_timestamp => Timestamp}.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns the current request timestamp in native unit
%% @end
%% -----------------------------------------------------------------------------
-spec request_details(t()) -> maybe_undefined(map()).

request_details(#{request_details := Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns true if the feature Feature is enabled for role Role.
%% @end
%% -----------------------------------------------------------------------------
-spec is_feature_enabled(t(), atom(), binary()) -> boolean().

is_feature_enabled(Ctxt, Role, Feature) ->
    maps_utils:get_path([Role, Feature], roles(Ctxt), false).
