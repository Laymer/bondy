
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
-include_lib("opencensus/include/opencensus.hrl").

-type subprotocol_2()        ::  subprotocol()
                                | {http, text, json | msgpack}.


-type t()       ::  #{
    %% Mandatory
    id := id(),
    node := atom(),
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
    roles => map(),
    %% Request context
    request_id := maybe_undefined(id()),
    request_timestamp := maybe_undefined(integer()),
    request_timeout := non_neg_integer(),
    request_deadline := non_neg_integer(),
    request_details := maybe_undefined(map()),
    request_bytes := maybe_undefined(non_neg_integer()),
    trace_context => ctx:t()
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
-export([trace_context/1]).
-export([set_trace_context/2]).

-export([span_context/1]).
-export([current_span_context/1]).
-export([with_child_span/2]).
-export([with_parent_span/2]).
-export([with_child_span/3]).
-export([with_span_context/2]).
-export([finish_span/1]).



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
        request_timeout => 0,
        trace_context => ctx:new()
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
        request_timeout => 0,
        trace_context => ctx:new()
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
%% @end
%% -----------------------------------------------------------------------------
-spec trace_context(t()) -> ctx:t().

trace_context(#{trace_context := Map}) -> Map.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set_trace_context(t(), opencensus:span_ctx()) -> t().

set_trace_context(Ctxt, TraceCtxt) ->
    maps:put(trace_context, TraceCtxt, Ctxt).


%% -----------------------------------------------------------------------------
%% @doc Return the opencesus span context, if it exists, from Ctx.
%% Otherwise create a new one
%% @end
%% -----------------------------------------------------------------------------
-spec span_context(t()) -> opencensus:span_ctx().

span_context(Ctxt) ->
    oc_trace:from_ctx(trace_context(Ctxt)).



%% -----------------------------------------------------------------------------
%% @doc Return the current span context in a `Ctxt' or `undefined'.
%% @end
%% -----------------------------------------------------------------------------
-spec current_span_context(t()) -> maybe(opencensus:span_ctx()).

current_span_context(Ctxt) ->
    oc_trace:current_span_ctx(trace_context(Ctxt)).


%% -----------------------------------------------------------------------------
%% @doc Set the current span context in a context to `SpanCtx'.
%% @end
%% -----------------------------------------------------------------------------
-spec with_span_context(t(), opencensus:span_ctx()) -> t().

with_span_context(Ctxt, undefined) ->
    Ctxt;

with_span_context(Ctxt, SpanCtxt) ->
    TraceCtxt0 = trace_context(Ctxt),
    TraceCtxt1 = oc_trace:with_span_ctx(TraceCtxt0, SpanCtxt),
    set_trace_context(Ctxt, TraceCtxt1).


%% -----------------------------------------------------------------------------
%% @doc Create a child span with parent from the current context `Ctx'. And
%% sets it as the current span context in `Ctx'.
%% @end
%% -----------------------------------------------------------------------------
-spec with_child_span(t(), Name :: unicode:unicode_binary()) -> t().

with_child_span(Ctxt, Name) when is_binary(Name) ->
    TraceCtxt0 = trace_context(Ctxt),
    TraceCtxt1 = oc_trace:with_child_span(TraceCtxt0, Name),
    set_trace_context(Ctxt, TraceCtxt1).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec with_child_span(Ctx, Name, Options) -> Ctx when
      Ctx :: t(),
      Name :: unicode:unicode_binary(),
      Options :: #{remote_parent => boolean(),
                   sampler => module(),
                   attributes => opencensus:attributes()}.

with_child_span(Ctxt, Name, Options) ->
    TraceCtxt0 = trace_context(Ctxt),
    TraceCtxt1 = oc_trace:with_child_span(TraceCtxt0, Name, Options),
    set_trace_context(Ctxt, TraceCtxt1).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
with_parent_span(Ctxt0, SpanCtxt) ->
    Ctxt1 = with_span_context(Ctxt0, SpanCtxt),
    case current_span_context(Ctxt1) of
        SpanCtxt ->
            Ctxt1;
        Child ->
            with_child_span(Ctxt1, Child)
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec finish_span(t()) -> boolean().

finish_span(Ctxt) ->
    oc_trace:finish_span(current_span_context(Ctxt)).


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





%% =============================================================================
%% TELEMETRY
%% =============================================================================



