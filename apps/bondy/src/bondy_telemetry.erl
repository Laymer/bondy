%% =============================================================================
%%  bondy_telemetry - this module is used to configure the prometheus metrics
%%  and export the prometheus report.
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

%% -----------------------------------------------------------------------------
%% @doc
%%
%%
%% We follow https://prometheus.io/docs/practices/naming/
%% @end
%% -----------------------------------------------------------------------------
-module(bondy_telemetry).
-behaviour(gen_event).
-behaviour(prometheus_collector).

-include("bondy.hrl").
-include_lib("prometheus/include/prometheus.hrl").
-include_lib("wamp/include/wamp.hrl").

-define(WAMP_TAGS, [
    realm_uri, session_id, peername, agent, authmethod,
    transport, frame_type, encoding
]).

%% Used by promethues METRIC_NAME macro
-define(METRIC_NAME_PREFIX, "bondy_").

-record(state, {}).



%% API
-export([record/3]).
-export([report/0]).

%% PROMETHEUS_COLLECTOR CALLBACKS
-export([deregister_cleanup/1]).
-export([collect_mf/2]).


%% GEN_EVENT CALLBACKS
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-import(prometheus_model_helpers, [create_mf/4]).


%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec record(binary(), any(), bondy_context:t()) -> ok.

record(Measure, Value, Ctxt) ->
    %% We treat bondy_context:t() as opencensus:tags(), since both are maps
    ocp:record(Ctxt, Measure, Value).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
report() ->
    prometheus_text_format:format().





%% =============================================================================
%% PROMETHEUS_COLLECTOR CALLBACKS
%% =============================================================================



deregister_cleanup(_) ->
    ok.


-spec collect_mf(
    prometheus_registry:registry(), prometheus_collector:callback()) -> ok.

collect_mf(_Registry, Callback) ->
  Metrics = collector_metrics(),
  EnabledMetrics = enabled_metrics(),
  [add_metric_family(Metric, Callback)
   || {Name, _, _, _}=Metric <- Metrics, metric_enabled(Name, EnabledMetrics)],
  ok.



%% =============================================================================
%% GEN_EVENT CALLBACKS
%% =============================================================================



init([]) ->
    ok = setup(),
    State = #state{},
    {ok, State}.


handle_event({[socket, opened], #{count := N}, Meta}, State) ->
    oc_stat:record(Meta, <<"bondy/sockets_opened">>, N),
    {ok, State};

handle_event({[socket, closed], #{count := N}, Meta}, State) ->
    oc_stat:record(Meta, <<"bondy/sockets_opened">>, N),
    {ok, State};

handle_event({[socket, error], #{count := N}, Meta}, State) ->
    oc_stat:record(Meta, <<"bondy/socket_errors">>, N),
    {ok, State};

handle_event({[session, opened], #{count := N}, Meta}, State) ->
    oc_stat:record(Meta, <<"bondy/sessions_opened">>, N),
    {ok, State};

handle_event({[session, closed], #{count := N, duration := Ms}, Meta}, State) ->
    oc_stat:record(Meta, <<"bondy/sessions_closed">>, N),
    oc_stat:record(Meta, <<"bondy/session_duration">>, Ms),
    {ok, State};

handle_event({[wamp, message], Measures, Meta0}, State) ->
    {Message, Meta1} = maps:take(message, Meta0),
    handle_wamp_event(Message, Measures, Meta1, State);

handle_event(_Event, State) ->
    {ok, State}.


handle_call(Event, State) ->
    _ = lager:error(
        "Error handling call, reason=unsupported_event, event=~p", [Event]),
    {reply, {error, {unsupported_call, Event}}, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @private
handle_wamp_event(#error{} = M, #{count := N}, Meta0, State) ->
    Meta1 = maps:put(error_uri, M#error.error_uri, Meta0),
    oc_stat:record(Meta1, <<"bondy/wamp/error_messages">>, N),
    {ok, State};

handle_wamp_event(#abort{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/abort_messages">>, N),
    {ok, State};

handle_wamp_event(#authenticate{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/authenticate_messages">>, N),
    {ok, State};

handle_wamp_event(#call{} = M, #{count := N}, Meta0, State) ->
    Meta1 = Meta0#{procedure_uri => M#call.procedure_uri},
    oc_stat:record(Meta1, <<"bondy/wamp/call_messages">>, N),
    {ok, State};

handle_wamp_event(#cancel{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/cancel_messages">>, N),
    {ok, State};

handle_wamp_event(#challenge{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/challenge_messages">>, N),
    {ok, State};

handle_wamp_event(#event{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/event_messages">>, N),
    {ok, State};

handle_wamp_event(#goodbye{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/goodbye_messages">>, N),
    {ok, State};

handle_wamp_event(#hello{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/hello_messages">>, N),
    {ok, State};

handle_wamp_event(#interrupt{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/interrupt_messages">>, N),
    {ok, State};

handle_wamp_event(#invocation{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/invocation_messages">>, N),
    {ok, State};

handle_wamp_event(#publish{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/publish_messages">>, N),
    {ok, State};

handle_wamp_event(#published{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/published_messages">>, N),
    {ok, State};

handle_wamp_event(#register{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/register_messages">>, N),
    {ok, State};

handle_wamp_event(#registered{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/registered_messages">>, N),
    {ok, State};

handle_wamp_event(#result{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/result_messages">>, N),
    {ok, State};

handle_wamp_event(#subscribe{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/subscribe_messages">>, N),
    {ok, State};

handle_wamp_event(#subscribed{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/subscribed_messages">>, N),
    {ok, State};

handle_wamp_event(#unregister{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/unregister_messages">>, N),
    {ok, State};

handle_wamp_event(#unsubscribe{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/unsubscribe_messages">>, N),
    {ok, State};

handle_wamp_event(#unsubscribed{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/unsubscribed_messages">>, N),
    {ok, State};

handle_wamp_event(#welcome{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/welcome_messages">>, N),
    {ok, State};

handle_wamp_event(#yield{}, #{count := N}, Meta, State) ->
    oc_stat:record(Meta, <<"bondy/wamp/yield_messages">>, N),
    {ok, State}.



%% =============================================================================
%% PRIVATE
%% =============================================================================



setup() ->
    ok = bondy_telemetry_http_metrics:setup(),
    ok = setup_oc(),
    %% ok = declare_metrics(),
    %% ok = declare_net_metrics(),
    %% ok = declare_session_metrics(),
    %% ok = declare_wamp_metrics(),

    Collectors = [
        prometheus_vm_memory_collector,
        prometheus_vm_statistics_collector,
        prometheus_vm_system_info_collector,
        oc_stat_exporter_prometheus
    ],
    _ = [prometheus_registry:register_collector(C) || C <- Collectors],
    ok.


setup_oc() ->
    Measures = lists:append([
        oc_net_measures(),
        oc_session_measures(),
        oc_wamp_measures()
    ]),
    _ = [
        oc_stat_measure:new(Name, Description, Unit)
        || {Name, Description, Unit} <- Measures
    ],

    Views = lists:append([
        oc_net_views(),
        oc_session_views(),
        oc_wamp_views()
    ]),
    _ = [oc_stat_view:subscribe(V) || V <- Views],
    ok.


oc_net_measures() ->
    [
        %% Net
        {
            <<"bondy/sockets_opened">>,
            <<"The number of sockets opened.">>,
            none
        },
        {
            <<"bondy/sockets_closed">>,
            <<"The number of sockets closed.">>,
            none
        },
        {
            <<"bondy/socket_errors">>,
            <<"The number of socket errors.">>,
            none
        },
        %% {
        %%     <<"bondy/bytes_received">>,
        %%     <<"The number of bytes received.">>,
        %%     bytes
        %% },
        %% {
        %%     <<"bondy/bytes_sent">>,
        %%     <<"The number of bytes sent.">>,
        %%     bytes
        %% },
        {
            <<"bondy/forwarded_bytes_received">>,
            <<"The number of bytes received from cluster peers.">>,
            bytes
        },
        {
            <<"bondy/forwarded_bytes_sent">>,
            <<"The number of bytes sent to cluster peers.">>,
            bytes
        },
        {
            <<"bondy/forwarded_bytes_dropped">>,
            <<"The number of bytes dropped to/from cluster peers.">>,
            bytes
        },
        {
            <<"bondy/replication_bytes_received">>,
            <<"The number of bytes received from cluster peers.">>,
            bytes
        },
        {
            <<"bondy/replication_bytes_sent">>,
            <<"The number of bytes sent to cluster peers.">>,
            bytes
        },
        {
            <<"bondy/replication_bytes_dropped">>,
            <<"The number of bytes dropped to/from cluster peers.">>,
            bytes
        },
        {
            <<"bondy/aae_bytes_received">>,
            <<"The number of bytes received from cluster peers during AAE exchanges.">>,
            bytes
        },
        {
            <<"bondy/aae_bytes_sent">>,
            <<"The number of bytes sent to cluster peers during AAE exchanges.">>,
            bytes
        }
    ].


%% @private
oc_net_views() ->
    [
        #{
            name => <<"bondy/sockets_opened_total">>,
            measure => "bondy/sockets_opened",
            description => "The total number of sockets opened",
            tags => [protocol, transport, peername],
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/sockets_closed_total">>,
            measure => "bondy/sockets_closed",
            description => "The total number of sockets closed",
            tags => [protocol, transport, peername],
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/socket_errors_total">>,
            measure => <<"bondy/socket_errors">>,
            description => <<"The total number of socket errors">>,
            tags => [protocol, transport, peername],
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => "bondy/bytes_received_total",
            measure => <<"bondy/bytes_received">>,
            description => <<"The total number of bytes received.">>,
            tags => [protocol, transport, peername],
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => "bondy/bytes_sent_total",
            measure => <<"bondy/bytes_sent">>,
            description => <<"The total number of bytes sent.">>,
            tags => [protocol, transport, peername],
            aggregation => oc_stat_aggregation_count

        }
    ].


%% @private
oc_session_measures() ->
    [
        {
            <<"bondy/sessions_opened">>,
            <<"The number of sessions opened.">>,
            none
        },
        {
            <<"bondy/sessions_closed">>,
            <<"The number of sessions closed.">>,
            none
        },
        {
            <<"bondy/session_duration">>,
            <<"The duration of a session in milliseconds.">>,
            millisecond
        },
        {
            <<"bondy/session_requests">>,
            <<"The number of requests received for a session">>,
            none
        },
        {
            <<"bondy/session_bytes_received">>,
            <<"The number of bytes received for a session">>,
            none
        },
        {
            <<"bondy/session_bytes_sent">>,
            <<"The number of bytes sent for a session">>,
            none
        },
        {
            <<"bondy/send_errors">>,
            <<"The number of errors sending a message to a peer.">>,
            none
        }
    ].


%% @private
oc_session_views() ->
    [].


oc_wamp_measures() ->
    [
        {
            <<"bondy/wamp/error_messages">>,
            <<"The number of WAMP ERROR messages.">>,
            none
        },
        {
            <<"bondy/wamp/abort_messages">>,
            <<"The number of WAMP ABORT messages.">>,
            none
        },
        {
            <<"bondy/wamp/authenticate_messages">>,
            <<"The number of WAMP AUTHENTICATE messages.">>,
            none
        },
        {
            <<"bondy/wamp/call_messages">>,
            <<"The number of WAMP CALL messages.">>,
            none
        },
        {
            <<"bondy/wamp/cancel_messages">>,
            <<"The number of WAMP CANCEL messages.">>,
            none
        },
        {
            <<"bondy/wamp/challenge_messages">>,
            <<"The number of WAMP CHALLENGE messages.">>,
            none
        },
        {
            <<"bondy/wamp/event_messages">>,
            <<"The number of WAMP EVENT messages.">>,
            none
        },
        {
            <<"bondy/wamp/goodbye_messages">>,
            <<"The number of WAMP GOODBYE messages.">>,
            none
        },
        {
            <<"bondy/wamp/hello_messages">>,
            <<"The number of WAMP HELLO messages.">>,
            none
        },
        {
            <<"bondy/wamp/interrupt_messages">>,
            <<"The number of WAMP INTERRUPT messages.">>,
            none
        },
        {
            <<"bondy/wamp/invocation_messages">>,
            <<"The number of WAMP INVOCATION messages.">>,
            none
        },
        {
            <<"bondy/wamp/publish_messages">>,
            <<"The number of WAMP PUBLISH messages.">>,
            none
        },
        {
            <<"bondy/wamp/published_messages">>,
            <<"The number of WAMP PUBLISHED messages.">>,
            none
        },
        {
            <<"bondy/wamp/register_messages">>,
            <<"The number of WAMP REGISTER messages.">>,
            none
        },
        {
            <<"bondy/wamp/registered_messages">>,
            <<"The number of WAMP REGISTERED messages.">>,
            none
        },
        {
            <<"bondy/wamp/result_messages">>,
            <<"The number of WAMP UNSUBSCRIBE messages.">>,
            none
        },
        {
            <<"bondy/wamp/subscribe_messages">>,
            <<"The number of WAMP SUBSCRIBE messages.">>,
            none
        },
        {
            <<"bondy/wamp/subscribed_messages">>,
            <<"The number of WAMP SUBSCRIBED messages.">>,
            none
        },
        {
            <<"bondy/wamp/unregister_messages">>,
            <<"The number of WAMP UNSUBSCRIBE messages.">>,
            none
        },
        {
            <<"bondy/wamp/unsubscribe_messages">>,
            <<"The number of WAMP UNSUBSCRIBE messages.">>,
            none
        },
        {
            <<"bondy/wamp/unsubscribed_messages">>,
            <<"The number of WAMP UNSUBSCRIBED messages.">>,
            none
        },
        {
            <<"bondy/wamp/welcome_messages">>,
            <<"The number of WAMP WELCOME messages.">>,
            none
        },
        {
            <<"bondy/wamp/yield_messages">>,
            <<"The number of WAMP YIELD messages.">>,
            none
        },
        {
            <<"bondy/wamp/messages_received">>,
            <<"The number of incoming WAMP messages.">>,
            none
        },
        {
            <<"bondy/wamp/messages_sent">>,
            <<"The number of outgoing WAMP messages.">>,
            none
        },
        {
            <<"bondy/wamp/messages_dropped">>,
            <<"The number of dropped WAMP messages.">>,
            none
        },
        {
            <<"bondy/wamp/forwarded_messages_received">>,
            <<"The number of incoming WAMP messages forwarded by cluster peers.">>,
            none
        },
        {
            <<"bondy/wamp/forwarded_messages_sent">>,
            <<"The number of outgoing WAMP messages forwarded to cluster peers.">>,
            none
        },
        {
            <<"bondy/wamp/message_latency">>,
            <<"The latency in milliseconds of routing a WAMP message.">>,
            millisecond
        },
        {
            <<"bondy/wamp/message_retries">>,
            <<"The number of retries required to route a WAMP message.">>,
            none
        }
    ].


%% @private
oc_wamp_views() ->
    [
        #{
            name => <<"bondy/wamp/error_messages_total">>,
            measure => <<"bondy/wamp/error_messages">>,
            description => <<"The total number of WAMP ERROR messages">>,
            tags => tags(error),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/abort_messages_total">>,
            measure => <<"bondy/wamp/abort_messages">>,
            description => <<"">>,
            tags => tags(abort),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/authenticate_messages_total">>,
            measure => <<"bondy/wamp/authenticate_messages">>,
            description => <<"">>,
            tags => tags(authenticate),
            aggregation => oc_stat_aggregation_count
        },
        #{

            name => <<"bondy/wamp/call_messages_total">>,
            measure => <<"bondy/wamp/call_messages">>,
            description => <<"">>,
            tags => tags(call),
            aggregation => oc_stat_aggregation_count
        },
        #{

            name => <<"bondy/wamp/cancel_messages_total">>,
            measure => <<"bondy/wamp/cancel_messages">>,
            description => <<"">>,
            tags => tags(cancel),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/challenge_messages_total">>,
            measure => <<"bondy/wamp/challenge_messages">>,
            description => <<"">>,
            tags => tags(challenge),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/event_messages_total">>,
            measure => <<"bondy/wamp/event_messages">>,
            description => <<"">>,
            tags => tags(event),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/goodbye_messages_total">>,
            measure => <<"bondy/wamp/goodbye_messages">>,
            description => <<"">>,
            tags => tags(goodbye),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/hello_messages_total">>,
            measure => <<"bondy/wamp/hello_messages">>,
            description => <<"">>,
            tags => tags(hello),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/interrupt_messages_total">>,
            measure => <<"bondy/wamp/interrupt_messages">>,
            description => <<"">>,
            tags => tags(interrupt),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/invocation_messages_total">>,
            measure => <<"bondy/wamp/invocation_messages">>,
            description => <<"">>,
            tags => tags(invocation),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/publish_messages_total">>,
            measure => <<"bondy/wamp/publish_messages">>,
            description => <<"">>,
            tags => tags(publish),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/published_messages_total">>,
            measure => <<"bondy/wamp/published_messages">>,
            description => <<"">>,
            tags => tags(published),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/register_messages_total">>,
            measure => <<"bondy/wamp/register_messages">>,
            description => <<"">>,
            tags => tags(register),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/registered_messages_total">>,
            measure => <<"bondy/wamp/registered_messages">>,
            description => <<"">>,
            tags => tags(registered),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/result_messages_total">>,
            measure => <<"bondy/wamp/result_messages">>,
            description => <<"">>,
            tags => tags(result),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/subscribe_messages_total">>,
            measure => <<"bondy/wamp/subscribe_messages">>,
            description => <<"">>,
            tags => tags(subscribe),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/subscribed_messages_total">>,
            measure => <<"bondy/wamp/subscribed_messages">>,
            description => <<"">>,
            tags => tags(subscribed),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/unregister_messages_total">>,
            measure => <<"bondy/wamp/unregister_messages">>,
            description => <<"">>,
            tags => tags(unregister),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/unsubscribe_messages_total">>,
            measure => <<"bondy/wamp/unsubscribe_messages">>,
            description => <<"">>,
            tags => tags(unsubscribe),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/unsubscribed_messages_total">>,
            measure => <<"bondy/wamp/unsubscribed_messages">>,
            description => <<"">>,
            tags => tags(unsubscribed),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/welcome_messages_total">>,
            measure => <<"bondy/wamp/welcome_messages">>,
            description => <<"">>,
            tags => tags(welcome),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/yield_messages_total">>,
            measure => <<"bondy/wamp/yield_messages">>,
            description => <<"">>,
            tags => tags(yield),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/messages_received_total">>,
            measure => <<"bondy/wamp/messages_received">>,
            description => <<"">>,
            tags => ?WAMP_TAGS,
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/messages_sent_total">>,
            measure => <<"bondy/wamp/messages_sent">>,
            description => <<"">>,
            tags => ?WAMP_TAGS,
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/messages_dropped_total">>,
            measure => <<"bondy/wamp/messages_dropped">>,
            description => <<"">>,
            tags => ?WAMP_TAGS,
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/forwarded_messages_received_total">>,
            measure => <<"bondy/wamp/forwarded_messages_received">>,
            description => <<"">>,
            tags => ?WAMP_TAGS,
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/wamp/forwarded_messages_sent_total">>,
            measure => <<"bondy/wamp/forwarded_messages_sent">>,
            description => <<"">>,
            tags => ?WAMP_TAGS,
            aggregation => oc_stat_aggregation_count
        }
    ].



tags(call) ->
    [procedure_uri | ?WAMP_TAGS];

tags(error) ->
    [error_uri | ?WAMP_TAGS];

tags(publish) ->
    [topic_uri | ?WAMP_TAGS];

tags(register) ->
    [procedure_uri | ?WAMP_TAGS];

tags(subscribe) ->
    [topic_uri | ?WAMP_TAGS];

tags(_) ->
   ?WAMP_TAGS.





%% %% @private
%% get_session_tags(RealmUri, Id, Agent, {_, _} = Peer) ->
%%     get_session_tags(
%%         RealmUri, Id, Agent, inet_utils:peername_to_binary(Peer));


%% get_session_tags(RealmUri, Id, Agent, Peername) ->
%%     #{
%%         realm_uri => RealmUri,
%%         node => bondy_peer_service:mynode(),
%%         session_id => Id,
%%         agent => Agent,
%%         peername => Peername
%%     }.


%% get_tags(Ctxt) ->
%%     {T, FT, E} = bondy_context:subprotocol(Ctxt),
%%     #{
%%         realm => get_realm(Ctxt),
%%         node => bondy_peer_service:mynode(),
%%         session_id => bondy_context:session_id(Ctxt),
%%         agent => bondy_context:agent(Ctxt),
%%         peername => bondy_context:peername(Ctxt),
%%         protocol => wamp,
%%         transport => T,
%%         frame_type => FT,
%%         encoding => E
%%     }.


%% @private
%% get_realm(Ctxt) ->
%%     try
%%         bondy_context:realm_uri(Ctxt)
%%     catch
%%         ?EXCEPTION(_, _, _) ->
%%             undefined
%%     end.


%% observe_message(Metric, M, Tags, Ctxt) ->
%%     Bytes = erts_debug:flat_size(M) * 8,
%%     oc_stat:record(Tags, <<"bondy/wamp/messages">>, 1),

%%     MergedTags = maps:merge(get_tags(Ctxt), Tags),
%%     oc_stat:record(Tags, <<"bondy/wamp/messages">>, 1),
%%     oc_stat:record(Tags, <<"bondy/wamp/messages">>, 1),

%%     ok = prometheus_counter:inc(bondy_wamp_messages_total, Labels),
%%     ok = prometheus_counter:inc(Metric, AllLabels),
%%     prometheus_histogram:observe(bondy_wamp_message_bytes, Labels, Size).




%% bytes_bucket() ->
%%     %% 0 to 8 Mbs
%%     [0, 1024, 1024*4, 1024*16, 1024*32, 1024*64, 1024*128, 1024*256, 1024*512, 1024*1024, 1024*1024*2, 1024*1024*4, 1024*1024*8].


%% declare_metrics() ->
%%     _ = prometheus_counter:declare([
%%         {name, bondy_errors_total},
%%         {help,
%%             <<"The total number of errors in a bondy node since reset.">>},
%%         {labels, [reason | ?WAMP_MESSAGE_LABELS]}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_send_errors_total},
%%         {help,
%%             <<"The total number of router send errors in a bondy node since reset.">>},
%%         {labels, [reason, message_type | ?WAMP_MESSAGE_LABELS]}
%%     ]),
%%     ok.


%% declare_wamp_metrics() ->
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_messages_total},
%%         {help,
%%             <<"The total number of wamp messages routed by a bondy node since reset.">>},
%%         {labels, ?WAMP_MESSAGE_LABELS}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_abort_messages_total},
%%         {help, <<"The total number of abort messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(abort)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_authenticate_messages_total},
%%         {help, <<"The total number of authenticate messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(authenticate)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_call_messages_total},
%%         {help, <<"The total number of call messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(call)}
%%     ]),
%%     _ = prometheus_histogram:declare([
%%         {name, bondy_wamp_call_latency_milliseconds},
%%         {buckets, milliseconds_duration_buckets()},
%%         {help,
%%             <<"A histogram of routed RPC response latencies. This measurement reflects the time between the dealer processing a WAMP call message and the first response (WAMP result or error).">>},
%%         {labels, get_labels(call)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_call_retries_total},
%%         {help, <<"The total number of retries for WAMP call.">>},
%%         {labels, get_labels(call)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_cancel_messages_total},
%%         {help, <<"The total number of cancel messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(cancel)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_challenge_messages_total},
%%         {help, <<"The total number of challenge messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(challenge)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_error_messages_total},
%%         {help, <<"The total number of error messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(error)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_event_messages_total},
%%         {help, <<"The total number of event messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(event)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_goodbye_messages_total},
%%         {help, <<"The total number of goodbye messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(goodbye)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_hello_messages_total},
%%         {help, <<"The total number of hello messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(hello)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_interrupt_messages_total},
%%         {help, <<"The total number of interrupt messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(interrupt)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_invocation_messages_total},
%%         {help, <<"The total number of invocation messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(invocation)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_publish_messages_total},
%%         {help, <<"The total number of publish messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(publish)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_published_messages_total},
%%         {help, <<"The total number of published messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(published)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_register_messages_total},
%%         {help, <<"The total number of register messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(register)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_registered_messages_total},
%%         {help, <<"The total number of registered messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(registered)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_result_messages_total},
%%         {help, <<"The total number of result messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(result)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_subscribe_messages_total},
%%         {help, <<"The total number of subscribe messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(subscribe)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_subscribed_messages_total},
%%         {help, <<"The total number of subscribed messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(subscribed)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_unregister_messages_total},
%%         {help, <<"The total number of unregister messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(unregister)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_unregistered_messages_total},
%%         {help, <<"The total number of unregistered messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(unregistered)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_unsubscribe_messages_total},
%%         {help, <<"The total number of unsubscribe messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(unsubscribe)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_unsubscribed_messages_total},
%%         {help, <<"The total number of unsubscribed messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(unsubscribed)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_welcome_messages_total},
%%         {help, <<"The total number of welcome messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(welcome)}
%%     ]),
%%     _ = prometheus_counter:declare([
%%         {name, bondy_wamp_yield_messages_total},
%%         {help, <<"The total number of yield messages routed by a bondy node since reset.">>},
%%         {labels, get_labels(yield)}
%%     ]),

%%     _ = prometheus_histogram:declare([
%%         {name, bondy_wamp_message_bytes},
%%         {help,
%%             <<"A summary of the size of the wamp messages received by a bondy node">>},
%%         {buckets, bytes_bucket()},
%%         {labels, ?WAMP_MESSAGE_LABELS}
%%      ]),
%%     ok.










%% =============================================================================
%% PRIVATE BONDY COLLECTOR
%% =============================================================================



%% @private
collector_metrics() ->
    lists:append([
        registry_metrics()
    ]).

%% @private
registry_metric(size, Labels, Value) ->
    Help = "The total number of elements in a trie.",
    {registry_trie_elements, gauge, Help, Labels, Value};

registry_metric(nodes, Labels, Value) ->
    Help = "The total number of modes in a trie.",
    {registry_trie_nodes, gauge, Help, Labels, Value};

registry_metric(memory, Labels, Value) ->
    Help = "The total number of modes in a trie.",
    {registry_trie_nodes, gauge, Help, Labels, Value}.


%% @private
registry_metrics() ->
    Info = bondy_registry:info(),
    L = [
        registry_metric(K, [{name, Name}], V)
        || {Name, PL} <- Info,  {K, V} <- PL, K =/= owner
    ],
    [
        {registry_tries, gauge, "Registry tries count.", length(Info)}
        | L
    ].


%% @private
add_metric_family({Name, Type, Help, Metrics}, Callback) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, Type, Metrics)).

%% @private
enabled_metrics() ->
    application:get_env(prometheus, bondy_prometheus_metrics, all).


%% @private
metric_enabled(Name, Metrics) ->
    Metrics =:= all orelse lists:member(Name, Metrics).