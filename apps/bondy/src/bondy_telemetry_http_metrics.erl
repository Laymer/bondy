%% @doc
%% Collects Cowboy metrics using
%% <a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
%%   metrics stream handler
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `cowboy_early_errors_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[]', configured via `early_errors_tags'.<br/>
%%     Total number of Cowboy early errors, i.e. errors that occur before a request is received.
%%   </li>
%%   <li>
%%     `cowboy_protocol_upgrades_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[]', configured via `protocol_upgrades_tags'.<br/>
%%     Total number of protocol upgrades, i.e. when http connection
%%     upgraded to websocket connection.
%%   </li>
%%   <li>
%%     `cowboy_requests_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Total number of Cowboy requests.
%%   </li>
%%   <li>
%%     `cowboy_spawned_processes_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Total number of spawned processes.
%%   </li>
%%   <li>
%%     `cowboy_errors_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[method, reason, error]', configured via `error_tags'.<br/>
%%     Total number of Cowboy request errors.
%%   </li>
%%   <li>
%%     `cowboy_request_duration_seconds'<br/>
%%     Type: histogram.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]',
%%     configured via `duration_buckets'.<br/>
%%     Cowboy request duration.
%%   </li>
%%   <li>
%%     `cowboy_receive_body_duration_seconds'<br/>
%%     Type: histogram.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]',
%%     configured via `duration_buckets'.<br/>
%%     Request body receiving duration.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Prometheus Cowboy2 instrumenter configured via `cowboy_instrumenter' key of `prometheus'
%% app environment.
%%
%% Default configuration:
%%
%% <pre lang="erlang">
%% {prometheus, [
%%   ...
%%   {cowboy_instrumenter, [{duration_buckets, [0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]},
%%                          {early_error_tags,  []},
%%                          {request_tags, [method, reason, status_class]},
%%                          {error_tags, [method, reason, error]},
%%                          {registry, default}]}
%%   ...
%% ]}
%% </pre>
%%
%% ==Tags==
%%
%% Builtin:
%%  - host,
%%  - port,
%%  - method,
%%  - status,
%%  - status_class,
%%  - reason,
%%  - error.
%%
%% ===Custom tags===
%% can be implemented via module exporting meta_value/2 function.
%% First argument will be tag name, second is Metrics data from
%% <a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
%% metrics stream handler
%% </a>.
%% Set this module to `tags_module' configuration option.
%%
%% @end
-module(bondy_telemetry_http_metrics).

-export([setup/0]).
-export([observe/1]).



%% ===================================================================
%% API
%% ===================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% <a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
%% Metrics stream handler
%% </a> callback.
%% @end
%% -----------------------------------------------------------------------------
-spec observe(map()) -> ok.

observe(#{ref := ListenerRef} = Metrics0) ->
    {Host, Port} = ranch:get_addr(ListenerRef),
    Metrics1 = Metrics0#{listener_host => Host, listener_port => Port},
    report_metrics(Metrics1),
    ok.


%% -----------------------------------------------------------------------------
%% @doc Sets all metrics up.
%% @end
%% -----------------------------------------------------------------------------
setup() ->
    _ = [
        oc_stat_measure:new(Name, Description, Unit)
        || {Name, Description, Unit} <- measures()
    ],
    _ = [oc_stat_view:subscribe(V) || V <- views()],
    ok.



%% =============================================================================
%% PRIVATE
%% =============================================================================


%% @private
measures() ->
    [
        {
            <<"bondy/http/early_errors">>,
            <<"Counts Cowboy early errors.">>,
            none
        },
        {
            <<"bondy/http/protocol_upgrades">>,
            <<"Counts protocol upgrades.">>,
            none
        },
        {
            <<"bondy/http/request_duration">>,
            <<"http request duration.">>,
            millisecond
        },
        {
            <<"bondy/http/spawned_processes">>,
            <<"Counts spawned processes.">>,
            none
        },
        {
            <<"bondy/http/errors">>,
            <<"Counts request errors.">>,
            none
        },
        {
            <<"bondy/http/receive_body_duration">>,
            <<"Time needed to receive full body.">>,
            millisecond
        }
    ].


views() ->
    [
        #{
            name => <<"bondy/http/early_errors_total">>,
            measure => <<"bondy/http/early_errors">>,
            description => <<"The total number of HTTP errors.">>,
            tags => tags(),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/http/protocol_upgrades_total">>,
            measure => <<"bondy/http/protocol_upgrades">>,
            description => <<"The total number of HTTP protocol upgrades.">>,
            tags => tags(),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/http/request_duration_seconds">>,
            measure => <<"bondy/http/request_duration">>,
            description => <<"HTTP request duration histogram.">>,
            tags => tags(),
            unit => second,
            aggregation => bondy_telemetry:latency_distribution()
        },
        #{
            name => <<"bondy/http/spawned_processes_total">>,
            measure => <<"bondy/http/spawned_processes">>,
            description => <<"The total number of spawned processes.">>,
            tags => tags(),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/http/errors_total">>,
            measure => <<"bondy/http/errors">>,
            description => <<"The total number of request errors.">>,
            tags => tags(),
            aggregation => oc_stat_aggregation_count
        },
        #{
            name => <<"bondy/http/receive_body_duration">>,
            measure => <<"bondy/http/receive_body_duration">>,
            description => <<"Histogram of time needed to receive full body.">>,
            tags => tags(),
            unit => native_time_unit,
            aggregation => bondy_telemetry:latency_distribution()
        }
    ].


report_metrics(#{early_time_error := _} = Metrics) ->
    oc_stat:record(meta(Metrics), <<"bondy/http/early_errors">>, 1);

report_metrics(#{reason := switch_protocol} = Metrics) ->
  oc_stat:record(meta(Metrics), <<"bondy/http/protocol_upgrades">>, 1);

report_metrics(Metrics) ->
    #{
        req_start := ReqStart,
        req_end := ReqEnd,
        req_body_start := ReqBodyStart,
        req_body_end := ReqBodyEnd,
        reason := Reason,
        procs := Procs
    } = Metrics,
    Meta = meta(Metrics),

    Duration = erlang:convert_time_unit(ReqEnd - ReqStart, native, millisecond),
    oc_stat:record(Meta, <<"bondy/http/spawned_processes">>, maps:size(Procs)),
    oc_stat:record(Meta, <<"bondy/http/request_duration">>, Duration),

    case ReqBodyEnd of
        undefined ->
            ok;
        _ ->
            Duration = ReqBodyEnd - ReqBodyStart,
            oc_stat:record(
                Meta, <<"bondy/http/receive_body_duration">>, Duration)
    end,

    case Reason of
        normal ->
            ok;
        switch_protocol ->
            ok;
        stop ->
            ok;
        _ ->
            oc_stat:record(meta(Metrics), <<"bondy/http/errors">>, 1)
    end.



tags() ->
    [
        host, port, method, status, status_class, reason, error
    ].


meta(Metrics) ->
    Tags = #{
        host => meta_host(Metrics),
        port => meta_port(Metrics),
        method => meta_method(Metrics),
        status => meta_status(Metrics),
        status_class => meta_status_class(Metrics),
        reason => meta_reason(Metrics),
        error => meta_error(Metrics)
    },
    maps:merge(ocp:current_tags(), Tags).


meta_host(#{listener_host := Host}) -> Host.


meta_port(#{listener_port := Port}) -> Port.


meta_method(#{req := Req}) -> cowboy_req:method(Req).


meta_status(#{resp_status := Status}) -> Status.


meta_status_class(#{resp_status := undefined}) ->
    undefined;

meta_status_class(#{resp_status := Status}) ->
    status_class(Status).


meta_reason(#{reason:=Reason}) ->
    case Reason of
        _ when is_atom(Reason) -> Reason;
        {ReasonAtom, _} -> ReasonAtom;
        {ReasonAtom, _, _} -> ReasonAtom
    end.

meta_error(#{reason:=Reason}) ->
    case Reason of
        _ when is_atom(Reason) -> undefined;
        {_, {Error, _}, _} -> Error;
        {_, Error, _} when is_atom(Error) -> Error;
        _ -> undefined
    end.


%% -----------------------------------------------------------------------------
%% @doc
%% Returns status class for the http status code `SCode'.
%%
%% Raises `{invalid_value_error, SCode, Message}' error if `SCode'
%% isn't a positive integer.
%% @end
%% -----------------------------------------------------------------------------
status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode < 100 ->
    "unknown";

status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode < 200 ->
    "informational";

status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode < 300 ->
    "success";

status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode < 400 ->
    "redirection";

status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode < 500 ->
    "client-error";

status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode < 600 ->
    "server-error";

status_class(SCode)
when is_integer(SCode) andalso SCode > 0 andalso SCode >= 600 ->
    "unknown";

status_class(C) ->
    erlang:error({invalid_value, C, "status code must be a positive integer"}).