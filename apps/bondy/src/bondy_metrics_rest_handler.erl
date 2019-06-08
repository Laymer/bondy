%% @doc
%% Cowboy2 handler for exporting prometheus metrics.
%% @end

%% From https://github.com/deadtrickster/prometheus-cowboy/blob/master/src/prometheus_cowboy2_handler.erl
-module(bondy_metrics_rest_handler).

%% -behaviour(cowboy_handler).

-export([
         init/2,
         terminate/3
        ]).

%% ===================================================================
%% cowboy_handler callbacks
%% ===================================================================

init(Req, _Opts) ->
  handle(Req).

terminate(_Reason, _Req, _State) ->
  ok.

%% ===================================================================
%% Private functions
%% ===================================================================

handle(Request) ->
  Method = cowboy_req:method(Request),
  Request1 = gen_response(Method, Request),
  {ok, Request1, undefined}.

gen_response(<<"GET">>, Request) ->
  Registry0 = cowboy_req:binding(registry, Request, <<"default">>),
  case prometheus_registry:exists(Registry0) of
    false ->
      cowboy_req:reply(404, #{}, <<"Unknown Registry">>, Request);
    Registry ->
      gen_metrics_response(Registry, Request)
  end;
gen_response(_, Request) ->
  Request.

gen_metrics_response(Registry, Request) ->
  URI = true,
  GetHeader = fun(Name, Default) ->
                  cowboy_req:header(iolist_to_binary(Name),
                                    Request, Default)
              end,
  {Code, RespHeaders, Body} = prometheus_http_impl:reply(
                                #{path => URI,
                                  headers => GetHeader,
                                  registry => Registry,
                                  standalone => false}),

  Headers = to_cowboy_headers(RespHeaders),
  cowboy_req:reply(Code, maps:from_list(Headers), Body, Request).






%% =============================================================================
%%  From https://github.com/deadtrickster/prometheus-cowboy/blob/master/src/prometheus_cowboy.erl
%% =============================================================================



to_cowboy_headers(RespHeaders) ->
  lists:map(fun to_cowboy_headers_/1, RespHeaders).

%% ===================================================================
%% Private functions
%% ===================================================================

to_cowboy_headers_({Name, Value}) ->
  {to_cowboy_name(Name), Value}.

to_cowboy_name(Name) ->
  binary:replace(atom_to_binary(Name, utf8), <<"_">>, <<"-">>).