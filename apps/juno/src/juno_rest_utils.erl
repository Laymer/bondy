-module(juno_rest_utils).

-type state_fun() :: fun((Client :: pbs_client:pbs_client() | undefined) -> NewState :: any()).
-export_type([state_fun/0]).


-export([is_authorized/2]).
-export([location_uri/2]).
-export([set_resp_error_body/2]).
-export([bindings_to_map/2]).
-export([set_resp_link_header/4]).
-export([set_resp_link_header/2]).

%% The StateFun should receive 
-spec is_authorized(Req :: cowboy_req:req(), StateFun :: state_fun()) ->
    stop | true | {false, WWWAuthHeader :: binary()}.
is_authorized(Req0, StateFun) ->
    % We only allow the client to issue requests for a known tenant key
    TenantKey = cowboy_req:header(<<"authorization">>, Req0),
    try
        TenantKey /= undefined orelse throw({unknown_tenant_key, TenantKey}),
        C = pbs_client:new(TenantKey),
        {true, Req0, StateFun(C)}
    catch
        throw:Reason = {unknown_tenant_key, TenantKey} ->
            % We set a useless WWWAuthHeader
            Req1 = set_resp_error_body(Reason, Req0),
            {{false, <<"unknown_tenant_key">>}, Req1, StateFun(undefined)};
        _:Reason ->
            % We force a JSON error object as body
            Req1 = set_resp_error_body(Reason, Req0),
            Req2 = cowboy_req:reply(500, Req1),
            {stop, Req2, StateFun(undefined)}
    end.

set_resp_error_body(Reason, Req) ->
    Body = pbs_json_utils:error(Reason),
    %% cowboy_req:reply(500, [?CT_JSON], Body, Req).
    cowboy_req:set_resp_body(Body, Req).


-spec location_uri(ID :: binary(), Req :: cowboy_req:req()) ->
    URI :: binary().
location_uri(ID, Req) ->
    Path = cowboy_req:path(Req),
    <<Path/binary, "/", ID/binary>>.


-spec bindings_to_map(Bindings :: list(), Map :: map()) ->
    NewMap :: map().
bindings_to_map(Bindings, Map) when is_list(Bindings), is_map(Map) ->
    bindings_to_map(Bindings, Map, #{}).

%% @private
bindings_to_map([], _, Acc) ->
    Acc;
bindings_to_map([{K, V} | T], Map, Acc0) ->
    Field = maps:get(K, Map),
    Acc1 = maps:put(Field, V, Acc0),
    bindings_to_map(T, Map, Acc1).


-spec set_resp_link_header(
        [{binary(), iodata(), iodata()}], Req :: cowboy_req:req()) ->
    NewReq :: cowboy_req:req().
set_resp_link_header([Link], Req) ->
    set_resp_link_header(resp_link_value(Link), Req);
set_resp_link_header(L, Req) ->
    Value = resp_link_values(L, []),
    cowboy_req:set_resp_header(<<"link">>, Value, Req).



-spec set_resp_link_header(
        binary(), iodata(), iodata(), Req :: cowboy_req:req()) ->
    NewReq :: cowboy_req:req().
set_resp_link_header(URI, Rel, Title, Req) ->
    Value = resp_link_value({URI, Rel, Title}),
    cowboy_req:set_resp_header(<<"link">>, Value, Req).


resp_link_values([], Acc) ->
    Acc;
resp_link_values([H | T], Acc0) ->
    Acc1 = [resp_link_value(H), $, | Acc0],
    resp_link_values(T, Acc1).


resp_link_value({URI, Rel, Title}) ->
    [
        $<, URI, $>, $;, $\s,
        "rel=", $", Rel, $", $;, $\s,
        "title=", $", Title, $"
    ].



