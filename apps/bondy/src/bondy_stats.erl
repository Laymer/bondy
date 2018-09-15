%% =============================================================================
%%  bondy_stats.erl -
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

-module(bondy_stats).
-behaviour(gen_server).

-record(state, {}).


-export([otp_release/0]).
-export([sys_driver_version/0]).
-export([sys_monitor_count/0]).
-export([system_architecture/0]).
-export([system_version/0]).
-export([update/1]).
-export([start_link/0]).

%% GEN_SERVER CALLBACKS
-export([init/1]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).


%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% Borrowed from https://github.com/basho/riak_kv/src/riak_kv_stat_bc.erl
%% -----------------------------------------------------------------------------
otp_release() ->
    list_to_binary(erlang:system_info(otp_release)).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% Borrowed from https://github.com/basho/riak_kv/src/riak_kv_stat_bc.erl
%% -----------------------------------------------------------------------------
sys_driver_version() ->
    list_to_binary(erlang:system_info(driver_version)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% Borrowed from https://github.com/basho/riak_kv/src/riak_kv_stat_bc.erl
%% -----------------------------------------------------------------------------
system_version() ->
    list_to_binary(
        string:strip(erlang:system_info(system_version), right, $\n)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% Borrowed from https://github.com/basho/riak_kv/src/riak_kv_stat_bc.erl
%% -----------------------------------------------------------------------------
system_architecture() ->
    list_to_binary(erlang:system_info(system_architecture)).


%% -----------------------------------------------------------------------------
%% @doc
%% Count up all monitors, unfortunately has to obtain process_info
%% from all processes to work it out.
%% @end
%% Borrowed from https://github.com/basho/riak_kv/src/riak_kv_stat_bc.erl
%% -----------------------------------------------------------------------------
sys_monitor_count() ->
    lists:foldl(
        fun(Pid, Count) ->
            case erlang:process_info(Pid, monitors) of
                {monitors, Mons} ->
                    Count + length(Mons);
                _ ->
                    Count
            end
        end, 0, processes()).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec update(tuple()) -> ok.

update(Event) when is_tuple(Event) ->
    gen_server:cast(?MODULE, {event, Event}).





%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================



init([]) ->
    bondy_cowboy_prometheus:setup(),
    bondy_prometheus:init(),
    {ok, #state{}}.


handle_call(Event, From, State) ->
    _ = lager:error(
        "Error handling call, reason=unsupported_event, event=~p, from=~p", [Event, From]),
    {noreply, State}.


handle_cast({event, Event}, State) ->
    ok = bondy_prometheus:update(Event),
    {noreply, State};

handle_cast(Event, State) ->
    _ = lager:error(
        "Error handling call, reason=unsupported_event, event=~p", [Event]),
    {noreply, State}.


handle_info(Info, State) ->
    _ = lager:error("Unexpected message, message=~p", [Info]),
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.