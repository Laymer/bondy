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

-export([init/0]).
-export([otp_release/0]).
-export([sys_driver_version/0]).
-export([sys_monitor_count/0]).
-export([system_architecture/0]).
-export([system_version/0]).
-export([update/1]).


%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init() -> ok.

init() ->
    % create_metrics(system_specs()),
    %% create_metrics(bc_specs()),
    %% create_metrics(static_specs()).
    bondy_cowboy_prometheus:setup(),
    bondy_prometheus:init().


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

update(Event) ->
    bondy_prometheus:update(Event).



