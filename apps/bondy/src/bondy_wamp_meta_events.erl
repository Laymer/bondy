%% =============================================================================
%%  bondy_wamp_events - An event handler to turn bondy events into WAMP events.
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
%% @doc An event handler to turn bondy events into WAMP events.
%% @end
%% -----------------------------------------------------------------------------
-module(bondy_wamp_meta_events).
-behaviour(gen_event).
-include("bondy.hrl").
-include("bondy_backup.hrl").
-include("bondy_security.hrl").
-include_lib("wamp/include/wamp.hrl").

-record(state, {}).

%% GEN_EVENT CALLBACKS
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



%% =============================================================================
%% GEN_EVENT CALLBACKS
%% =============================================================================



init([]) ->
    State = #state{},
    {ok, State}.


handle_event(_, State) ->
    {ok, State}.


handle_call(Event, State) ->
    _ = lager:error(
        "Error handling call, reason=unsupported_event, event=~p", [Event]
    ),
    {reply, {error, {unsupported_call, Event}}, State}.


handle_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.