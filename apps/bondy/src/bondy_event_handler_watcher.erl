%% =============================================================================
%%  bondy_promethues_event_handler.erl -
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
%% @doc This module provides a bridge between WAMP events and OTP events.
%% @end
%% -----------------------------------------------------------------------------
-module(bondy_event_handler_watcher).
-behaviour(gen_server).
-include_lib("wamp/include/wamp.hrl").
-include("bondy.hrl").

-export([start/2]).
-export([start/3]).
-export([start_link/2]).
-export([start_link/3]).

%% gen_server callbacks
-export([init/1]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).

-record(state, {
    manager         ::  module(),
    handler         ::  module(),
    args            ::  any()
}).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start(Manager, {swap, Old, New} = Args)
when is_tuple(Old) andalso is_tuple(New) ->
    gen_server:start(?MODULE, [Manager, Args], []).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start(Manager, Handler, Args) ->
    gen_server:start(?MODULE, [Manager, Handler, Args], []).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start_link(Manager, {swap, Old, New} = Args)
when is_tuple(Old) andalso is_tuple(New) ->
    gen_server:start_link(?MODULE, [Manager, Args], []).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start_link(Manager, Handler, Args) when is_atom(Handler) ->
    gen_server:start_link(?MODULE, [Manager, Handler, Args], []).



%% =============================================================================
%% GEN_SERVER CALLBACKS
%% =============================================================================




init([Manager, {swap, Old, {Handler, Args} = New}]) ->
    ok = gen_event:swap_sup_handler(alarm_handler, Old, New),
    {ok, #state{manager = Manager, handler = Handler, args = Args}};

init([Manager, Handler, Args]) ->
    ok = add_sup_handler(Manager, Handler, Args),
    {ok, #state{manager = Manager, handler = Handler, args = Args}}.


handle_call(_, _, State) ->
    {reply, ok, State}.


handle_cast(_Event, State) ->
    {noreply, State}.


handle_info(add_sup_handler, State) ->
    Manager = State#state.manager,
    Handler = State#state.handler,
    Args = State#state.args,
    ok = add_sup_handler(Manager, Handler, Args),
    {noreply, State};

handle_info(
    {gen_event_EXIT, Handler, normal}, #state{handler = Handler} = State) ->
    {stop, normal, State};

handle_info(
    {gen_event_EXIT, Handler, shutdown}, #state{handler = Handler} = State) ->
    {stop, normal, State};

handle_info(
    {gen_event_EXIT, Handler, Reason}, #state{handler = Handler} = State) ->
    _ = lager:error(
        "Event handler exited, re-installing; reason=~p, handler=~p",
        [Reason, Handler]
    ),
    ok = add_sup_handler(State#state.manager, Handler, State#state.args),
    {noreply, State};

handle_info(Info, State) ->
    _ = lager:debug("Unexpected message, message=~p", [Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
add_sup_handler(Manager, Handler, Args) ->
    case bondy_event_manager:add_sup_handler(Manager, Handler, Args) of
        ok ->
            ok;
        {error, {fatal, Reason}} ->
            _ = lager:error(
                "Fatally failed to install event handler, not retrying; reason=~p, manager=~p, handler=~p",
                [Reason, Manager, Handler]
            ),
            self() ! stop,
            ok;
        {error, Reason} ->
            _ = lager:error(
                "Failed to install event handler, retrying later; "
                "reason=~p, manager=~p, handler=~p",
                [Reason, Manager, Handler]
            ),
            erlang:send_after(5000, self(), add_sup_handler),
            ok
    end.

