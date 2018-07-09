%% =============================================================================
%%  bondy_wamp_raw_handler.erl -
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



%% =============================================================================
%% @doc
%% A ranch handler for the wamp protocol over either tcp or tls transports.
%% @end
%% =============================================================================
-module(bondy_wamp_raw_handler).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-include("bondy.hrl").
-include_lib("wamp/include/wamp.hrl").


-define(TCP, wamp_tcp).
-define(TLS, wamp_tls).
-define(TIMEOUT, ?PING_TIMEOUT * 2).
-define(PING_TIMEOUT, 10000). % 10 secs
-define(RAW_MAGIC, 16#7F).
-define(RAW_MSG_PREFIX, <<0:5, 0:3>>).
-define(RAW_PING_PREFIX, <<0:5, 1:3>>).
-define(RAW_PONG_PREFIX, <<0:5, 2:3>>).

%% 0: illegal (must not be used)
%% 1: serializer unsupported
%% 2: maximum message length unacceptable
%% 3: use of reserved bits (unsupported feature)
%% 4: maximum connection count reached
%% 5 - 15: reserved for future errors
-define(RAW_ERROR(Upper), <<?RAW_MAGIC:8, Upper:4, 0:20>>).
-define(RAW_FRAME(Bin), <<(?RAW_MSG_PREFIX)/binary, (byte_size(Bin)):24, Bin/binary>>).

-record(state, {
    socket                  ::  gen_tcp:socket(),
    transport               ::  module(),
    frame_type              ::  frame_type(),
    encoding                ::  atom(),
    max_len                 ::  pos_integer(),
    ping_sent = false       ::  {true, binary(), reference()} | false,
    ping_attempts = 0       ::  non_neg_integer(),
    ping_max_attempts = 2   ::  non_neg_integer(),
    hibernate = false       ::  boolean(),
    start_time              ::  integer(),
    protocol_state          ::  bondy_wamp_protocol:state() | undefined,
    buffer = <<>>           ::  binary()
}).
-type state() :: #state{}.

-type raw_error()           ::  invalid_response
                                | invalid_socket
                                | invalid_handshake
                                | maximum_connection_count_reached
                                | maximum_message_length_unacceptable
                                | maximum_message_length_exceeded
                                | serializer_unsupported
                                | use_of_reserved_bits.


-export([start_link/4]).
-export([start_listeners/0]).
-export([suspend_listeners/0]).
-export([resume_listeners/0]).
-export([stop_listeners/0]).
-export([connections/0]).
-export([tcp_connections/0]).
-export([tls_connections/0]).


-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).




%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% Starts the tcp and tls raw socket listeners
%% @end
%% -----------------------------------------------------------------------------
-spec start_listeners() -> ok.

start_listeners() ->
    start_listeners([?TCP, ?TLS]).



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec stop_listeners() -> ok.

stop_listeners() ->
    catch ranch:stop_listener(?TCP),
    catch ranch:stop_listener(?TLS),
    ok.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec suspend_listeners() -> ok.

suspend_listeners() ->
    catch ranch:suspend_listener(?TCP),
    catch ranch:suspend_listener(?TLS),
    ok.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec resume_listeners() -> ok.

resume_listeners() ->
    catch ranch:resume_listener(?TCP),
    catch ranch:resume_listener(?TLS),
    ok.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
connections() ->
    tls_connections() ++ tcp_connections().

tls_connections() ->
    ranch:procs(?TLS, connections).


tcp_connections() ->
    ranch:procs(?TCP, connections).



%% =============================================================================
%% GEN SERVER CALLBACKS
%% =============================================================================



init({Ref, Socket, Transport, _Opts}) ->
    %% We must call ranch:accept_ack/1 before doing any socket operation.
    %% This will ensure the connection process is the owner of the socket.
    %% It expects the listener’s name as argument.
    ok = ranch:accept_ack(Ref),
    Transport:setopts(Socket, [{active, once}, {packet, 0}]),
    St = #state{
        start_time = erlang:monotonic_time(second),
        socket = Socket,
        transport = Transport
    },
    ok = socket_opened(St),
    gen_server:enter_loop(?MODULE, [], St, ?TIMEOUT).



handle_info(
    {tcp, Socket, <<?RAW_MAGIC:8, MaxLen:4, Encoding:4, _:16>>},
    #state{socket = Socket, protocol_state = undefined} = St0) ->
    case handle_handshake(MaxLen, Encoding, St0) of
        {ok, St1} ->
            (St1#state.transport):setopts(Socket, [{active, once}]),
            {noreply, St1, ?TIMEOUT};
        {stop, Reason, St1} ->
            ok = close_socket(Reason, St1),
            {stop, Reason, St1}
    end;

handle_info(
    {tcp, Socket, Data},
    #state{socket = Socket, protocol_state = undefined} = St) ->
    %% RFC: After a _Client_ has connected to a _Router_, the _Router_ will
    %% first receive the 4 octets handshake request from the _Client_.
    %% If the _first octet_ differs from "0x7F", it is not a WAMP-over-
    %% RawSocket request. Unless the _Router_ also supports other
    %% transports on the connecting port (such as WebSocket), the
    %% _Router_ MUST *fail the connection*.
    _ = lager:error(
        "Received data before WAMP protocol handshake, reason=invalid_handshake, data=~p",
        [Data]
    ),
    {stop, invalid_handshake, St};

handle_info({tcp, Socket, Data}, #state{socket = Socket} = St0) ->
    %% We append the newly received data to any existing buffer
    Buffer = St0#state.buffer,
    St1 = St0#state{buffer = <<>>},
    case handle_data(<<Buffer/binary, Data/binary>>, St1) of
        {ok, St2} ->
            (St2#state.transport):setopts(Socket, [{active, once}]),
            {noreply, St2, ?TIMEOUT};
        {stop, Reason, St2} ->
            ok = close_socket(Reason, St2),
            {stop, Reason, St2}
    end;

handle_info({?BONDY_PEER_REQUEST, Pid, M}, St) when Pid =:= self() ->
    %% Here we receive a message from the bondy_rotuer in those cases
    %% in which the router is embodied by our process i.e. the sync part
    %% of a routing process e.g. wamp calls, so we do not ack
    %% TODO check if we still need this now that bondy:ack seems to handle this case
    handle_outbound(M, St);

handle_info({?BONDY_PEER_REQUEST, Pid, Ref, M}, St) ->
    %% Here we receive the messages that either the router or another peer
    %% have sent to us using bondy:send/2,3 which requires us to ack
    ok = bondy:ack(Pid, Ref),
    %% We send the message to the peer
    handle_outbound(M, St);

handle_info({tcp_closed, Socket}, State) ->
    _ = log(
        info, "Connection closed by peer, socket='~p'", [Socket], State),
    ok = socket_closed(false, State),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, State) ->
    _ = log(
        error,
        "Connection closing, error=tcp_error, reason=~p, socket='~p'",
        [Reason, Socket],
        State
    ),
    ok = socket_closed(true, State),
    {stop, Reason, State};

handle_info(timeout, #state{ping_sent = false} = State0) ->
    _ = log(debug, "Timeout. Sending first ping", [], State0),
    {ok, State1} = send_ping(State0),
    %% Here we do not return a timeout value as send_ping set an ah-hoc timet
    {noreply, State1};

handle_info(
    ping_timeout,
    #state{ping_sent = Val, ping_attempts = N, ping_max_attempts = N} = State) when Val =/= false ->
    _ = log(
        error, "Raw TCP connection closing, reason=ping_timeout, attempts=~p", [N],
        State
    ),
    ok = close_socket(ping_timeout, State),
    {stop, timeout, State#state{ping_sent = false}};

handle_info(ping_timeout, #state{ping_sent = {_, Bin, _}} = State) ->
    %% We try again until we reach ping_max_attempts
    _ = log(debug, "Ping timeout. Sending second ping", [], State),
    %% We reuse the same payload, in case the client responds the previous one
    {ok, State1} = send_ping(Bin, State),
    %% Here we do not return a timeout value as send_ping set an ah-hoc timer
    {noreply, State1};

handle_info({stop, Reason}, State) ->
    Peer = bondy_wamp_protocol:peer(State#state.protocol_state),
    SessionId = bondy_wamp_protocol:session_id(State#state.protocol_state),
    _ = lager:debug(<<"Raw TCP WAMP session shutdown, reason=~p, peer=~p, session_id=~p">>, [Reason, Peer, SessionId]),
    {stop, normal, State};

handle_info(Info, State) ->
    _ = lager:error("Received unknown info, message='~p'", [Info]),
    {noreply, State}.


handle_call(Msg, From, State) ->
    _ = lager:info("Received unknown call, message=~p, from=~p", [Msg, From]),
    {noreply, State, ?TIMEOUT}.


handle_cast(Msg, State) ->
    _ = lager:info("Received unknown cast, message=~p", [Msg]),
    {noreply, State, ?TIMEOUT}.


terminate(Reason, St) ->
    _ = lager:info("Terminating, reason=~p", [Reason]),
    bondy_wamp_protocol:terminate(St#state.protocol_state).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
start_listeners([]) ->
    ok;

start_listeners([H|T]) ->
    {ok, Opts} = application:get_env(bondy, H),
    case lists:keyfind(enabled, 1, Opts) of
        {_, true} ->
            {_, PoolSize} = lists:keyfind(acceptors_pool_size, 1, Opts),
            {_, Port} = lists:keyfind(port, 1, Opts),
            {_, MaxConns} = lists:keyfind(max_connections, 1, Opts),
            TransportOpts = case H == ?TLS of
                true ->
                    {_, Files} = lists:keyfind(pki_files, 1, Opts),
                    Files;
                false ->
                    []
            end,
            {ok, _} = ranch:start_listener(
                H,
                PoolSize,
                ranch_mod(H),
                [{port, Port}],
                bondy_wamp_raw_handler,
                TransportOpts
            ),
            _ = ranch:set_max_connections(H, MaxConns),
            start_listeners(T);
        {_, false} ->
            start_listeners(T)
    end.


%% @private
ranch_mod(?TCP) -> ranch_tcp;
ranch_mod(?TLS) -> ranch_ssl.


%% @private
-spec handle_data(Data :: binary(), State :: state()) ->
    {ok, state()} | {stop, raw_error(), state()}.

handle_data(<<0:5, _:3, Len:24, _Data/binary>>, #state{max_len = MaxLen} = St)
when Len > MaxLen ->
    %% RFC: During the connection, Router MUST NOT send messages to the Client
    %% longer than the LENGTH requested by the Client, and the Client MUST NOT
    %% send messages larger than the maximum requested by the Router in it's
    %% handshake reply.
    %% If a message received during a connection exceeds the limit requested,
    %% a Peer MUST fail the connection.
    _ = log(
        error,
        "Client committed a WAMP protocol violation, "
        "reason=maximum_message_length_exceeded, message_length=~p",
        [Len],
        St
    ),
    ok = close_socket(maximum_message_length_exceeded, St),
    {stop, maximum_message_length_exceeded, St};

handle_data(<<0:5, 0:3, Len:24, Data/binary>>, St)
when byte_size(Data) >= Len ->
    %% We received a WAMP message
    %% Len is the number of octets after serialization
    <<Mssg:Len/binary, Rest/binary>> = Data,
    case bondy_wamp_protocol:handle_inbound(Mssg, St#state.protocol_state) of
        {ok, PSt} ->
            handle_data(Rest, St#state{protocol_state = PSt});
        {reply, L, PSt} ->
            St1 = St#state{protocol_state = PSt},
            ok = send(L, St1),
            handle_data(Rest, St1);
        {stop, PSt} ->
            {stop, normal, St#state{protocol_state = PSt}};
        {stop, L, PSt} ->
            St1 = St#state{protocol_state = PSt},
            ok = send(L, St1),
            {stop, normal, St1};
        {stop, Reason, L, PSt} ->
            St1 = St#state{protocol_state = PSt},
            ok = send(L, St1),
            {stop, Reason, St1}
    end;

handle_data(<<0:5, 1:3, Len:24, Data/binary>>, St) ->
    %% We received a PING, send a PONG
    <<Payload:Len/binary, Rest/binary>> = Data,
    ok = send_frame(<<0:5, 2:3, Len:24, Payload/binary>>, St),
    handle_data(Rest, St);

handle_data(<<0:5, 2:3, Len:24, Data/binary>>, St) ->
    %% We received a PONG
    _ = log(debug, "Received pong", [], St),
    <<Payload:Len/binary, Rest/binary>> = Data,
    case St#state.ping_sent of
        {true, Payload, TimerRef} ->
            %% We reset the state
            ok = erlang:cancel_timer(TimerRef, [{info, false}]),
            handle_data(Rest, St#state{ping_sent = false, ping_attempts = 0});
        {true, Bin, TimerRef} ->
            ok = erlang:cancel_timer(TimerRef, [{info, false}]),
            _ = log(
                error,
                "Invalid pong message from peer, reason=invalid_payload, received=~p, expected=~p",
                [Bin, Payload], St
            ),
            ok = close_socket(invalid_ping_response, St),
            {stop, invalid_ping_response, St};
        false ->
            _ = log(error, "Unrequested pong message from peer", [], St),
            %% Should we stop instead?
            handle_data(Rest, St)
    end;

handle_data(<<0:5, R:3, Len:24, Data/binary>>, St) when R > 2 ->
    %% The three bits (R) encode the type of the transport message,
    %% values 3 to 7 are reserved
    <<Mssg:Len, Rest/binary>> = Data,
    ok = send_frame(error_number(use_of_reserved_bits), St),
    _ = log(
        error,
        "Client committed a WAMP protocol violation and the message was dropped, reason=~p, value=~p, message=~p",
        [use_of_reserved_bits, R, Mssg],
        St
    ),
    %% Should we stop instead?
    handle_data(Rest, St);

handle_data(<<>>, St) ->
    %% We finished consuming data
    {ok, St};

handle_data(Data, St) ->
    %% We have a partial message i.e. byte_size(Data) < Len
    %% we store is as buffer
    {ok, St#state{buffer = Data}}.




-spec handle_outbound(any(), state()) ->
    {noreply, state(), timeout()}
    | {stop, normal, state()}.

handle_outbound(M, St0) ->
    case bondy_wamp_protocol:handle_outbound(M, St0#state.protocol_state) of
        {ok, Bin, PSt} ->
            St1 = St0#state{protocol_state = PSt},
            ok = send(Bin, St1),
            {noreply, St1, ?TIMEOUT};
        {stop, PSt} ->
            {stop, normal, St0#state{protocol_state = PSt}};
        {stop, Bin, PSt} ->
            ok = send(Bin, St0#state{protocol_state = PSt}),
            {stop, normal, St0#state{protocol_state = PSt}};
        {stop, Bin, PSt, Time} when is_integer(Time), Time > 0 ->
            erlang:send_after(
                Time, self(), {stop, <<"Router dropped session.">>}),
            ok = send(Bin, St0#state{protocol_state = PSt}),
            {noreply, St0#state{protocol_state = PSt}}
    end.


%% =============================================================================
%% PRIVATE
%% =============================================================================


%% @private
handle_handshake(Len, Enc, St) ->
    try
        init_wamp(Len, Enc, St)
    catch
        throw:Reason ->
            ok = send_frame(error_number(Reason), St),
            _ = lager:error("WAMP protocol error, reason=~p", [Reason]),
            {stop, Reason, St}
    end.


%% @private
init_wamp(Len, Enc, St0) ->
    MaxLen = validate_max_len(Len),
    {FrameType, EncName} = validate_encoding(Enc),

    case inet:peername(St0#state.socket) of
        {ok, {_, _} = Peer} ->
            Proto = {raw, FrameType, EncName},
            case bondy_wamp_protocol:init(Proto, Peer, #{}) of
                {ok, CBState} ->
                    St1 = St0#state{
                        frame_type = FrameType,
                        encoding = EncName,
                        max_len = MaxLen,
                        protocol_state = CBState
                    },
                    ok = send_frame(
                        <<?RAW_MAGIC, Len:4, Enc:4, 0:8, 0:8>>, St1),
                    _ = lager:info(
                        "Established connection with peer, protocol=wamp,"
                        " transport=raw, frame_type=~p, encoding=~p,"
                        " message_max_length=~p, peer=~s",
                        [FrameType, EncName, MaxLen, inet_utils:peername_to_binary(Peer)]
                    ),
                    {ok, St1};
                {error, Reason} ->
                    {stop, Reason, St0}
            end;
        {ok, NonIPAddr} ->
            _ = lager:error(
                "Unexpected peername when establishing connection,"
                " received a non IP address of '~p', reason=invalid_socket,"
                " protocol=wamp, transport=raw, frame_type=~p, encoding=~p,"
                " message_max_length=~p",
                [NonIPAddr, FrameType, EncName, MaxLen]
            ),
            {stop, invalid_socket, St0};
        {error, Reason} ->
            _ = lager:error(
                "Invalid peername when establishing connection,"
                " reason=~p, description=~p, protocol=wamp, transport=raw,"
                " frame_type=~p, encoding=~p, message_max_length=~p",
                [Reason, inet:format_error(Reason), FrameType, EncName, MaxLen]
            ),
            {stop, invalid_socket, St0}
    end.



%% @private
-spec send(binary() | list(), state()) -> ok | {error, any()}.

send(L, St) when is_list(L) ->
    lists:foreach(fun(Bin) -> send(Bin, St) end, L);

send(Bin, St) ->
    send_frame(?RAW_FRAME(Bin), St).


%% @private
-spec send_frame(binary(), state()) -> ok | {error, any()}.

send_frame(Frame, St) when is_binary(Frame) ->
    (St#state.transport):send(St#state.socket, Frame).


%% @private
send_ping(St) ->
    send_ping(integer_to_binary(erlang:system_time(microsecond)), St).


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Sends a ping message with a reference() as a payload to the client and sent
%% ourselves a ping_timeout message in the future.
%% @end
%% -----------------------------------------------------------------------------
send_ping(Bin, St0) ->
    ok = send_frame(<<0:5, 1:3, (byte_size(Bin)):24, Bin/binary>>, St0),
    Timeout = application:get_env(bondy, ping_timeout, ?PING_TIMEOUT),
    TimerRef = erlang:send_after(Timeout, self(), ping_timeout),
    St1 = St0#state{
        ping_sent = {true, Bin, TimerRef},
        ping_attempts = St0#state.ping_attempts + 1
    },
    {ok, St1}.


%% -----------------------------------------------------------------------------
%% @doc
%% The possible values for "LENGTH" are:
%%
%% 0: 2**9 octets
%% 1: 2**10 octets ...
%% 15: 2**24 octets
%%
%% This means a _Client_ can choose the maximum message length between *512*
%% and *16M* octets.
%% @end
%% -----------------------------------------------------------------------------
validate_max_len(N) when N >= 0, N =< 15 ->
    trunc(math:pow(2, 9 + N));

validate_max_len(_) ->
    %% TODO define correct error return
    throw(maximum_message_length_unacceptable).


%% 0: illegal
%% 1: JSON
%% 2: MessagePack
%% 3 - 15: reserved for future serializers
validate_encoding(1) -> {binary, json};
validate_encoding(2) -> {binary, msgpack};
% validate_encoding(3) -> cbor;
validate_encoding(4) -> {binary, bert};
validate_encoding(15) -> {binary, erl};
validate_encoding(_) ->
    %% TODO define correct error return
    throw(serializer_unsupported).


%% 0: illegal (must not be used)
%% 1: serializer unsupported
%% 2: maximum message length unacceptable
%% 3: use of reserved bits (unsupported feature)
%% 4: maximum connection count reached
%% 5 - 15: reserved for future errors
error_number(serializer_unsupported) ->?RAW_ERROR(1);
error_number(maximum_message_length_unacceptable) ->?RAW_ERROR(2);
error_number(use_of_reserved_bits) ->?RAW_ERROR(3);
error_number(maximum_connection_count_reached) ->?RAW_ERROR(4).


%% error_reason(1) -> serializer_unsupported;
%% error_reason(2) -> maximum_message_length_unacceptable;
%% error_reason(3) -> use_of_reserved_bits;
%% error_reason(4) -> maximum_connection_count_reached.


%% @private
close_socket(Reason, St) when Reason =:= normal orelse Reason =:= shutdown ->
    ok = (St#state.transport):close(St#state.socket),
    _ = log(info, <<"TCP Connection closed, reason=~p">>, [Reason], St),
    socket_closed(false, St);

close_socket(Reason, St) ->
    ok = (St#state.transport):close(St#state.socket),
    _ = log(error, <<"TCP Connection closed, reason=~p">>, [Reason], St),
    socket_closed(true, St).


%% @private
log(Level, Prefix, Head, St)
when is_binary(Prefix) orelse is_list(Prefix), is_list(Head) ->
    Format = iolist_to_binary([
        Prefix,
        <<
            ", transport=raw, frame_type=~p, encoding=~p"
            ", message_max_length=~p, peer=~s"
        >>
    ]),
    Peername = inet_utils:peername_to_binary(
        bondy_wamp_protocol:peer(St#state.protocol_state)),
    Tail = [
        St#state.frame_type,
        St#state.encoding,
        St#state.max_len,
        Peername
    ],
    lager:log(Level, self(), Format, lists:append(Head, Tail)).


%% @private
socket_opened(_St) ->
    _ = bondy_stats:socket_open(wamp, raw),
    ok.

%% @private
socket_closed(true, St) ->
    ok = bondy_stats:socket_error(wamp, raw),
    socket_closed(false, St);

socket_closed(false, St) ->
    Seconds = erlang:monotonic_time(second) - St#state.start_time,
    bondy_stats:socket_closed(wamp, raw, Seconds).

