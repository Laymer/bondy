%% =============================================================================
%%  bondy_utils.erl -
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

-module(bondy_utils).
-include("bondy.hrl").
-include_lib("wamp/include/wamp.hrl").

-export([bin_to_pid/1]).
-export([decode/2]).
-export([encode/2]).
-export([foreach/2]).
-export([generate_fragment/1]).
-export([get_flake_id/0]).
-export([get_id/1]).
-export([get_nonce/0]).
-export([get_random_string/2]).
-export([is_uuid/1]).
-export([log/5]).
-export([maybe_encode/2]).
-export([merge_map_flags/2]).
-export([pid_to_bin/1]).
-export([timeout/1]).
-export([to_binary_keys/1]).
-export([uuid/0]).



%% =============================================================================
%%  API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec foreach(
    fun((Elem :: term()) -> term()), ?EOT | {[term()], any()} | any()) -> ok.

foreach(_, ?EOT) ->
    ok;

foreach(Fun, {L, Cont}) ->
    ok = lists:foreach(Fun, L),
    foreach(Fun, Cont);

foreach(Fun, Cont) when is_function(Cont, 0) ->
    foreach(Fun, Cont()).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
pid_to_bin(Pid) ->
    list_to_binary(pid_to_list(Pid)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
bin_to_pid(Bin) ->
    list_to_pid(binary_to_list(Bin)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec log(
    atom(), binary() | list(), list(), wamp_message(), bondy_context:t()) -> ok.

log(Level, Prefix, Head, WampMessage, Ctxt)
when is_binary(Prefix) orelse is_list(Prefix), is_list(Head) ->
    Format = iolist_to_binary([
        Prefix,
        <<
            %% Right now trace_id is a bin as msgpack does not support
            %% 128-bit integers
            ", trace_id=~s"
            ", realm_uri=~s"
            ", session_id=~p"
            ", message_type=~s"
            ", message_id=~p"
            ", encoding=~s"
        >>
    ]),
    TraceId = <<>>,
    Tail = [
        TraceId,
        bondy_context:realm_uri(Ctxt),
        bondy_context:session_id(Ctxt),
        element(1, WampMessage), %% add function to wamp_message.erl
        element(2, WampMessage), %% add function to wamp_message.erl
        bondy_context:encoding(Ctxt)
    ],
    _ = lager:log(Level, self(), Format, lists:append(Head, Tail)),
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
to_binary_keys(Map) when is_map(Map) ->
    F = fun
        (K, V, Acc) when is_binary(K) ->
            maps:put(K, maybe_to_binary_keys(V), Acc);
        (K, V, Acc) when is_atom(K) ->
            maps:put(list_to_binary(atom_to_list(K)), maybe_to_binary_keys(V), Acc)
    end,
    maps:fold(F, #{}, Map).



%% @private
maybe_to_binary_keys(T) when is_map(T) ->
    to_binary_keys(T);
maybe_to_binary_keys(T) ->
    T.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec uuid() -> bitstring().

uuid() ->
    list_to_bitstring(uuid:uuid_to_string(uuid:get_v4())).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_uuid(any()) -> boolean().

is_uuid(Term) when is_bitstring(Term) ->
    uuid:is_v4(uuid:string_to_uuid(bitstring_to_list(Term)));

is_uuid(_) ->
    false.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
maybe_encode(_, <<>>) ->
    <<>>;

maybe_encode(json, Term) ->
    case jsx:is_json(Term) of
        true ->
            Term;
        false ->
            jsx:encode(Term)
    end;

 maybe_encode(msgpack, Term) ->
     %% TODO see if we can catch error when Term is already encoded
     msgpack:pack(Term).



%% @private
decode(json, <<>>) ->
    <<>>;

decode(json, Term) ->
    jsx:decode(Term, [return_maps]);

decode(msgpack, Term) ->
    Opts = [
        {map_format, map},
        {unpack_str, as_binary}
    ],
    {ok, Bin} = msgpack:unpack(Term, Opts),
    Bin;
decode(ContentType, Term) ->
    %% We cannot decode this so create a wrapped data object
    #{<<"type">> => ContentType, <<"content">> => Term}.



%% @private
encode(json, Term) ->
    jsx:encode(Term);

encode(msgpack, Term) ->
    Opts = [
        {map_format, map},
        {pack_str, from_binary}
    ],
    msgpack:pack(Term, Opts).

%% -----------------------------------------------------------------------------
%% @doc
%% IDs in the _global scope_ MUST be drawn _randomly_ from a _uniform
%% distribution_ over the complete range [0, 2^53]
%% @end
%% -----------------------------------------------------------------------------
-spec get_id(Scope :: global | {router, uri()} | {session, id()}) -> id().

get_id(global) ->
    %% IDs in the _global scope_ MUST be drawn _randomly_ from a _uniform
    %% distribution_ over the complete range [0, 2^53]
    wamp_utils:rand_uniform();

get_id({router, _}) ->
    get_id(global);

get_id({session, SessionId}) when is_integer(SessionId) ->
    %% IDs in the _session scope_ SHOULD be incremented by 1 beginning
    %% with 1 (for each direction - _Client-to-Router_ and _Router-to-
    %% Client_)
    bondy_session:incr_seq(SessionId).


%% -----------------------------------------------------------------------------
%% @doc Calls flake_server:id/0 and returns the generated ID.
%% @end
%% -----------------------------------------------------------------------------
-spec get_flake_id() -> binary().
get_flake_id() ->
    {ok, Id} = flake_server:id(),
    Id.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
timeout(#{timeout := T}) when is_integer(T), T > 0 ->
    T;
timeout(#{timeout := 0}) ->
    infinity;
timeout(_) ->
    bondy_config:request_timeout().


%% -----------------------------------------------------------------------------
%% @doc
%% The call will fail with a {badkey, any()} exception is any key found in M1
%% is not present in M2.
%% @end
%% -----------------------------------------------------------------------------
merge_map_flags(M1, M2) when is_map(M1) andalso is_map(M2) ->
    maps:fold(fun merge_fun/3, M2, M1).



%% Borrowed from
%% https://github.com/kivra/oauth2/blob/master/src/oauth2_token.erl
-spec generate_fragment(non_neg_integer()) -> binary().

generate_fragment(0) ->
    <<>>;

generate_fragment(N) ->
    Rand = base64:encode(crypto:strong_rand_bytes(N)),
    Frag = << <<C>> || <<C>> <= <<Rand:N/bytes>>, is_alphanum(C) >>,
    <<Frag/binary, (generate_fragment(N - byte_size(Frag)))/binary>>.


%% @doc Returns true for alphanumeric ASCII characters, false for all others.
-spec is_alphanum(char()) -> boolean().

is_alphanum(C) when C >= 16#30 andalso C =< 16#39 -> true;
is_alphanum(C) when C >= 16#41 andalso C =< 16#5A -> true;
is_alphanum(C) when C >= 16#61 andalso C =< 16#7A -> true;
is_alphanum(_)                                    -> false.




%% =============================================================================
%%  PRIVATE
%% =============================================================================



%% @private
merge_fun(K, V, Acc) ->
    case {maps:get(K, Acc, undefined), V} of
        {true, true} -> Acc;
        {false, false} -> Acc;
        _ -> maps:put(K, false, Acc)
    end.


get_nonce() ->
    list_to_binary(
        get_random_string(
            32,
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")).



%% -----------------------------------------------------------------------------
%% @doc
%% borrowed from
%% http://blog.teemu.im/2009/11/07/generating-random-strings-in-erlang/
%% @end
%% -----------------------------------------------------------------------------
get_random_string(Length, AllowedChars) ->
    lists:foldl(
        fun(_, Acc) ->
            [lists:nth(rand:uniform(length(AllowedChars)),
            AllowedChars)]
            ++ Acc
        end,
        [],
        lists:seq(1, Length)).