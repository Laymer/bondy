%% =============================================================================
%%  mops.erl -
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


%% -----------------------------------------------------------------------------
%% @doc
%% This module enables the evaluation of a very simple mustache-inspired 
%% Expression lnguage against a Context, where the latter is represented by a 
%% map where all keys are binaries.
%%
%% The following are the key characteristics and features:
%% * 
%% @end
%% -----------------------------------------------------------------------------
-module(mops).
-define(START, <<"{{">>).
-define(END, <<"}}">>).
-define(PIPE_OP, <<$|,$>>>).
-define(DOUBLE_QUOTES, <<$">>).

-define(OPTS_SPEC, #{
    %% Defines the format of the keys in the map
    labels => #{
        required => true,
        default => binary,
        allow_null => false,
        allow_undefined => false,
        datatype => {in, [binary, atom]}
    },
    %% Allows to register custom pipe operators (aka filters)
    operators => #{
        required => false,
        allow_null => false,
        validator => fun
            ({Name, Fun} = Op) when (is_atom(Name) 
            orelse is_binary(Name)) 
            andalso is_function(Fun, 1) ->
                {ok, Op};
            (_) ->
                false
        end
    }
}).

-record(state, {
    context                                                     ::  map(),
    opts                                                        ::  map(),
    acc = []                                                    ::  any(),
    is_ground = true                                            ::  boolean(),
    is_open = false                                             ::  boolean(),
    sp = binary:compile_pattern(?START)                         ::  any(),
    dqp = binary:compile_pattern(?DOUBLE_QUOTES)                ::  any(),
    pop = binary:compile_pattern(?PIPE_OP)                      ::  any(),
    ep = binary:compile_pattern(?END)                           ::  any(),
    re = element(2, re:compile("^\\s+|\\s+$", ""))              ::  any()
}).

-type state()       ::  #state{}.
-type context()     ::  map().



-export([eval/2]).
-export([eval/3]).






%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% Evaluates **Term** using the context **Ctxt**
%% Returns **Term** in case the term is not a binary and does not contain a 
%% mops expression. Otherwise, it tries to evaluate the expression using the
%% **Ctxt**.
%%
%% The following are example mops expressions and their meanings:
%% - `<<"{{foo}}">>' - resolve the value for key `<<"foo">>` in the context. 
%% It fails if the context does not have a key named `<<"foo">>'.
%% - `<<"\"{{foo}}\">>' - return a string by resolving the value for key 
%% `<<"foo">>' in the context. It fails if the context does not have a key 
%% named `<<"foo">>'.
%% - `<<"{{foo.bar}}">>' - resolve the value for key path in the context.
%% It fails if the context does not have a key named `<<"foo">>' which has a value that is either a `function()` or a `map()` with a key named `<<"bar">>'.
%% - `<<"{{foo |> integer}}">>' - resolves the value for key `<<"foo">>' in the 
%% context and converts the result to an integer. If the result was a 
%% `function()', it returns a function composition. 
%% It fails if the context does not have a key named `<<"foo">>'
%%
%% Examples:
%% <pre language="erlang">
%% > mops:eval(foo, #{<<"foo">> => bar}).
%% > foo
%% > mops:eval(<<"foo">>, #{<<"foo">> => bar}).
%% > <<"foo">>
%% > mops:eval(1, #{<<"foo">> => bar}).
%% > 1
%% > mops:eval(<<"{{foo}}">>, #{<<"foo">> => bar}).
%% > bar
%% > mops:eval(<<"{{foo.bar}}">>, #{<<"foo">> => #{<<"bar">> => foobar}}).
%% > foobar
%% </pre>
%% @end
%% -----------------------------------------------------------------------------
-spec eval(any(), context()) -> any().

eval(T, Ctxt) ->
    eval(T, Ctxt, #{}).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec eval(any(), context(), map()) -> any().

eval(T, Ctxt, Opts) ->
    do_eval(T, Ctxt, Opts#{unwrap => true}).



%% =============================================================================
%% PRIVATE
%% =============================================================================




%% @private
do_eval(T, Ctxt) ->
    do_eval(T, Ctxt, #{}).


%% @private
-spec do_eval(any(), context(), map()) -> any().

do_eval({'$mops_proxy', T}, Ctxt, Opts) when is_map(Ctxt) ->
    do_eval(T, Ctxt, Opts);

do_eval(T, Ctxt, Opts) when is_function(T, 1), is_map(Ctxt) ->
    maybe_unwrap(T(Ctxt), Opts);

do_eval(T, Ctxt, Opts) when is_map(T), is_map(Ctxt), is_map(Opts) ->
    eval_map(T, Ctxt, Opts);

do_eval(T, Ctxt, Opts) when is_list(T), is_map(Ctxt), is_map(Opts) ->
    eval_list(T, Ctxt, Opts);

do_eval(T, Ctxt, Opts) when is_tuple(T), is_map(Ctxt), is_map(Opts) ->
    list_to_tuple(eval_list(tuple_to_list(T), Ctxt, Opts));

do_eval(<<>>, Ctxt, Opts) when is_map(Ctxt), is_map(Opts) ->
    <<>>;

do_eval(T, Ctxt, Opts) when is_binary(T), is_map(Ctxt), is_map(Opts) ->
    maybe_unwrap(
        eval_expr(T, #state{
            context = Ctxt, 
            opts = maps_utils:validate(Opts, ?OPTS_SPEC) 
        }),
        Opts
    );

do_eval(T, Ctxt, Opts) when is_map(Ctxt), is_map(Opts) ->
    T.


%% @private
eval_map(Map, Ctxt, Opts) ->
    Fold = fun(K, V0, {L, R}) ->
        case do_eval(V0, Ctxt, Opts) of
            {'$mops_proxy', _} = V1 ->
                {L, [{K, V1}|R]};
            V1 ->
                {[{K, V1}|L], R}
        end
    end,
    case maps:fold(Fold, {[], []}, Map) of
        {Ls, []} ->
            maps:from_list(Ls);
        {Ls, Rs} ->
            maybe_unwrap(
                {'$mops_proxy', maps:from_list(lists:append(Ls, Rs))}, Opts)
    end.

%% @private
eval_list(List, Ctxt, Opts) ->
    Fold = fun(V0, {L, R}) ->
        case do_eval(V0, Ctxt, Opts) of
            {'$mops_proxy', _} = V1 ->
                {[V1|L], true};
            V1 ->
                {[V1|L], R}
        end
    end,
    case lists:foldl(Fold, {[], false}, List) of
        {L, false} ->
            L;
        {L, true} ->
            maybe_unwrap({'$mops_proxy', lists:reverse(L)}, Opts)
    end.


%% @private
maybe_unwrap({'$mops_proxy', Val}, #{unwrap := true} = Opts) 
when is_map(Val) ->
    maps:map(fun(_K, V) -> maybe_unwrap(V, Opts) end, Val);

maybe_unwrap({'$mops_proxy', Val}, #{unwrap := true} = Opts) 
when is_list(Val) ->
    [maybe_unwrap(X, Opts) || X <- Val];

maybe_unwrap({'$mops_proxy', Val}, #{unwrap := true}) ->
    Val;

maybe_unwrap(Val, _) ->
    Val.


%% @private
-spec eval_expr(binary(), state()) -> any() | no_return().

eval_expr(<<>>, #state{is_ground = true} = St) ->
    %% The end of a string (quoted expression)
    %% We reverse the list of terms and turn them into iolist()
    Fun = fun
        (X, Acc) -> 
            [term_to_iolist(X)|Acc] 
    end,
    iolist_to_binary(
        lists:foldl(Fun, [], [?DOUBLE_QUOTES | St#state.acc]));

eval_expr(<<>>, #state{is_ground = false} = St) ->
    %% The end of a string (quoted expression)
    %% We have functions so we wrap the iolist in a function receiving
    %% a future context as an argument
    Fun = fun(Ctxt) ->
        Eval = fun
            (F, Acc) when is_function(F, 1) ->
                [term_to_iolist(F(Ctxt))|Acc];
            (Val, Acc) ->
                [term_to_iolist(Val)|Acc]
        end,
        iolist_to_binary(
            lists:foldl(Eval, [], [?DOUBLE_QUOTES | St#state.acc]))
    end,
    {'$mops_proxy', Fun};

eval_expr(Bin0, #state{acc = []} = St0) when is_binary(Bin0) ->
    %% We start processing a binary
    
    case binary:match(Bin0, St0#state.sp) of
        nomatch ->
            %% Not a mop expression, so we return and finish
            Bin0;         
        {Pos, 2}  ->
            Bin1 = trim(Bin0),
            %% Maybe a mop expression as we found the left moustache
            St1 = St0#state{is_open = true},
            Len = byte_size(Bin1),
            case binary:matches(Bin1, St0#state.dqp) of
                [] when Pos =:= 0 ->
                    %% Not a string so we should have a single 
                    %% mustache expression
                    parse_expr(
                        binary:part(Bin1, 2, Len - 4), St1);
                [{0, 1}, {QPos, 1}] when QPos =:= Len - 1 ->
                    %% A string so we might have multiple 
                    %% mustache expressions
                    %% e.g. "Hello {{foo}}. Today is {{date}}."
                    Unquoted = binary:part(Bin1, 1, Len - 2),
                    [Pre, Rest] = binary:split(Unquoted, St0#state.sp),
                    eval_expr(Rest, acc(St1, Pre));
                _ ->
                    %% We found quotes that are in the middle of the string
                    %% or we have no closing quote
                    %% or this is not a string and we have no quotes but chars 
                    %% before opening mustache
                    error(badarg)
            end
    end;

eval_expr(Bin, #state{is_open = true} = St0) when is_binary(Bin) ->
    %% Split produces a list of binaries that are all referencing Bin. 
    %% This means that the data in Bin is not copied to new binaries, 
    %% and that Bin cannot be garbage collected until the results of the split 
    %% are no longer referenced.
    case binary:split(Bin, St0#state.ep) of
        [Expr, Rest] ->
            St1 = St0#state{is_open = false},
            eval_expr(Rest, acc(St1, parse_expr(Expr, St1)));
        _ ->
            %% We did not find matching closing mustaches
            error(badarg)
    end;

eval_expr(Bin, #state{is_open = false} = St) when is_binary(Bin) ->
    %% Split produces a list of binaries that are all referencing Bin. 
    %% This means that the data in Bin is not copied to new binaries, 
    %% and that Bin cannot be garbage collected until the results of the split 
    %% are no longer referenced.
    case binary:split(Bin, St#state.sp) of
        [Pre, Rest]->
            eval_expr(Rest, acc(St#state{is_open = true}, Pre));
        [Bin] ->
            %% No more mustaches so we finish
            eval_expr(<<>>, acc(St, Bin))
    end.
    

%% @private
%% -----------------------------------------------------------------------------
%% @doc
%% Accumulates the evaluation in the case of strings.
%% @end
%% -----------------------------------------------------------------------------
acc(#state{acc = []} = St, Val) ->
    acc(St#state{acc= [?DOUBLE_QUOTES]}, Val);

acc(St, <<>>) ->
    St;

acc(#state{acc = Acc} = St, Val) when is_function(Val, 1) ->
    St#state{acc = [Val | Acc], is_ground = false};

acc(#state{acc = Acc} = St, Val) when is_binary(Val) ->
    St#state{acc = [Val | Acc]};

acc(#state{acc = Acc} = St, Val) ->
    St#state{acc = [io_lib:format("~p", [Val]) | Acc]}.




%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%% -----------------------------------------------------------------------------
parse_expr(Bin, #state{context = Ctxt} = St) ->
    case binary:split(Bin, St#state.pop, [global]) of
        [Val] ->
            get_value(trim(Val), Ctxt);
        [Val | Ops] ->
            %% We evaluate the value and apply the ops in the pipe
            apply_ops([trim(P) || P <- Ops], get_value(trim(Val), Ctxt), Ctxt);
        _ ->
            error(badarg)
    end.


%% @private
get_value(Key, Ctxt) ->
    try  
        PathElems = binary:split(Key, <<$.>>, [global]),
        case get(PathElems, Ctxt) of
            {ok, '$mops_proxy', _Rem} ->
                {'$mops_proxy', fun(X) -> get_value(Key, X) end};
            {ok, Val} ->
                do_eval(Val, Ctxt)
        end
    catch
        error:{badkey, _} ->
            %% We blame the full key
            error({badkeypath, Key})
    end.




%% @private
get(Rem, '$mops_proxy') ->
    {ok, '$mops_proxy', Rem};

get([], Term) ->
    {ok, Term};

get(Key, {'$mops_proxy', F}) when is_function(F, 1) ->
    Fun = fun(X) ->
        try 
            get_value(Key, F(X))
        catch
            error:{badkey, _} ->
                error({badkeypath, Key})
        end
    end,
    {ok, {'$mops_proxy', Fun}};

get([H|T], Ctxt) when is_map(Ctxt) ->
    get(T, maps:get(H, Ctxt)).


%% @private
apply_ops([H|T], Val, Ctxt) ->
    apply_ops(T, apply_op(H, Val, Ctxt), Ctxt);

apply_ops([], Val, _) ->
    Val.


%% @private
apply_op(Op, {'$mops_proxy', Val}, _) 
when is_function(Val, 1) andalso (
    Op == <<"integer">> orelse 
    Op == <<"float">> orelse
    Op == <<"abs">> orelse
    Op == <<"head">> orelse
    Op == <<"tail">> orelse
    Op == <<"last">> orelse
    Op == <<"length">>
    ) ->
    {'$mops_proxy', fun(X) -> apply_op(Op, Val(X), X) end};

apply_op(_, <<>>, _) ->
    <<>>;

apply_op(<<"abs">>, Val, _) when is_number(Val) ->
    abs(Val);

apply_op(<<"integer">>, Val, _) when is_binary(Val) ->
    binary_to_integer(Val);

apply_op(<<"integer">>, Val, _) when is_list(Val) ->
    list_to_integer(Val);

apply_op(<<"integer">>, Val, _) when is_integer(Val) ->
    Val;

apply_op(<<"integer">>, Val, _) when is_float(Val) ->
    trunc(Val);

apply_op(<<"float">>, Val, _) when is_binary(Val) ->
    binary_to_float(Val);

apply_op(<<"float">>, Val, _) when is_list(Val) ->
    float(list_to_integer(Val));

apply_op(<<"float">>, Val, _) when is_float(Val) ->
    Val;

apply_op(<<"float">>, Val, _) when is_integer(Val) ->
    float(Val);

apply_op(<<"head">>, [], _) ->
    <<>>;

apply_op(<<"head">>, Val, _) when is_list(Val) ->
    hd(Val);

apply_op(<<"tail">>, [], _) ->
    %% To avoid getting an exception at runtime we return []
    [];

apply_op(<<"tail">>, Val, _) when is_list(Val) ->
    tl(Val);

apply_op(<<"last">>, [], _) ->
    <<>>;

apply_op(<<"last">>, Val, _) when is_list(Val) ->
    lists:last(Val);

apply_op(<<"length">>, Val, _) when is_list(Val) ->
    length(Val);

%% Custom ops
apply_op(Bin, Val, Ctxt) ->
    apply_custom_op(Bin, Val, Ctxt).


%% @private

apply_custom_op(<<"get", _/binary>> = Op, {'$mops_proxy', _} = Val, _) ->
    {'$mops_proxy',
        fun(X) -> apply_custom_op(Op, maybe_eval(Val, X), X) end
    };

apply_custom_op(<<"get(", Rest/binary>> = Op, Val, Ctxt) when is_map(Val)->
    case [maybe_eval(K, Ctxt) || K <- get_arguments(Rest, Op, <<")">>)] of
        [Key] when is_function(Key, 1) ->
            fun(X) -> maps:get(Key(X), Val) end;
        [Key] ->
            maps:get(Key, Val);
        [Key, Default] when is_function(Key, 1), is_function(Default, 1) ->
            fun(X) -> maps:get(Key(X), Val, Default(X)) end;
        [Key, Default] when is_function(Default, 1) ->
            fun(X) -> maps:get(Key, Val, Default(X)) end;
        [Key, Default] when is_function(Key, 1) ->
            fun(X) -> maps:get(Key(X), Val, Default) end;
        [Key, Default] ->
            maps:get(Key, Val, Default);
        _ -> 
            error({invalid_expression, Op})
    end;

%% apply_custom_op(<<"merge(", _/binary>> = Op, Val, _) when is_function(Val, 1) ->
%%     fun(X) -> apply_custom_op(Op, Val(X), X) end;

%% apply_custom_op(<<"merge(", Rest/binary>> = Op, Val, Ctxt) when is_map(Val)->
%%     case [maybe_eval(A, Ctxt) || A <- get_arguments(Rest, Op, <<")">>)] of
%%         [R] when is_function(R, 1) ->
%%             %% fun(X) -> apply_custom_op(Op, Val, X) end;
%%             fun(X) -> maps:merge(R(X), Val) end;
%%         [R] when is_map(R) ->
%%             maps:merge(Val, R);
%%         [<<"_">>, R] when is_function(R, 1) ->
%%             %% fun(X) -> apply_custom_op(Op, Val, X) end;
%%             fun(X) -> maps:merge(R(X), Val) end;
%%         [<<"_">>, R] when is_map(R) ->
%%             maps:merge(Val, R);
%%         [L, <<"_">>] when is_function(L, 1) ->
%%             %% fun(X) -> apply_custom_op(Op, Val, X) end;
%%             fun(X) -> maps:merge(Val, L(X)) end;
%%         [L, <<"_">>] ->
%%             maps:merge(L, Val);
%%         _ -> 
%%             error({invalid_expression, Op})
%%     end;


apply_custom_op(<<"merge(", Rest/binary>> = Op, Expr1, Ctxt) ->

    Args = case 
        [maybe_eval(A, Ctxt) || A <- get_arguments(Rest, Op, <<")">>)] 
    of
        [Expr2] ->
            {Expr1, Expr2};
        [<<"_">>, Expr2] ->
            {Expr1, Expr2};
        [Expr2, <<"_">>] ->
            {Expr2, Expr1};
        _ ->
            error({invalid_expression, Op})
    end,
    case Args of
        {{'$mops_proxy', LT}, {'$mops_proxy', RT}} 
        when is_map(LT) andalso is_map(RT) ->
            {'$mops_proxy', maps:merge(LT, RT)};

        {{'$mops_proxy', LT}, R} when is_map(LT) andalso is_map(R) ->
            {'$mops_proxy', maps:merge(LT, R)};

        {L, {'$mops_proxy', RT}} when is_map(L) andalso is_map(RT) ->
            {'$mops_proxy', maps:merge(L, RT)};

        {{'$mops_proxy', LT} = L, R} when is_function(LT, 1) ->
            {'$mops_proxy',
                fun(X) -> maybe_merge(maybe_eval(L, X), maybe_eval(R, X)) end
            };

        {L, {'$mops_proxy', RT} = R} when is_function(RT, 1) ->
            {'$mops_proxy',
                fun(X) -> maybe_merge(maybe_eval(L, X), maybe_eval(R, X)) end
            };
        
        {L, R} when is_map(L), is_map(R) ->
            maps:merge(L, R);

        _ -> 
            error({invalid_expression, Op})
    end;

apply_custom_op(<<"with(", _/binary>> = Op, {'$mops_proxy', _} = L, _) ->
    {'$mops_proxy',
        fun(X) -> apply_custom_op(Op, maybe_eval(L, X), X) end
    };

apply_custom_op(<<"without(", _/binary>> = Op, {'$mops_proxy', _} = L, _) ->
    {'$mops_proxy',
        fun(X) -> apply_custom_op(Op, maybe_eval(L, X), X) end
    };

apply_custom_op(<<"with([", Rest/binary>> = Op, Val, Ctxt) when is_map(Val)->
    Fold = fun(K, {L, R}) ->
        case maybe_eval(K, Ctxt) of
            {'$mops_proxy', _} = V ->
                {L, [V|R]};
            V ->
                {[V|L], R}
        end
    end,
    Args = get_arguments(Rest, Op, <<"])">>),
    case lists:foldl(Fold, {[], []}, Args) of
        {Ls, []} ->
            maps:with(Ls, Val);
        {Ls, Rs} ->
            {'$mops_proxy',
                fun(X) -> 
                    Keys = lists:append(Ls, [maybe_eval(R, X) || R <- Rs]),
                    maps:with(Keys, Val) 
                end
            }
    end;

apply_custom_op(<<"without([", Rest/binary>> = Op, Val, Ctxt) when is_map(Val)->
    Fold = fun(K, {L, R}) ->
        case maybe_eval(K, Ctxt) of
            {'$mops_proxy', _} = V  ->
                {L, [V|R]};
            V ->
                {[V|L], R}
        end
    end,
    Args = get_arguments(Rest, Op, <<"])">>),
    case lists:foldl(Fold, {[], []}, Args) of
        {Ls, []} ->
            maps:without(Ls, Val);
        {Ls, Rs} ->
            {'$mops_proxy',
                fun(X) -> 
                    Keys = lists:append(Ls, [maybe_eval(R, X) || R <- Rs]),
                    maps:with(Keys, Val) 
                end
            }
    end;

apply_custom_op(Op, Val, _) ->
    error({invalid_expression, [Op, Val]}).


%% @private
maybe_merge({'$mops_proxy', L}, {'$mops_proxy', R}) 
when is_map(L) andalso is_map(R) ->
    {'$mops_proxy', maps:merge(L, R)};

maybe_merge({'$mops_proxy', _} = L, R) ->
    {'$mops_proxy', 
        fun(X) -> maybe_merge(maybe_eval(L, X), maybe_eval(R, X)) end
    };

maybe_merge(L, {'$mops_proxy', _} = R) ->
    {'$mops_proxy', 
        fun(X) -> maybe_merge(maybe_eval(L, X), maybe_eval(R, X)) end
    };

maybe_merge(L, R) ->
    maps:merge(L, R).


%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Evaluates only binary terms and any other term wrapped in a 
%% '$mops_proxy' tuple. For all other terms, returns the term untouched.
%% @end
%% -----------------------------------------------------------------------------
maybe_eval({'$mops_proxy', T}, Ctxt) ->
    do_eval(T, Ctxt);

maybe_eval(Term, Ctxt) when is_binary(Term) ->
    case unquote(Term) of
        {true, Key} ->
            Key;
        {false, Expr} ->
            do_eval(Expr, Ctxt)
    end;

maybe_eval(Term, _Ctxt) ->
    Term.


%% @private
unquote(<<$', Rest/binary>>) ->
    {true, binary:part(Rest, {0, byte_size(Rest) - 1})};
    
unquote(Other) ->
    {false, Other}.    


%% @private
get_arguments(Rest, Op, Terminal) ->
    Len = byte_size(Rest),
    TLen = size(Terminal),
    case binary:matches(Rest, Terminal) of
        [{Pos, TLen}] when Len - TLen == Pos ->
            Part = binary:part(Rest, 0, Len - TLen),
            binary:split(Part, <<$,>>, [global, trim_all]);
        _ ->
            error({invalid_expression, Op})
    end.


%% @private
term_to_iolist(Term) when is_binary(Term) ->
    Term;

term_to_iolist(Term) when is_list(Term) ->
    Term;
    
term_to_iolist(Term) ->
    io_lib:format("~p", [Term]).




%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Eliminates whitespaces in a binary
%% @end
%% -----------------------------------------------------------------------------
trim(Bin) ->
    trimb(Bin, <<>>).

%% @private
trimb(<<>>, Acc) ->
    Acc;

trimb(<<C, Rest/binary>>, Acc) when C =:= $\s; C =:= $\t ->
    trimb(Rest, Acc);

trimb(Bin, Acc) ->
    case binary:match(Bin, [<<$\s>>, <<$\t>>], []) of 
        nomatch ->
            <<Acc/binary, Bin/binary>>;
        {Pos, 1} ->
            Part = binary:part(Bin, 0, Pos),
            trimb(
                binary:part(Bin, Pos, byte_size(Bin) - Pos), 
                <<Acc/binary, Part/binary>>)
    end.
