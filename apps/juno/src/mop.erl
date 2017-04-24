% dbg:tracer(), dbg:p(all,c), dbg:tpl(mop, '_', []).
% Bin = <<"\"{{foo}}\"">>.
% Len = byte_size(Bin).
% mop:eval(Bin, #{<<"foo">> => 3}).
% mop:eval(<<"\"Hello {{foo}}, {{foo}}\"">>, #{<<"foo">> => 3}).
% mop:eval(<<"\"Hello {{foo | float | integer}}, {{foo | integer}}\"">>, #{<<"foo">> => 3}).
% [_, Fun, _]=mop:eval(<<"\"{{foo.bar.a}}\"">>, #{<<"foo">> => fun(X) -> X end}).
% Fun(#{<<"bar">> => #{<<"a">> => 3}}).
-module(mop).
-define(START, <<"{{">>).
-define(END, <<"}}">>).

-record(state, {
    context             :: map(),
    acc = []            :: any(),
    is_ground = true    :: boolean(),
    is_open = false     :: boolean()
}).
-type state()       :: #state{}.

-export([eval/2]).





%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec eval(any(), Ctxt :: map()) -> any() | no_return().
eval(Val, Ctxt) when is_binary(Val) ->
    do_eval(Val, #state{context = Ctxt});

eval(Val, _) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec do_eval(binary(), state()) -> any() | no_return().
do_eval(<<>>, #state{is_ground = true} = St) ->
    iolist_to_binary(lists:reverse([$" | St#state.acc]));

do_eval(<<>>, St) ->
    lists:reverse([$" | St#state.acc]); 

do_eval(Bin0, #state{acc = []} = St0) ->
    Bin1 = trim(Bin0),
    case binary:match(Bin1, ?START) of
        nomatch ->
            Bin1;         
        {Pos, 2}  ->
            St1 = St0#state{is_open = true},
            Len = byte_size(Bin1),
            case binary:matches(Bin1, <<$">>) of
                [] when Pos =:= 0 ->
                    %% Not a string so we should have a single 
                    %% mustache expression
                    parse_expr(
                        binary:part(Bin1, 2, Len - 4), St1#state.context);
                [{0, 1}, {QPos, 1}] when QPos =:= Len - 1 ->
                    %% A string so we might have multiple 
                    %% mustache expressions
                    %% e.g. "Hello {{foo}}. Today is {{date}}."
                    Unquoted = binary:part(Bin1, 1, Len - 2),
                    [Pre, Rest] = binary:split(Unquoted, ?START),
                    do_eval(Rest, acc(St1, Pre));
                _ ->
                    %% We found quotes that are in the middle of the string
                    %% or we have no closing quote
                    %% or we have no quotes but chars before opening mustache
                    error(badarg)
            end
    end;

do_eval(Bin, #state{is_open = true} = St0) ->
    case binary:split(Bin, ?END) of
        [Expr, Rest] ->
            St1 = St0#state{is_open = false},
            do_eval(Rest, acc(St1, parse_expr(Expr, St1#state.context)));
        _ ->
            error(badarg)
    end;

do_eval(Bin, #state{is_open = false} = St) ->
    case binary:split(Bin, ?START) of
        [Pre, Rest]->
            do_eval(Rest, acc(St#state{is_open = true}, Pre));
        [Bin] ->
            do_eval(<<>>, acc(St, Bin))
    end.

    




%% =============================================================================
%% PRIVATE
%% =============================================================================
acc(#state{acc = []} = St, Val) ->
    acc(St#state{acc= [$"]}, Val);

acc(St, <<>>) ->
    St;

acc(#state{acc = Acc} = St, Val) when is_function(Val) ->
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
parse_expr(Bin, Ctxt) ->
    case binary:split(Bin, <<$|>>, [global]) of
        [Val | Pipes] ->
            apply_ops([trim(P) || P <- Pipes], get_value(trim(Val), Ctxt));
        _ ->
            error(badarg)
    end.



%% @private
get_value(Key, Ctxt) when is_map(Ctxt) ->
    try 
        case binary:split(Key, <<$.>>) of
            [Key] ->
                maps:get(Key, Ctxt);
            [A, B] ->
                case  maps:get(A, Ctxt) of
                    Val when is_map(Val) ->
                        get_value(B, Val);
                    Val when is_function(Val, 1) ->
                        fun(X) ->
                            try 
                                get_value(B, Val(X))
                            catch
                                error:{badkey, _} ->
                                    error({badkey, Key})
                            end
                        end
                end
        end
    catch
        error:{badkey, _} ->
            error({badkey, Key})
    end.



%% @private
apply_ops([H|T], Acc) ->
    apply_ops(T, apply_op(H, Acc));

apply_ops([], Acc) ->
    Acc.


%% @private
apply_op(<<"integer">>, Val) when is_binary(Val) ->
    binary_to_integer(Val);

apply_op(<<"integer">>, Val) when is_list(Val) ->
    list_to_integer(Val);

apply_op(<<"integer">>, Val) when is_integer(Val) ->
    Val;

apply_op(<<"integer">>, Val) when is_float(Val) ->
    trunc(Val);

apply_op(<<"float">>, Val) when is_binary(Val) ->
    binary_to_float(Val);

apply_op(<<"float">>, Val) when is_list(Val) ->
    float(list_to_integer(Val));

apply_op(<<"float">>, Val) when is_float(Val) ->
    Val;

apply_op(<<"float">>, Val) when is_integer(Val) ->
    float(Val).


%% @private
trim(Bin) ->
    re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).