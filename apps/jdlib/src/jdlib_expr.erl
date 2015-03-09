%% @doc
%% Mathematical expressions manipulation.
%%
%% <ul>
%% <b>The following operations is supported:</b>
%%   <li><tt>neg</tt> - negate (<tt>x</tt> -> <tt>-x</tt>)</li>
%%   <li><tt>sum</tt> - sum of expressions list (<tt>x + y + ...</tt>)</li>
%%   <li><tt>sub</tt> - subtraction of two values (<tt>x - y</li>)</li>
%%   <li><tt>mul</tt> - multiplication of expressions list (<tt>x * y * ...</tt>)</li>
%%   <li><tt>dvs</tt> - division operation(<tt>x / y</tt>)</li>
%%   <li><tt>pow</tt> - возведение в степень(<tt>x^y</tt>)</li>
%% </ul
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_expr).

% Export.
-export([neg/1, sum/2, sum/1, sub/2, mul/2, mul/1, dvs/2, pow/2,
         is_neg/1, is_sum/1, is_sub/1, is_mul/1, is_dvs/1, is_pow/1, is_oper/2,
         is_eq/2, is_const/1, is_polynomial/1,
         substitute/3, expand_mul/1,
         to_string/1,
         simplify/1, simplify/2,
         rule_normalization/1, rule_normalization/2,
         rule_calculation/1, rule_calculation/2,
         rule_collect_items/1, rule_collect_items/2,
         rule_sum_mul_pairs/1, rule_sum_mul_pairs/2,
         rule_open_brackets/1, rule_open_brackets/2,
         rule_collect_negs/1, rule_collect_negs/2,
         rule_split_sum/1, rule_split_sum/2]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

% Very small value.
-define(EPS, 1.0e-10).

% Less than, greater than and equal check.
% Written as macroses for use in guard expressions.
-define(IS_LT(X, Y), (X < Y - ?EPS)).
-define(IS_GT(X, Y), (X > Y + ?EPS)).
-define(IS_EQ(X, Y), (not (?IS_LT(X, Y) orelse ?IS_GT(X, Y)))).
-define(IS_LE(X, Y), (not ?IS_GT(X, Y))).
-define(IS_GE(X, Y), (not ?IS_LT(X, Y))).

% Check if expression is trivial (can not be optimized).
-define(IS_TRIVIAL(X), (is_integer(X) orelse is_atom(X))).

% Check if expression is atom (can not be decomposed).
-define(IS_ATOM(X), (is_number(X) orelse is_atom(X))).

% Check if term is expression.
% It is macros for use in guard expressions.
% It is just light check, even if ?IS_EXPR(X) =:= true X may be not valid expression.
-define(IS_EXPR(X), (?IS_ATOM(X) orelse is_list(X))).

% Check if oper unary.
-define(IS_UNARY(OPER), (OPER =:= neg)).

% Check if oper binary.
-define(IS_BINARY(OPER), ((OPER =:= sub) orelse (OPER =:= dvs) orelse (OPER =:= pow))).

% Check if oper has list of arguments.
-define(IS_MULTINARY(OPER), ((OPER =:= sum) orelse (OPER =:= mul))).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([unary_operation/0, binary_operation/0, multinary_operation/0, operation/0,
              expr/0,
              options/0]).

% Unary operation.
-type unary_operation() :: neg.

% Binary operation.
-type binary_operation() :: sub | dvs | pow.

% Operation with many arguments.
-type multinary_operation() :: sum | mul.

% Operation.
-type operation() :: unary_operation() | binary_operation() | multinary_operation().

% Expression.
-type expr() :: number() | atom() | list().

% Expressions list.
-type exprs_list() :: [expr()].

% Options.
-type options() :: #{}.

%---------------------------------------------------------------------------------------------------
% Contructors.
%---------------------------------------------------------------------------------------------------

-spec neg(X :: expr()) -> expr().
%% @doc
%% Negate constructor.
neg(X) when ?IS_EXPR(X) ->
    simplify([neg, X]).

%---------------------------------------------------------------------------------------------------

-spec sum(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Sum constructor.
sum(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify([sum, X, Y]).

%---------------------------------------------------------------------------------------------------

-spec sum(L :: exprs_list()) -> expr().
%% @doc
%% Sum constructor.
sum([]) ->
    throw({badarg, []});
sum(L) when is_list(L) ->
    simplify([sum | L]).

%---------------------------------------------------------------------------------------------------

-spec sub(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Sub constructor.
sub(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify([sub, X, Y]).

%---------------------------------------------------------------------------------------------------

-spec mul(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Mul constructor.
mul(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify([mul, X, Y]).

%---------------------------------------------------------------------------------------------------

-spec mul(L :: exprs_list()) -> expr().
%% @doc
%% Mul constructor.
mul([]) ->
    throw({badarg, []});
mul(L) when is_list(L) ->
    simplify([mul | L]).

%---------------------------------------------------------------------------------------------------

-spec dvs(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Division constructor.
dvs(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify([dvs, X, Y]).

%---------------------------------------------------------------------------------------------------

-spec pow(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Power.
pow(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify([pow, X, Y]).

%---------------------------------------------------------------------------------------------------
% Check for particular operation.
%---------------------------------------------------------------------------------------------------

-spec is_neg(E :: expr()) -> boolean().
%% @doc
%% Check if it is neg operation.
is_neg([neg, _]) ->
    true;
is_neg(_) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_sum(E :: expr()) -> boolean().
%% @doc
%% Check if it is sum operation.
is_sum([sum | _]) ->
    true;
is_sum(_) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_sub(E :: expr()) -> boolean().
%% @doc
%% Check if it is sub operation.
is_sub([sub, _, _]) ->
    true;
is_sub(_) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_mul(E :: expr()) -> boolean().
%% @doc
%% Check if it is mul operation.
is_mul([mul | _]) ->
    true;
is_mul(_) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_dvs(E :: expr()) -> boolean().
%% @doc
%% Check if it is dvs operation.
is_dvs([dvs, _, _]) ->
    true;
is_dvs(_) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_pow(E :: expr()) -> boolean().
%% @doc
%% Check if it is pow operation.
is_pow([pow, _, _]) ->
    true;
is_pow(_) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_oper(E :: expr(), Oper :: operation()) -> boolean().
%% @doc
%% Check expression for operation.
is_oper([Oper | _], Oper) -> 
    true;
is_oper(_, _) ->
    false.

%---------------------------------------------------------------------------------------------------
% Expression functions.
%---------------------------------------------------------------------------------------------------

-spec is_eq(E1 :: expr(), E2 :: expr()) -> boolean().
%% @doc
%% Check equality.
%% Warning!
%% E1 is equal to E2 without simplification effect.
%% So [sum, a, b] is not equal to [sum, b, a],
%% but [neg, 1] is equal to [neg, 1.0].
is_eq(E, E) when is_atom(E) ->
    true;
is_eq(E1, E2) when (is_number(E1) andalso is_number(E2)) ->
    ?IS_EQ(E1, E2);
is_eq([Oper | T1], [Oper | T2]) when (length(T1) =:= length(T2)) ->
    lists:all(fun({E1, E2}) -> is_eq(E1, E2) end, lists:zip(T1, T2));
is_eq(_, _) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec is_const(E :: expr()) -> boolean().
%% @doc
%% Check if expression is constant.
is_const([_ | T]) ->
    lists:all(fun is_const/1, T);
is_const(E) ->
    is_number(E).

%---------------------------------------------------------------------------------------------------

-spec is_polynomial(E :: expr()) -> boolean().
%% @doc
%% Check if expression is polynomial (contains only neg, sum, sub, mul, pow operations).
is_polynomial([Oper | T]) when (Oper /= dvs) ->
    lists:all(fun is_polynomial/1, T);
is_polynomial(E) ->
    ?IS_ATOM(E).

%---------------------------------------------------------------------------------------------------

-spec substitute(E :: expr(), A :: atom(), S :: expr()) -> expr().
%% @doc
%% Substiture atom A with expression S.
substitute([Oper | T], A, S) ->
    [Oper | lists:map(fun(E) -> substitute(E, A, S) end, T)];
substitute(A, A, S) ->
    S;
substitute(E, _, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec expand_mul(E :: expr()) -> expr().
%% @doc
%% Expand mul operations.
expand_mul([mul, Arg]) ->
    Arg;
expand_mul([mul, H | T]) ->
    Items_F =
        fun
            ([sum | Items]) ->
                Items;
            ([sub, X, Y]) ->
                [X, [neg, Y]];
            (Any) ->
                [Any]
        end,
    H_Items = Items_F(H),
    New_Items =
        lists:foldl
        (
            fun(E, Acc_Items) ->
                E_Items = Items_F(E),
                [[mul, Acc_Item, E_Item] || Acc_Item <- Acc_Items, E_Item <- E_Items]
            end,
            H_Items,
            T
        ),
    simplify([sum | New_Items]);
expand_mul([pow, X, P] = E) when is_integer(P) ->
    Is_Expand = is_sum(X) orelse is_sub(X),
    if
        Is_Expand ->
            [mul | lists:duplicate(P, X)];
        true ->
            E
    end;
expand_mul(E) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec to_string(E :: expr()) -> string().
%% @doc
%% Convert expression to string.
to_string([neg, X]) ->
    "(-" ++ to_string(X) ++ ")";
to_string([sum, H | T]) ->
    Next_Component_F =
        fun
            ([neg, E]) ->
                " - " ++ to_string(E);
            (E) ->
                " + " ++ to_string(E)
        end,
    "("
    ++ to_string(H)
    ++ lists:append(lists:map(Next_Component_F, T))
    ++ ")";
to_string([sub, X, Y]) ->
    "(" ++ to_string(X) ++ " - " ++ to_string(Y) ++ ")";
to_string([mul, H | T]) ->
    Next_Factor_F = fun(E) -> " * " ++ to_string(E) end,
    "("
    ++ to_string(H)
    ++ lists:append(lists:map(Next_Factor_F, T))
    ++ ")";
to_string([dvs, X, Y]) ->
    "(" ++ to_string(X) ++ " / " ++ to_string(Y) ++ ")";
to_string([pow, X, Y]) ->
    to_string(X) ++ "^" ++ to_string(Y);
to_string(X) ->
    lists:flatten(io_lib:format("~w", [X])).

%---------------------------------------------------------------------------------------------------

-spec mul_coef(E :: expr()) -> {float(), expr()}.
%% @doc
%% Get coefficient of multiplication.
%%   x => 1
%%   -x => -1
%%   5x => 5
mul_coef([neg, X]) ->
    {-1, X};
mul_coef([mul, N, X]) when is_number(N) ->
    {N, X};
mul_coef(X) ->
    {1, X}.

%---------------------------------------------------------------------------------------------------

-spec pow_coef(E :: expr()) -> {float(), expr()}.
%% @doc
%% Get coefficient of power.
%%   x => 1
%%   1 / x => -1
%%   x^5 => 5
pow_coef([dvs, N, X]) when (is_number(N) andalso ?IS_EQ(N, 1)) ->
    {-1, X};
pow_coef([pow, X, N]) when is_number(N) ->
    {N, X};
pow_coef(X) ->
    {1, X}.

%---------------------------------------------------------------------------------------------------
% Options.
%---------------------------------------------------------------------------------------------------

-spec default_options() -> options().
%% @doc
%% Get default options.
default_options() ->
    #{
        % Calculate fractions.
        % It can lead to loss of precision, for example in expression 1 / 3.
        is_calc_frac => true,

        % Ignore division by zero and other indefinities.
        % It allows us to apply such optimizations as x / x = 1.
        is_ignore_indef => true
    }.

%---------------------------------------------------------------------------------------------------
% Simplification functions.
%---------------------------------------------------------------------------------------------------

-spec simplify(E :: expr()) -> expr().
%% @doc
%% Expression simplification.
simplify(E) ->
    simplify(E, default_options()).

-spec simplify(E :: expr(), Opts :: options()) -> expr().
%% @private
%% @doc
%% Expression simplification with given options.
simplify(E, _) when ?IS_TRIVIAL(E) ->
    E;
simplify(E, Opts) when is_float(E) ->
    rules(E, Opts);
simplify([Oper | Args] = E, Opts) ->

    % First try to apply simplification to nested expressions.
    Simp_Args = lists:map(fun(X) -> simplify(X, Opts) end, Args),

    % Then try to simplify whole expression while it is possible.
    New = rules([Oper | Simp_Args], Opts),
    case New of

        % Expression is unchanged - stop simplification.
        E ->
            E;

        % Try simplification more time.
        Simp_E ->
            simplify(Simp_E, Opts)
    end.

%---------------------------------------------------------------------------------------------------
% Simplification rules.
%---------------------------------------------------------------------------------------------------

-spec rules(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Apply some simplification rules to expression.
rules(E, Opts) ->
    Rules =
        [
            rule_normalization,
            rule_calculation,
            rule_collect_items,
            rule_sum_mul_pairs,
            rule_open_brackets,
            rule_collect_negs,
            rule_split_sum
        ],
    lists:foldl(fun(Rule, Cur_E) -> rule(Cur_E, Rule, Opts) end, E, Rules).

%---------------------------------------------------------------------------------------------------

-spec rule(E :: expr(), R :: atom(), Opts :: options()) -> expr().
%% @doc
%% Apply single rule.
%% Apply while it is possible.
rule(E, R, Opts) ->
    case apply(jdlib_expr, R, [E, Opts]) of

        % Nothing has changed. End.
        E ->
            E;

        % Try to apply optimization one more time.
        New ->
            rule(New, R, Opts)
    end.

%---------------------------------------------------------------------------------------------------

-spec rule_normalization(E :: expr()) -> expr().
%% @doc
%% Normalization rule.
rule_normalization(E) ->
    rule_normalization(E, default_options()).

-spec rule_normalization(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Normalization rule.
%% For float values drops zero fraction:
%%   N.0 => N
rule_normalization(E, _) when is_float(E) ->
    F = jdlib_math:fraction(E),
    if
        ?IS_EQ(F, 0) ->
            trunc(E);
        true ->
            E
    end;

% Multinary operations are to flatten:
%   [sum, a, b, c, [sum, x, y, z]] => [sum, a, b, c, x, y, z]
%   [mul, a, b, c, [mul, x, y, z]] => [mul, a, b, c, x, y, z]
% and arguments are sorted.
% If there is only one argument, we do not need operation:
%   [sum, x] => x
%   [mul, x] => x
rule_normalization([Oper | L], _) when ?IS_MULTINARY(Oper) ->
    {Same_Args, Other_Args} = lists:partition(fun(X) -> is_oper(X, Oper) end, L),
    New_Args =
        lists:foldl
        (
            fun([_ | Args], Cur_Args) ->
                lists:append(Cur_Args, Args)
            end,
            [], Same_Args
        ),
    case lists:append(Other_Args, New_Args) of
        [Single_Arg] ->
            Single_Arg;
        Many_Args ->
            [Oper | lists:sort(Many_Args)]
    end;
rule_normalization(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_calculation(E :: expr()) -> expr().
%% @doc
%% Calculate numeric expressions.
rule_calculation(E) ->
    rule_calculation(E, default_options()).

-spec rule_calculation(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Calculate numeric expressions.
rule_calculation([neg, X], _) when is_number(X) ->
    -X;
rule_calculation([sum | Args], _) ->
    {Numbers, Exprs} = lists:partition(fun(X) -> is_number(X) end, Args),
    Value = lists:sum(Numbers),
    if
        % Only numeric part.
        Exprs =:= [] ->
            Value;

        % No numeric part.
        ?IS_EQ(Value, 0) ->
            [sum | Exprs];

        % Numeric and symbol part.
        true ->
            [sum, Value | Exprs]
    end;

% Sub calculation rules:
%   x - x => 0
%   0 - y = -y
%   x - 0 = x
rule_calculation([sub, X, Y] = E, _) ->
    Is_Eq = is_eq(X, Y),
    if
        Is_Eq ->
            0;
        is_number(X) andalso is_number(Y) ->
            X - Y;
        ?IS_EQ(X, 0) ->
            [neg, Y];
        ?IS_EQ(Y, 0) ->
            X;
        true ->
            E
    end;

% Mul calculation rules:
%   x * 0 = 0 * x => 0
%   x * 1 = 1 * x => x
rule_calculation([mul | Args], _) ->
    {Numbers, Exprs} = lists:partition(fun(X) -> is_number(X) end, Args),
    Value = lists:foldl(fun(X, Cur) -> X * Cur end, 1, Numbers),
    if
        % Only numeric part.
        Exprs =:= [] ->
            Value;

        % Multiplication by zero.
        ?IS_EQ(Value, 0) ->
            0;

        % No numeric part.
        ?IS_EQ(Value, 1) ->
            [mul | Exprs];

        % Numeric and symbol parts.
        true ->
            [mul, Value | Exprs]
    end;

% Dvs calculation rules:
%   0 / 0 => exception
%   x / x => 1 (is_ignore_indef)
%   x / 1 => x
%   x / (-1) => -x
rule_calculation([dvs, X, Y] = E, #{is_calc_frac := Is_Calc_Frac,
                                    is_ignore_indef := Is_Ignore_Indef}) ->
    if
        % We can not process this exception.
        ?IS_EQ(Y, 0) ->
            throw({dbz, E});

        ?IS_EQ(Y, 1) ->
            X;
        ?IS_EQ(Y, -1) ->
            [neg, X];

        % Numbers and Y /= 0.
        % We have a chance to calculate it.
        is_number(X) andalso is_number(Y) ->

            D = X / Y,

            if
                is_float(X) orelse is_float(Y) orelse Is_Calc_Frac ->
                    D;

                % X and Y are integers.
                % But we can try to calculate if result is integer.
                trunc(D) == D ->
                    D;

                true ->
                    E
            end;

        ?IS_EQ(X, 0) andalso Is_Ignore_Indef ->
            0;

        % Can not calculate.
        true ->
            Is_Eq = is_eq(X, Y),

            if
                Is_Eq andalso Is_Ignore_Indef ->
                    1;
                true ->
                    E
            end
    end;

% Pow calculation rules:
%   0^0 => exception
%   0^x => 0 (is_ignore_indef)
%   x^0 => 1 (is_ignore_indef)
%   x^1 => x
rule_calculation([pow, [pow, X, N], M], _) when (is_number(N) andalso is_number(M)) ->
    [pow, X, N * M];
rule_calculation([pow, X, Y] = E, #{is_ignore_indef := Is_Ignore_Indef}) ->
    if
        ?IS_EQ(X, 0) ->
            if
                ?IS_EQ(Y, 0) ->
                    throw({indef, E});
                Is_Ignore_Indef ->
                    0;
                true ->
                    E
            end;
        ?IS_EQ(Y, 0) ->
            if
                Is_Ignore_Indef ->
                    1;
                true ->
                    E
            end;
        ?IS_EQ(Y, 1) ->
            X;
        is_number(X) andalso is_number(Y) ->
            math:pow(X, Y);
        true ->
            E
    end;
rule_calculation(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_collect_items(E :: expr()) -> expr().
%% @doc
%% Collect items for sum and mul operation.
rule_collect_items(E) ->
    rule_collect_items(E, default_options()).

-spec rule_collect_items(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Collect items for sum and mul operation.
rule_collect_items([sum | L], _) ->
    H = jdlib_lists:sorted_histogram(L),
    New_L =
        lists:map
        (
            fun
                ({X, 1}) ->
                    X;
                ({X, C}) ->
                    [mul, C, X]
            end,
            H
        ),
    [sum | New_L];
rule_collect_items([mul | L], _) ->
    H = jdlib_lists:sorted_histogram(L),
    New_L =
        lists:map
        (
            fun
                ({X, 1}) ->
                    X;
                ({X, C}) ->
                    [pow, X, C]
            end,
            H
        ),
    [mul | New_L];
rule_collect_items(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_sum_mul_pairs(E :: expr()) -> expr().
%% @doc
%% Apply rules for sum and mul pairs.
rule_sum_mul_pairs(E) ->
    rule_sum_mul_pairs(E, default_options()).

-spec rule_sum_mul_pairs(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Apply rules for sum and mul pairs.
%% Very limited cases, which allow to apply the following rules:
%%   n * x + m * x => (n + m) * x
%%   (x^n) * (x^m) => x^(n + m)
rule_sum_mul_pairs([sum | L] = E, _) ->
    Sum_Pair_F =
        fun(X, Y) ->
            {XC, XF} = mul_coef(X),
            {YC, YF} = mul_coef(Y),
            Is_Eq = is_eq(XF, YF),
            if
                Is_Eq ->
                    {true, [mul, XC + YC, XF]};
                true ->
                    false
            end
        end,
    case jdlib_lists:apply_to_any_pair(L, Sum_Pair_F) of
        {true, A1, A2, Res} ->
            L1 = lists:delete(A1, L),
            L2 = lists:delete(A2, L1),
            [sum, Res | L2];
        false ->
            E
    end;
rule_sum_mul_pairs([mul | L] = E, _) ->
    Mul_Pair_F =
        fun(X, Y) ->
            {XC, XF} = pow_coef(X),
            {YC, YF} = pow_coef(Y),
            Is_Eq = is_eq(XF, YF),
            if
                Is_Eq ->
                    {true, [pow, XF, XC + YC]};
                true ->
                    false
            end
        end,
    case jdlib_lists:apply_to_any_pair(L, Mul_Pair_F) of
        {true, A1, A2, Res} ->
            L1 = lists:delete(A1, L),
            L2 = lists:delete(A2, L1),
            [mul, Res | L2];
        false ->
            E
    end;
rule_sum_mul_pairs(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_open_brackets(E :: expr()) -> expr().
%% @doc
%% Open brackets in expression.
rule_open_brackets(E) ->
    rule_open_brackets(E, default_options()).

-spec rule_open_brackets(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Open brackets in expression.
%%   -(-x) => x
%%   -(x - y) => y - x
rule_open_brackets([neg, [neg, E]], _) ->
    E;
rule_open_brackets([neg, [sub, X, Y]], _) ->
    [sub, Y, X];

% Sum open brackets rules:
%   x + (a - b) => x + a + (-b)
rule_open_brackets([sum | L], _) ->
    {Sub_Exprs, Exprs} = lists:partition(fun is_sub/1, L),
    New_Args =
        lists:foldl
        (
            fun([sub, X, Y], Cur_Args) ->
                lists:append(Cur_Args, [X, [neg, Y]])
            end,
            [], Sub_Exprs
        ),
    [sum | lists:append(Exprs, New_Args)];

% Sub open brackets rules:
%   x - (-y) => x + y
%   x - (a - b) => (x + b) - a
%   (-x) - y -> =(x + y)
rule_open_brackets([sub, X, [neg, Y]], _) ->
    [sum, X, Y];
rule_open_brackets([sub, X, [sub, A, B]], _) ->
    [sub, [sum, X, B], A];
rule_open_brackets([sub, [neg, X], Y], _) ->
    [neg, [sum, X, Y]];
rule_open_brackets(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_collect_negs(E :: expr()) -> expr().
%% @doc
%% Collect negs.
%%   (-a) * b * (-c) => a * b * c
%%   (-a) * (-b) * (-c) = -(a * b * c)
rule_collect_negs(E) ->
    rule_collect_negs(E, default_options()).

-spec rule_collect_negs(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Collect negs.
rule_collect_negs([mul | Exprs], _) ->
    {New_Exprs, Is_Neg} =
        lists:mapfoldl
        (
            fun
                ([neg, X], Acc) ->
                    {X, Acc xor true};
                (X, Acc) ->
                    {X, Acc}
            end,
            false, Exprs
        ),
    Mul = [mul | New_Exprs],
    if
        Is_Neg ->
            [neg, Mul];
        true ->
            Mul
    end;
rule_collect_negs(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_split_sum(E :: expr()) -> expr().
%% @doc
%% Split sum.
%%   (a + (-b) + c + (-d)) => (a + c) - (b + d).
rule_split_sum(E) ->
    rule_split_sum(E, default_options()).

-spec rule_split_sum(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Split sum.
rule_split_sum([sum | L] = E, _) when (L =/= []) ->
    {Neg, Pos} = lists:partition(fun is_neg/1, L),
    Inv_Neg = lists:map(fun([neg, X]) -> X end, Neg),
    if
        % Only positive members.
        Inv_Neg =:= [] ->
            E;

        % Only negative members.
        Pos =:= [] ->
            [neg, [sum | Inv_Neg]];

        % Positive and negative members.
        true ->
            [sub, [sum | Pos], [sum | Inv_Neg]]
    end;
rule_split_sum(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

