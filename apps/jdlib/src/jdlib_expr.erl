%% @doc
%% Mathematical expressions manipulation.
%%
%% <ul>
%% <b>The following operations is supported:</b>
%%   <li><tt>neg</tt> - negate (<tt>x</tt> -> <tt>-x</tt>)</li>
%%   <li><tt>inv</tt> - invert (<tt>x</tt> -> <tt>1 / x</tt>)</li>
%%   <li><tt>sum</tt> - sum of expressions list (<tt>x + y + ...</tt>)</li>
%%   <li><tt>sub</tt> - subtraction of two values (<tt>x - y</li>)</li>
%%   <li><tt>mul</tt> - multiplication of expressions list (<tt>x * y * ...</tt>)</li>
%%   <li><tt>dvs</tt> - division operation(<tt>x / y</tt>)</li>
%% </ul
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_expr).

% Export.
-export([neg/1, inv/1, sum/2, sub/2, mul/2, dvs/2,
         is_eq/2,
         simplify/1,
         rule_normalization/2, rule_calculation/2]).

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

% Check if expression is trivial.
-define(IS_TRIVIAL(X), (is_integer(X) orelse is_atom(X))).

% Check if term is expression.
% It is macros for use in guard expressions.
% It is just light check, even if ?IS_EXPR(X) =:= true X may be not valid expression.
-define(IS_EXPR(X), (?IS_TRIVIAL(X) orelse is_float(X) orelse is_tuple(X))).

% Check if oper unary.
-define(IS_UNARY(OPER), ((OPER =:= neg) orelse (OPER =:= inv))).

% Check if oper binary.
-define(IS_BINARY(OPER), ((OPER =:= sub) orelse (OPER =:= dvs))).

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
-type unary_operation() :: neg | inv.

% Binary operation.
-type binary_operation() :: sub | dvs.

% Operation with many arguments.
-type multinary_operation() :: sum | mul.

% Operation.
-type operation() :: unary_operation() | binary_operation() | multinary_operation().

% Expression.
-type expr() :: number() | atom()
                | {unary_operation(), expr()}
                | {binary_operation(), {expr(), expr()}}
                | {multinary_operation(), [expr()]}.

% Options.
-type options() :: #{}.

%---------------------------------------------------------------------------------------------------
% Contructors.
%---------------------------------------------------------------------------------------------------

-spec neg(X :: expr()) -> expr().
%% @doc
%% Negate constructor.
neg(X) when ?IS_EXPR(X) ->
    simplify({neg, X}).

%---------------------------------------------------------------------------------------------------

-spec inv(X :: expr()) -> expr().
%% @doc
%% Invert constructor.
inv(X) when ?IS_EXPR(X) ->
    simplify({inv, X}).

%---------------------------------------------------------------------------------------------------

-spec sum(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Sum constructor.
sum(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify({sum, [X, Y]}).

%---------------------------------------------------------------------------------------------------

-spec sub(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Sub constructor.
sub(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify({sub, {X, Y}}).

%---------------------------------------------------------------------------------------------------

-spec mul(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Mul constructor.
mul(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify({mul, [X, Y]}).

%---------------------------------------------------------------------------------------------------

-spec dvs(X :: expr(), Y :: expr()) -> expr().
%% @doc
%% Division constructor.
dvs(X, Y) when (?IS_EXPR(X) andalso ?IS_EXPR(Y)) ->
    simplify({dvs, {X, Y}}).

%---------------------------------------------------------------------------------------------------
% Check equality.
%---------------------------------------------------------------------------------------------------

-spec is_eq(E1 :: expr(), E2 :: expr()) -> boolean().
%% @doc
%% Check equality.
%% Warning!
%% E1 is equal to E2 without simplification effect.
%% So {sum, [a, b]} is not equal to {sum, [b, a]},
%% but {neg, 1} is equal to {neg, 1.0}.
is_eq(E1, E2) when (is_number(E1) andalso is_number(E2)) ->
    ?IS_EQ(E1, E2);
is_eq({Oper, X1}, {Oper, X2}) when ?IS_UNARY(Oper) ->
    is_eq(X1, X2);
is_eq({Oper, {X1, Y1}}, {Oper, {X2, Y2}}) when ?IS_BINARY(Oper) ->
    is_eq(X1, X2) andalso is_eq(Y1, Y2);
is_eq({Oper, L1}, {Oper, L2}) when (?IS_MULTINARY(Oper)
                                    andalso (length(L1) =:= length(L2))) ->
    lists:all(fun({X1, X2}) -> is_eq(X1, X2) end, lists:zip(L1, L2));
is_eq(_, _) ->
    false.

%---------------------------------------------------------------------------------------------------
% Simplification functions.
%---------------------------------------------------------------------------------------------------

-spec simplify(E :: expr()) -> expr().
%% @doc
%% Expression simplification.
simplify(E) ->
    simplify
    (
        E,

        % Options.
        #{
            % Calculate fractions.
            % It can lead to loss of precision, for example in expression 1 / 3.
            is_calc_frac => true,

            % Ignore division by zero exception.
            % It allows us to apply such optimizations as x / x = 1.
            is_ignore_dbz => true
        }
    ).

-spec simplify(E :: expr(), Opts :: options()) -> expr().
%% @private
%% @doc
%% Expression simplification with given options.
simplify(E, _) when ?IS_TRIVIAL(E) ->
    E;
simplify(E, Opts) when is_float(E) ->
    rules(E, Opts);
simplify({Oper, Args} = E, Opts) ->

    % First try to apply simplification to nested expressions.
    Simp_Args =
        case Args of
            L when is_list(L) ->
                lists:map(fun(X) -> simplify(X, Opts) end, L);
            {X1, X2} ->
                {simplify(X1, Opts), simplify(X2, Opts)};
            X ->
                simplify(X, Opts)
        end,

    % Then try to simplify whole expression while it is possible.
    case rules({Oper, Simp_Args}, Opts) of

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
            rule_calculation
        ],
    lists:foldl(fun(Rule, Cur_E) -> apply(jdlib_expr, Rule, [Cur_E, Opts]) end, E, Rules).

%---------------------------------------------------------------------------------------------------

-spec rule_normalization(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Normalization rule.
%% For float values drops zero fraction:
%%   N.0 -> N
rule_normalization(E, _) when is_float(E) ->
    N = trunc(E),
    if

        % Not "=:="!
        E == N ->
            N;

        true ->
            E
    end;

% Multinary operations are to flatten:
%   {sum, [a, b, c, {sum, [x, y, z]}]} -> {sum, [a, b, c, x, y, z]},
%   {mul, [a, b, c, {sum, [x, y, z]}]} -> {mul, [a, b, c, x, y, z]},
% and arguments are sorted.
rule_normalization({Oper, L}, _) when ?IS_MULTINARY(Oper) ->
    {Same_Oper_Exprs, Other_Opers_Exprs} =
        lists:partition
        (
            fun(X) ->
                case X of
                    {Oper, _} ->
                        true;
                    _ ->
                        false
                end
            end,
            L
        ),
    New_Args =
        lists:foldl
        (
            fun({_, Args}, Cur_Args) ->
                lists:append(Cur_Args, Args)
            end,
            [], Same_Oper_Exprs
        ),
    {Oper, lists:sort(lists:append(Other_Opers_Exprs, New_Args))};
rule_normalization(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

-spec rule_calculation(E :: expr(), Opts :: options()) -> expr().
%% @doc
%% Calculate numeric expressions.
rule_calculation({neg, X}, _) when is_number(X) ->
    -X;
rule_calculation({inv, X} = E, #{is_calc_frac := Is_Calc_Frac}) when is_number(X) ->
    if
        % Exact value.
        X =:= 1 ->
            1;
        X =:= -1 ->
            -1;

        % Division by zero.
        X =:= 0 ->
            throw({dbz, 0});

        % Calculate fraction.
        Is_Calc_Frac orelse is_float(X) ->
            1 / X;

        % Do not calculate.
        true ->
            E
    end;
rule_calculation({sum, Args}, _) ->
    {Numbers, Exprs} = lists:partition(fun(X) -> is_number(X) end, Args),
    Value = lists:sum(Numbers),
    if
        % Only numeric members.
        Exprs =:= [] ->
            Value;

        % There are non numeric members and value.
        Value /= 0 ->
            {sum, lists:sort([Value | Exprs])};

        % Now we know Value == 0.
        % Check one case when list of expressions is single expression.
        length(Exprs) =:= 1 ->
            [H] = Exprs,
            H;

        true ->
            {sum, lists:sort(Exprs)}
    end;
rule_calculation({sub, Args}, _) ->
    % TODO
    {sub, Args};
rule_calculation({mul, Args}, _) ->
    {Numbers, Exprs} = lists:partition(fun(X) -> is_number(X) end, Args),
    Value = lists:foldl(fun(X, Cur) -> X * Cur end, 1, Numbers),
    if
        % Only numeric members.
        Exprs =:= [] ->
            Value;

        % There are non numeric members and value.
        Value /= 1 ->
            {mul, lists:sort([Value | Exprs])};

        % TODO:
        % We have to handle special case Value == 0.

        % Now we know Value == 1.
        % Check one case when list of expressions is single expression.
        length(Exprs) =:= 1 ->
            [H] = Exprs,
            H;

        true ->
            {mul, lists:sort(Exprs)}
    end;
rule_calculation({dvs, Args}, _) ->
    % TODO
    {dvs, Args};
rule_calculation(E, _) ->
    E.

%---------------------------------------------------------------------------------------------------

