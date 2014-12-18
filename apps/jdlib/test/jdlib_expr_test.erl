%% @doc
%% Tests for jdlib_expr.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_expr_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_expr,
        [is_eq/2, is_const/1, substitute/3,
         rule_normalization/1, rule_normalization/2,
         rule_calculation/1, rule_calculation/2,
         neg/1, sum/2, sub/2, mul/2, dvs/2,
         simplify/1, simplify/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec is_eq_test() -> ok.
%% @doc
%% Function is_eq test.
is_eq_test() ->
    ?assert(is_eq(x, x)),
    ?assert(is_eq(1, 1.0)),
    ?assert(is_eq({sum, [y, 5]}, {sum, [y, 5.0]})),
    ok.

%---------------------------------------------------------------------------------------------------

-spec is_const_test() -> ok.
%% @doc
%% Function is_const test.
is_const_test() ->
    ?assert(is_const(1)),
    ?assert(not is_const(x)),
    ?assert(is_const({sum, [{neg, 3}, {dvs, {1, 1}}]})),
    ?assert(not is_const({sub, {{sum, [1, 2, 3]}, {mul, [1, x, 2]}}})),
    ok.

%---------------------------------------------------------------------------------------------------

-spec substitute_test() -> ok.
%% @doc
%% Function substitute test.
substitute_test() ->
    ?assertEqual({sum,
                  [{neg, {dvs, {v, w}}}, {sub, {a, {dvs, {v, w}}}}, {mul, [1, {dvs, {v, w}}, b]}]},
                 substitute({sum, [{neg, x}, {sub, {a, x}}, {mul, [1, x, b]}]}, x, {dvs, {v, w}})),
    ok.

%---------------------------------------------------------------------------------------------------

-spec rule_normalization_test() -> ok.
%% @doc
%% Rule normalization test.
rule_normalization_test() ->

    % Float normalization.
    ?assertEqual(5, rule_normalization(5.0)),

    % Multinary operation normalization.
    ?assertEqual({sum, [a, b, c, x, y, z]},
                 rule_normalization({sum, [a, b, c, {sum, [x, y, z]}]})),
    ?assertEqual({sum, [a, b, c, x, y, z]},
                 rule_normalization({sum, [c, {sum, [z, y, x]}, b, a]})),
    ?assertEqual({mul, [a, b, c, x, y, z]},
                 rule_normalization({mul, [c, {mul, [z, y, x]}, b, a]})),
    ?assertEqual({sum, [a, b, c, d, e, f, g]},
                 rule_normalization({sum, [b, e, g, a, d, f, c]})),

    ok.

%---------------------------------------------------------------------------------------------------

-spec rule_calculation_test() -> ok.
%% @doc
%% Rule calculation test.
rule_calculation_test() ->

    % Neg.
    ?assertEqual(-5, rule_calculation({neg, 5})),
    ?assertEqual(5, rule_calculation({neg, -5})),
    ?assertEqual({neg, x}, rule_calculation({neg, x})),

    % Sum.
    ?assertEqual(10, rule_calculation({sum, [2, 3, 1, 4]})),
    ?assertEqual(0, rule_calculation({sum, [2, -2]})),
    ?assertEqual({sum, [x]}, rule_calculation({sum, [-2, x, 2]})),

    % Sub.
    ?assertEqual(4, rule_calculation({sub, {10, 6}})),
    ?assertEqual({sub, {a, b}}, rule_calculation({sub, {a, b}})),
    ?assertEqual(0, rule_calculation({sub, {x, x}})),

    % Mul.
    ?assertEqual(24, rule_calculation({mul, [2, 3, 1, 4]})),
    ?assertEqual(0, rule_calculation({mul, [2, 3, 0, 4]})),
    ?assertEqual(0, rule_calculation({mul, [x, y, 0, z]})),

    % Dvs.
    ?assertEqual({dvs, {2, 4}}, rule_calculation({dvs, {2, 4}},
                                                 #{is_calc_frac => false, is_ignore_dbz => true})),
    ?assert(is_eq(0.5, rule_calculation({dvs, {2, 4}},
                                        #{is_calc_frac => true, is_ignore_dbz => true}))),
    ?assert(is_eq(2, rule_calculation({dvs, {4, 2}},
                                      #{is_calc_frac => false, is_ignore_dbz => true}))),
    ?assertThrow({dbz, _}, rule_calculation({dvs, {1, 0}})),
    ?assertThrow({dbz, _}, rule_calculation({dvs, {0, 0}})),
    ?assertThrow({dbz, _}, rule_calculation({dvs, {x, 0}})),
    ?assertEqual(1, rule_calculation({dvs, {x, x}},
                                     #{is_calc_frac => true, is_ignore_dbz => true})),
    ?assertEqual({dvs, {x, x}}, rule_calculation({dvs, {x, x}},
                                                 #{is_calc_frac => true, is_ignore_dbz => false})),
    ?assertEqual(0, rule_calculation({dvs, {0, x}},
                                     #{is_calc_frac => true, is_ignore_dbz => true})),
    ?assertEqual({dvs, {0, x}}, rule_calculation({dvs, {0, x}},
                                                 #{is_calc_frac => true, is_ignore_dbz => false})),

    ok.

%---------------------------------------------------------------------------------------------------

-spec neg_test() -> ok.
%% @doc
%% Function neg test.
neg_test() ->
    ?assertEqual(-5, neg(5)),
    ?assertEqual(5, neg(-5)),
    ?assertEqual({neg, x}, neg(x)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sum_test() -> ok.
%% @doc
%% Function sum test.
sum_test() ->
    ?assertEqual({sum, [x, y]}, sum(x, y)),
    ?assertEqual(x, sum(x, 0)),
    ?assertEqual(x, sum(0, x)),
    ?assertEqual(3, sum(1, 2)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sub_test() -> ok.
%% @doc
%% Function sub test.
sub_test() ->
    ?assertEqual({sub, {x, y}}, sub(x, y)),
    ?assertEqual({neg, x}, sub(0, x)),
    ?assertEqual(x, sub(x, 0)),
    ?assertEqual(0, sub(x, x)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec mul_test() -> ok.
%% @doc
%% Function mul test.
mul_test() ->
    ?assertEqual({mul, [x, y]}, mul(x, y)),
    ?assertEqual(x, mul(x, 1)),
    ?assertEqual(x, mul(1, x)),
    ?assertEqual(0, mul(0, x)),
    ?assertEqual(0, mul(x, 0)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec dvs_test() -> ok.
%% @doc
%% Function dvs test.
dvs_test() ->
    ?assertEqual(5, dvs(10, 2)),
    ?assertEqual(1, dvs(x, x)),
    ?assertEqual(0, dvs(0, x)),
    ?assertThrow({dbz, _}, dvs(x, 0)),
    ok.

%---------------------------------------------------------------------------------------------------
% Simplify function test.
%---------------------------------------------------------------------------------------------------

-spec simplify_test() -> ok.
%% @doc
%% Function simplify test.
simplify_test() ->

    % Simplify sum.
    % sum(2, x, -2) -> sum(x) -> x
    ?assertEqual(x, simplify({sum, [2, x, -2]})),

    % Simplify mul.
    % mul(2, x, 0.5) -> mul(x) -> x
    ?assertEqual(x, simplify({mul, [2, x, 0.5]})),

    ok.

%---------------------------------------------------------------------------------------------------

