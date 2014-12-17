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
        [neg/1, inv/1, sum/2, sub/2, mul/2, dvs/2,
         is_eq/2,
         rule_normalization/2, rule_calculation/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec rule_normalization_test() -> ok.
%% @doc
%% Rule normalization test.
rule_normalization_test() ->

    % Float normalization.
    ?assertEqual(5, rule_normalization(5.0, #{})),

    % Multinary operation normalization.
    ?assertEqual({sum, [a, b, c, x, y, z]},
                 rule_normalization({sum, [a, b, c, {sum, [x, y, z]}]}, #{})),
    ?assertEqual({sum, [a, b, c, x, y, z]},
                 rule_normalization({sum, [c, {sum, [z, y, x]}, b, a]}, #{})),
    ?assertEqual({mul, [a, b, c, x, y, z]},
                 rule_normalization({mul, [c, {mul, [z, y, x]}, b, a]}, #{})),
    ?assertEqual({sum, [a, b, c, d, e, f, g]},
                 rule_normalization({sum, [b, e, g, a, d, f, c]}, #{})),

    ok.

%---------------------------------------------------------------------------------------------------

-spec rule_calculation_test() -> ok.
%% @doc
%% Rule calculation test.
rule_calculation_test() ->

    % Neg.
    ?assertEqual(-5, rule_calculation({neg, 5}, #{})),
    ?assertEqual(5, rule_calculation({neg, -5}, #{})),
    ?assertEqual({neg, x}, rule_calculation({neg, x}, #{})),

    % Inv.
    ?assertEqual(1, rule_calculation({inv, 1}, #{is_calc_frac => true})),
    ?assertEqual(-1, rule_calculation({inv, -1}, #{is_calc_frac => true})),
    ?assertThrow({dbz, _}, rule_calculation({inv, 0}, #{is_calc_frac => true})),
    ?assert(is_eq(rule_calculation({inv, 3}, #{is_calc_frac => true}), 1.0 / 3.0)),
    ?assertEqual(1, rule_calculation({inv, 1}, #{is_calc_frac => false})),
    ?assertEqual(-1, rule_calculation({inv, -1}, #{is_calc_frac => false})),
    ?assertThrow({dbz, _}, rule_calculation({inv, 0}, #{is_calc_frac => false})),
    ?assertEqual({inv, 3}, rule_calculation({inv, 3}, #{is_calc_frac => false})),
    ?assertEqual({inv, x}, rule_calculation({inv, x}, #{})),

    % Sum.
    ?assertEqual(10, rule_calculation({sum, [2, 3, 1, 4]}, #{})),
    ?assertEqual(0, rule_calculation({sum, [2, -2]}, #{})),
    ?assertEqual(x, rule_calculation({sum, [2, x, -2]}, #{})),

    % Sub.

    % Mul.
    ?assertEqual(24, rule_calculation({mul, [2, 3, 1, 4]}, #{})),
    ?assertEqual(x, rule_calculation({mul, [2, x, 0.5]}, #{})),

    % Dvs.

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

-spec inv_test() -> ok.
%% @doc
%% Function inv test.
inv_test() ->
    ?assertEqual(1, inv(1)),
    ?assertEqual(-1, inv(-1)),
    ?assertThrow({dbz, _}, inv(0)),
    ?assertEqual(1, inv(1.0)),
    ?assertEqual(-1, inv(-1.0)),
    ?assertThrow({dbz, _}, inv(0.0)),
    ?assertEqual({inv, x}, inv(x)),
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
    ok.

%---------------------------------------------------------------------------------------------------

-spec mul_test() -> ok.
%% @doc
%% Function mul test.
mul_test() ->
    ok.

%---------------------------------------------------------------------------------------------------

-spec dvs_test() -> ok.
%% @doc
%% Function dvs test.
dvs_test() ->
    ok.

%---------------------------------------------------------------------------------------------------

