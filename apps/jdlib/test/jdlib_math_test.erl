%% @doc
%% Tests for jdlib_math.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_math_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_math,
        [is_lt/3, is_lt/2, is_gt/3, is_gt/2, is_eq/3, is_eq/2, is_le/3, is_le/2, is_ge/3, is_ge/2,
         add/2, sub/2, mul/2, dvs/2, npow/2, polynomial/2,
         fraction/1,
         solve_linear_equation/1, solve_linear_inequation/1,
         solve_square_equation/1, solve_square_inequation/1]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec operations_test() -> ok.
%% @doc
%% Functions add, sub, mul, dvs, polynomial test.
operations_test() ->
    ?assertEqual(10, add(3, 7)),
    ?assertEqual(3, sub(10, 7)),
    ?assertEqual(24, mul(6, 4)),
    ?assert(is_eq(4, dvs(24, 6))),
    ?assertEqual(0, polynomial([], 100)),
    ?assertEqual(5, polynomial([5], 100)),
    ?assertEqual(24, polynomial([2, 3, 4], 2)),
    ?assertThrow({badarg, _}, npow(5, 4.5)),
    ?assertEqual([8, 1, 4, 1, 0], [npow(2, 3), npow(1, 5), npow(4, 1), npow(4, 0), npow(0, 0)]),
    ok.

%---------------------------------------------------------------------------------------------------

-spec fraction_test() -> ok.
%% @doc
%% Function fraction test.
fraction_test() ->
    ?assert(is_eq(fraction(1.3), 0.3)),
    ?assert(is_eq(fraction(1.0), 0)),
    ?assert(is_eq(fraction(1), 0)),
    ?assert(is_eq(fraction(-1.3), -0.3)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec equation_inequation_test() -> ok.
%% @doc
%% Tests for linear and square equation and inequation solve.
equation_inequation_test() ->
    ?assert(jdlib_realline:is_eq(solve_linear_equation({0, 3}), empty)),
    ?assert(jdlib_realline:is_eq(solve_linear_equation({0, 0}), line)),
    ?assert(jdlib_realline:is_eq(solve_linear_equation({2, -4}), jdlib_realline:point(2))),
    ?assert(jdlib_realline:is_eq(solve_linear_inequation({0, 3}), line)),
    ?assert(jdlib_realline:is_eq(solve_linear_inequation({0, -3}), empty)),
    ?assert(jdlib_realline:is_eq(solve_linear_inequation({2, -4}),
                                 jdlib_realline:ray(2, true, pos))),
    ?assert(jdlib_realline:is_eq(solve_linear_inequation({-2, 4}),
                                 jdlib_realline:ray(2, true, neg))),
    ?assert(jdlib_realline:is_eq(solve_square_equation({1, 0, 1}), empty)),
    ?assert(jdlib_realline:is_eq(solve_square_equation({1, -2, 1}), jdlib_realline:point(1))),
    ?assert(jdlib_realline:is_eq(solve_square_equation({2, 4, 0}),
            [jdlib_realline:point(-2), jdlib_realline:point(0)])),
    ?assert(jdlib_realline:is_eq(solve_square_inequation({1, 0, 1}), line)),
    ?assert(jdlib_realline:is_eq(solve_square_inequation({-1, 0, -1}), empty)),
    ?assert(jdlib_realline:is_eq(solve_square_inequation({1, -2, 1}), line)),
    ?assert(jdlib_realline:is_eq(solve_square_inequation({-1, 2, -1}), jdlib_realline:point(1))),
    ?assert(jdlib_realline:is_eq(solve_square_inequation({2, 4, 0}),
            [jdlib_realline:ray(-2, true, neg), jdlib_realline:ray(0, true, pos)])),
    ?assert(jdlib_realline:is_eq(solve_square_inequation({-2, -4, 0}),
            jdlib_realline:interval(-2, true, 0, true))),
    ok.

%---------------------------------------------------------------------------------------------------

