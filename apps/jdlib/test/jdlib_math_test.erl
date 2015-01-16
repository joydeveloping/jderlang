%% @doc
%% Tests for jdlib_math.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_math_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_math,
        [is_lt/2, is_gt/2, is_eq/2, is_le/2, is_ge/2,
         add/2, sub/2, mul/2, dvs/2,
         fraction/1]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec add_test() -> ok.
%% @doc
%% Function add test.
add_test() ->
    ?assertEqual(10, add(3, 7)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sub_test() -> ok.
%% @doc
%% Function sub test.
sub_test() ->
    ?assertEqual(3, sub(10, 7)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec mul_test() -> ok.
%% @doc
%% Function mul test.
mul_test() ->
    ?assertEqual(24, mul(6, 4)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec dvs_test() -> ok.
%% @doc
%% Function dvs test.
dvs_test() ->
    ?assert(is_eq(4, dvs(24, 6))),
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

