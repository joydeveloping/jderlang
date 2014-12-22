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
         fraction/1]).

%---------------------------------------------------------------------------------------------------
% Tests.
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

