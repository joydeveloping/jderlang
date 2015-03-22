%% @doc
%% Tests for jdlib_comb.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_comb_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Import.
-import(jdlib_comb,
        [c/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec c_test() -> ok.
%% @doc
%% Function c test.
c_test() ->
    ?assertEqual([0, 1, 4, 6, 4, 1, 0],
                 [c(-3, 4), c(0, 4), c(1, 4), c(2, 4), c(3, 4), c(4, 4), c(5, 4)]),
    ?assertEqual([0, 0], [c(-5, 10), c(5, -10)]),

    % 10-th line of Pascal triangle.
    ?assertEqual(jdlib_math:npow(2, 10),
                 lists:sum(lists:map(jdlib_currying:curry_1(jdlib:flip(fun jdlib_comb:c/2), 10),
                           lists:seq(0, 10)))),

    ok.

%---------------------------------------------------------------------------------------------------

