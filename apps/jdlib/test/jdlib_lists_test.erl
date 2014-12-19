%% @doc
%% Tests for jdlib_lists.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_lists_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_lists,
        [count/2, sorted_histogram/1, apply_to_any_pair/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec count_test() -> ok.
%% @doc
%% Function count test.
count_test() ->
    L = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    ?assertEqual(0, count([], fun is_number/1)),
    ?assertEqual(7, count(L, fun(X) -> X < 7.5 end)),
    ?assertEqual(5, count(L, fun(X) -> X rem 2 =:= 0 end)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sorted_histogram_test() -> ok.
%% @doc
%% Function sorted_histogram test.
sorted_histogram_test() ->
    ?assertEqual([], sorted_histogram([])),
    ?assertEqual([{1, 1}, {2, 1}, {3, 1}], sorted_histogram([1, 2, 3])),
    ?assertEqual([{1, 2}, {2, 2}, {3, 2}], sorted_histogram([1, 2, 3, 1, 2, 3])),
    ?assertEqual([{1, 3}, {2, 3}], sorted_histogram([2, 2, 2, 1, 1, 1])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec apply_to_any_pair_test() -> ok.
%% @doc
%% Function apply_to_any_pair test.
apply_to_any_pair_test() ->
    ?assertEqual(false, apply_to_any_pair([], fun(_, _) -> {true, 0} end)),
    ?assertEqual(false, apply_to_any_pair([x, y, 1, 2], fun(_, _) -> false end)),
    ?assertEqual({true, 3, 4, 7}, apply_to_any_pair([3, 4, 5], fun(X, Y) -> {true, X + Y} end)),
    ?assertEqual({true, w, w, true},
                 apply_to_any_pair([1, 2, 3, a, b, c, 4, 5, w, x, y, w],
                                   fun(X, Y) -> if X =:= Y -> {true, true}; true -> false end end)),
    ok.

%---------------------------------------------------------------------------------------------------

