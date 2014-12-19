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
        [count/2, sorted_histogram/1]).

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

