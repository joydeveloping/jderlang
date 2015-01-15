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
        [head/1, tail/1, last/1, init/1,
         count/2, sorted_histogram/1, merge_sorted_histograms/2, apply_to_any_pair/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec head_test() -> ok.
%% @doc
%% Function head test.
head_test() ->
    ?assertThrow({badarg, []}, head([])),
    ?assertEqual(head([1]), 1),
    ?assertEqual(head([a, b]), a),
    ok.

%---------------------------------------------------------------------------------------------------

-spec tail_test() -> ok.
%% @doc
%% Function tail test.
tail_test() ->
    ?assertThrow({badarg, []}, tail([])),
    ?assertEqual(tail([1]), []),
    ?assertEqual(tail([a, b]), [b]),
    ok.

%---------------------------------------------------------------------------------------------------

-spec last_test() -> ok.
%% @doc
%% Function last test.
last_test() ->
    ?assertThrow({badarg, []}, last([])),
    ?assertEqual(last([1]), 1),
    ?assertEqual(last([a, b]), b),
    ok.

%---------------------------------------------------------------------------------------------------

-spec init_test() -> ok.
%% @doc
%% Function init test.
init_test() ->
    ?assertThrow({badarg, []}, init([])),
    ?assertEqual(init([1]), []),
    ?assertEqual(init([a, b]), [a]),
    ok.

%---------------------------------------------------------------------------------------------------

-spec count_test() -> ok.
%% @doc
%% Function count test.
count_test() ->
    L = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    ?assertEqual(count([], fun is_number/1), 0),
    ?assertEqual(count(L, fun(X) -> X < 7.5 end), 7),
    ?assertEqual(count(L, fun(X) -> X rem 2 =:= 0 end), 5),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sorted_histogram_test() -> ok.
%% @doc
%% Function sorted_histogram test.
sorted_histogram_test() ->
    ?assertEqual(sorted_histogram([]), []),
    ?assertEqual(sorted_histogram([1, 2, 3]), [{1, 1}, {2, 1}, {3, 1}]),
    ?assertEqual(sorted_histogram([1, 2, 3, 1, 2, 3]), [{1, 2}, {2, 2}, {3, 2}]),
    ?assertEqual(sorted_histogram([2, 2, 2, 1, 1, 1]), [{1, 3}, {2, 3}]),
    ok.

%---------------------------------------------------------------------------------------------------

-spec merge_sorted_histograms_test() -> ok.
%% @doc
%% Function merge_sorted_histograms test.
merge_sorted_histograms_test() ->
    ?assertEqual(merge_sorted_histograms([], []), []),
    ?assertEqual(merge_sorted_histograms([{a, 3}, {b, 1}, {c, 2}], []), [{a, 3}, {b, 1}, {c, 2}]),
    ?assertEqual(merge_sorted_histograms([], [{a, 1}, {b, 2}, {c, 3}]), [{a, 1}, {b, 2}, {c, 3}]),
    ?assertEqual(merge_sorted_histograms([{c, 2}, {f, 4}, {n, 2}],
                                         [{a, 5}, {b, 1}, {c, 1}, {d, 8}, {n, 3}, {q, 1}]),
                 [{a, 5}, {b, 1}, {c, 3}, {d, 8}, {f, 4}, {n, 5}, {q, 1}]),
    ok.

%---------------------------------------------------------------------------------------------------

-spec apply_to_any_pair_test() -> ok.
%% @doc
%% Function apply_to_any_pair test.
apply_to_any_pair_test() ->
    ?assertEqual(apply_to_any_pair([], fun(_, _) -> {true, 0} end), false),
    ?assertEqual(apply_to_any_pair([x, y, 1, 2], fun(_, _) -> false end), false),
    ?assertEqual(apply_to_any_pair([3, 4, 5], fun(X, Y) -> {true, X + Y} end), {true, 3, 4, 7}),
    ?assertEqual(apply_to_any_pair([1, 2, 3, a, b, c, 4, 5, w, x, y, w],
                                   fun(X, Y) -> if X =:= Y -> {true, true}; true -> false end end),
                 {true, w, w, true}),
    ok.

%---------------------------------------------------------------------------------------------------

