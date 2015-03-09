%% @doc
%% Tests for jdlib_lists.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_lists_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_lists,
        [head/1, tail/1, last/1, init/1,
         is_null/1, take/2, drop/2, product/1, minmax/1, duplicate_list/2,
         foldl_1/2, foldr_1/2, adj_pairs_map/2,
         count/2, sorted_histogram/1, merge_sorted_histograms/2, apply_to_any_pair/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec head_test() -> ok.
%% @doc
%% Function head test.
head_test() ->
    ?assertThrow({badarg, []}, head([])),
    ?assertEqual(1, head([1])),
    ?assertEqual(a, head([a, b])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec tail_test() -> ok.
%% @doc
%% Function tail test.
tail_test() ->
    ?assertThrow({badarg, []}, tail([])),
    ?assertEqual([], tail([1])),
    ?assertEqual([b], tail([a, b])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec last_test() -> ok.
%% @doc
%% Function last test.
last_test() ->
    ?assertThrow({badarg, []}, last([])),
    ?assertEqual(1, last([1])),
    ?assertEqual(b, last([a, b])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec init_test() -> ok.
%% @doc
%% Function init test.
init_test() ->
    ?assertThrow({badarg, []}, init([])),
    ?assertEqual([], init([1])),
    ?assertEqual([a], init([a, b])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec is_null_test() -> ok.
%% @doc
%% Function is_null test.
is_null_test() ->
    ?assertEqual(true, is_null([])),
    ?assertEqual(false, is_null([a, b, c])),
    ?assertThrow({badarg, _}, is_null(5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec take_test() -> ok.
%% @doc

%% Function drop test.
take_test() ->
    ?assertEqual([], take([], 5)),
    ?assertEqual([a, b, c, d], take([a, b, c, d], 5)),
    ?assertEqual([a, b], take([a, b, c, d], 2)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec drop_test() -> ok.
%% @doc
%% Function drop test.
drop_test() ->
    ?assertEqual([], drop([], 5)),
    ?assertEqual([], drop([a, b, c, d], 5)),
    ?assertEqual([c, d], drop([a, b, c, d], 2)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec product_test() -> ok.
%% @doc
%% Function product test.
product_test() ->
    ?assertEqual(1, product([])),
    ?assertEqual(3, product([3])),
    ?assertEqual(24, product([2, 3, 4])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec minmax_test() -> ok.
%% @doc
%% Function minmax test.
minmax_test() ->
    ?assertThrow({badarg, _}, minmax([])),
    ?assertEqual({5, 5}, minmax([5])),
    ?assertEqual({2, 9}, minmax([3, 5, 2, 6, 9])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec duplicate_list_test() -> ok.
%% @doc
%% Function duplicate_list test.
duplicate_list_test() ->
    ?assertEqual([], duplicate_list([a, b, c], 0)),
    ?assertEqual([a, b, c], duplicate_list([a, b, c], 1)),
    ?assertEqual([a, b, c, a, b, c, a, b, c], duplicate_list([a, b, c], 3)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec foldl_1_test() -> ok.
%% @doc
%% Function foldl_1 test.
foldl_1_test() ->
    ?assertThrow({badarg, _}, foldl_1(fun ldlib_math:add/2, [])),
    ?assertEqual(5, foldl_1(fun jdlib_math:add/2, [5])),
    ?assertEqual(11, foldl_1(fun jdlib_math:add/2, [5, 6])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec foldr_1_test() -> ok.
%% @doc
%% Function foldr_1 test.
foldr_1_test() ->
    ?assertThrow({badarg, _}, foldr_1(fun jdlib_math:sub/2, [])),
    ?assertEqual(5, foldr_1(fun jdlib_math:sub/2, [5])),
    ?assertEqual(2, foldr_1(fun jdlib_math:sub/2, [1, 2, 3])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec adj_pairs_map_test() -> ok.
%% @doc
%% Function adj_pairs_map test.
adj_pairs_map_test() ->
    ?assertEqual([], adj_pairs_map([], fun(X, Y) -> X == Y end)),
    ?assertEqual([], adj_pairs_map([], fun(X, Y) -> X + Y end)),
    L = [1, 2, 4, 7, 11],
    ?assertEqual([1, 2, 3, 4], adj_pairs_map(L, fun(X, Y) -> Y - X end)),
    ok.

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

-spec merge_sorted_histograms_test() -> ok.
%% @doc
%% Function merge_sorted_histograms test.
merge_sorted_histograms_test() ->
    ?assertEqual([], merge_sorted_histograms([], [])),
    ?assertEqual([{a, 3}, {b, 1}, {c, 2}], merge_sorted_histograms([{a, 3}, {b, 1}, {c, 2}], [])),
    ?assertEqual([{a, 1}, {b, 2}, {c, 3}], merge_sorted_histograms([], [{a, 1}, {b, 2}, {c, 3}])),
    ?assertEqual([{a, 5}, {b, 1}, {c, 3}, {d, 8}, {f, 4}, {n, 5}, {q, 1}],
                 merge_sorted_histograms([{c, 2}, {f, 4}, {n, 2}],
                                         [{a, 5}, {b, 1}, {c, 1}, {d, 8}, {n, 3}, {q, 1}])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec apply_to_any_pair_test() -> ok.
%% @doc
%% Function apply_to_any_pair test.
apply_to_any_pair_test() ->
    ?assertEqual(false, apply_to_any_pair([], fun(_, _) -> {true, 0} end)),
    ?assertEqual(false, apply_to_any_pair([x, y, 1, 2], fun(_, _) -> false end)),
    ?assertEqual({true, 3, 4, 7}, apply_to_any_pair([3, 4, 5], fun(X, Y) -> {true, X + Y} end)),
    ?assertEqual({true, w, w, true}, apply_to_any_pair([1, 2, 3, a, b, c, 4, 5, w, x, y, w],
                                   fun(X, Y) -> if X =:= Y -> {true, true}; true -> false end end)),
    ok.

%---------------------------------------------------------------------------------------------------

