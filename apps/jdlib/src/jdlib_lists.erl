%% @doc
%% Some additional lists functions realization.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_lists).

% Export.
-export([head/1, tail/1, last/1, init/1,
         is_null/1, take/2, drop/2, duplicate_list/2,
         foldl_1/2, foldr_1/2, adj_pairs_map/2,
         count/2, sorted_histogram/1, merge_sorted_histograms/2, apply_to_any_pair/2,
         product/1, minmax/1, add/2, sub/2, neg/1, mul/2, square/1, dvs/2, inv/1, log/1]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([sorted_histogram/0]).

% List of numbers.
-type nlist() :: [number()].

% Sorted histogram.
-type sorted_histogram() :: [{term(), integer()}].

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

-spec head(L :: list()) -> term().
%% @doc
%% Head of list.
head([]) ->
    throw({badarg, []});
head([H | _]) ->
    H.

%---------------------------------------------------------------------------------------------------

-spec tail(L :: list()) -> list().
%% @doc
%% Tail of list.
tail([]) ->
    throw({badarg, []});
tail([_ | T]) ->
    T.

%---------------------------------------------------------------------------------------------------

-spec last(L :: list()) -> term().
%% @doc
%% Last element of a list.
last([]) ->
    throw({badarg, []});
last([E]) ->
    E;
last([_ | T]) ->
    last(T).

%---------------------------------------------------------------------------------------------------

-spec init(L :: list()) -> list().
%% @doc
%% Initial list (list without last element).
init([]) ->
    throw({badarg, []});
init(L) ->
    lists:reverse(tail(lists:reverse(L))).

%---------------------------------------------------------------------------------------------------

-spec is_null(L :: list()) -> boolean().
%% @doc
%% Check if list is empty.
is_null([]) ->
    true;
is_null(L) when is_list(L) ->
    false;
is_null(T) ->
    throw({badarg, T}).

%---------------------------------------------------------------------------------------------------

-spec take(L :: list(), N :: integer()) -> list().
%% @doc
%% Take first elements of list.
%% If list contains less elements than needed returns whole list.
take(L, N) when (length(L) =< N) ->
    L;
take(L, N) ->
    {L1, _} = lists:split(N, L),
    L1.

%---------------------------------------------------------------------------------------------------

-spec drop(L :: list(), N :: integer()) -> list().
%% @doc
%% Drop first elements from given list.
%% If count of elements to drop is bigger than list length returns empty list.
drop(L, N) when (length(L) =< N) ->
    [];
drop(L, N) ->
    {_, L2} = lists:split(N, L),
    L2.

%---------------------------------------------------------------------------------------------------

-spec duplicate_list(L :: list(), N :: integer()) -> list().
%% @doc
%% Duplicate given list.
duplicate_list(L, N) ->
    lists:append(lists:duplicate(N, L)).

%---------------------------------------------------------------------------------------------------

-spec foldl_1(fun((term(), term()) -> term()), list()) -> term().
%% @doc
%% Function foldl with Acc0 is head of list.
foldl_1(_, []) ->
    throw({badarg, []});
foldl_1(F, [H | T]) ->
    lists:foldl(F, H, T).

%---------------------------------------------------------------------------------------------------

-spec foldr_1(fun((term(), term()) -> term()), list()) -> term().
%% @doc
%% Function foldr with Acc0 is head of list.
foldr_1(_, []) ->
    throw({badarg, []});
foldr_1(F, L) ->
    lists:foldr(F, last(L), init(L)).

%---------------------------------------------------------------------------------------------------

-spec adj_pairs_map(L, fun((E, E) -> Res_E)) -> Res_L
      when L :: [E], Res_L :: [Res_E], E :: term(), Res_E :: term().
%% @doc
%% Applies function to every pair of adjacent elements of the list
%% and returns list of results.
adj_pairs_map(L, _) when (length(L) < 2) ->
    [];
adj_pairs_map(L, F) ->
    adj_pairs_map(L, F, []).

-spec adj_pairs_map(L, fun((E, E) -> Res_E), Res_L) -> Res_L
      when L :: [E], Res_L :: [Res_E], E :: term(), Res_E :: term().
%% @private
%% @doc
%% Applies function to every pair of adjacent elements of the list
%% and returns list of results.
adj_pairs_map([_], _, R) ->
    lists:reverse(R);
adj_pairs_map([H1 | [H2 | _] = T], F, R) ->
    adj_pairs_map(T, F, [F(H1, H2) | R]).

%---------------------------------------------------------------------------------------------------

-spec count(L :: list(), Predicate :: fun((term()) -> boolean())) -> integer().
%% @doc
%% Count of Predicate(X) = true elements.
count(L, Predicate) ->
    count(L, Predicate, 0).

-spec count(L :: list(), Predicate :: fun((term()) -> boolean()), I :: integer()) -> integer().
%% @private
%% @doc
%% Count of Predicate(X) = true elements.
count([], _, I) ->
    I;
count([H | T], Predicate, I) ->
    Is = Predicate(H),
    count(T, Predicate, if Is -> I + 1; true -> I end).

%---------------------------------------------------------------------------------------------------

-spec sorted_histogram(L :: list()) -> sorted_histogram().
%% @doc
%% Sort given list and return its histogram (pairs of unique element and number of its copies).
sorted_histogram([]) ->
    [];
sorted_histogram(L) ->
    [H | T] = lists:sort(L),
    sorted_histogram(T, [{H, 1}]).

-spec sorted_histogram(L :: list(), R :: sorted_histogram()) -> sorted_histogram().
%% @private
%% @doc
%% Build list histogram.
sorted_histogram([], R) ->
    lists:reverse(R);
sorted_histogram([H | T], [{H, C} | RT]) ->
    sorted_histogram(T, [{H, C + 1} | RT]);
sorted_histogram([H | T], R) ->
    sorted_histogram(T, [{H, 1} | R]).
%---------------------------------------------------------------------------------------------------

-spec merge_sorted_histograms(HG1 :: sorted_histogram(),
                              HG2 :: sorted_histogram()) -> sorted_histogram().
%% @doc
%% Merge two sorted histograms.
merge_sorted_histograms(HG1, HG2) ->
    merge_sorted_histograms(HG1, HG2, []).

-spec merge_sorted_histograms(HG1 :: sorted_histogram(),
                              HG2 :: sorted_histogram(),
                              R :: sorted_histogram()) -> sorted_histogram().
%% @private
%% @doc
%% Merge two sorted histograms.
merge_sorted_histograms([], [], R) ->
    lists:reverse(R);
merge_sorted_histograms([], HG, R) ->
    lists:reverse(R) ++ HG;
merge_sorted_histograms(HG, [], R) ->
    lists:reverse(R) ++ HG;
merge_sorted_histograms([{I1, C1} = H1 | T1] = HG1, [{I2, C2} = H2 | T2] = HG2, R) ->
    if
        I1 < I2 ->
            merge_sorted_histograms(T1, HG2, [H1 | R]);
        I2 < I1 ->
            merge_sorted_histograms(HG1, T2, [H2 | R]);
        true ->
            merge_sorted_histograms(T1, T2, [{I1, C1 + C2} | R])
    end.

%---------------------------------------------------------------------------------------------------

-spec apply_to_any_pair(L :: list(), F) -> false | {true, term(), term(), term()}
      when F :: fun((term(), term()) -> false | {true, term()}).
%% @doc
%% Try to apply function to any pair of list elements.
%% After first success apply return pair of elements and result of function.
apply_to_any_pair([], _) ->
    false;
apply_to_any_pair([H | T], F) ->
    apply_to_any_pair(H, T, [], F).

-spec apply_to_any_pair(H :: term(),
                        T :: list(),
                        R :: list(),
                        F) -> false | {true, term(), term(), term()}
      when F :: fun((term(), term()) -> false | {true, term()}).
%% @private
%% @doc
%% Try to apply function to any pair of list elements.
apply_to_any_pair(_, [], [], _) ->
    false;
apply_to_any_pair(_, [], R, F) ->
    [H | T] = lists:reverse(R),
    apply_to_any_pair(H, T, [], F);
apply_to_any_pair(H, [H2 | T], R, F) ->
    case F(H, H2) of
        {true, Res} ->
            {true, H, H2, Res};
        false ->
            apply_to_any_pair(H, T, [H2 | R], F)
    end.

%---------------------------------------------------------------------------------------------------
% Mathematical functions on lists.
%---------------------------------------------------------------------------------------------------

-spec product(L :: nlist()) -> number().
%% @doc
%% Multiplicate all elements of list.
product([]) ->
    1;
product(L) ->
    foldl_1(fun jdlib_math:mul/2, L).

%---------------------------------------------------------------------------------------------------

-spec minmax(L :: nlist()) -> {number(), number()}.
%% @doc
%% Simultaneous search minimum and maximum elements of the list.
minmax([]) ->
    throw({badarg, []});
minmax([H | T]) ->
    minmax(T, {H, H}).

-spec minmax(L :: nlist(), {number(), number()}) -> {number(), number()}.
%% @doc
%% Simultaneous search minimum and maximum elements of the list.
minmax([], R) ->
    R;
minmax([H], {Min, Max}) ->
    {min(Min, H), max(Max, H)};
minmax([H1, H2 | T], {Min, Max}) ->
    R =
        if
            H1 < H2 ->
                {min(Min, H1), max(Max, H2)};
            true ->
                {min(Min, H2), max(Max, H1)}
        end,
    minmax(T, R).

%---------------------------------------------------------------------------------------------------

-spec add(L1 :: nlist() | number(), L2 :: nlist() | number()) -> nlist().
%% @doc
%% Lists addition.
add(L1, L2) when (is_list(L1) andalso is_list(L2)) ->
    lists:zipwith(fun jdlib_math:add/2, L1, L2).

%---------------------------------------------------------------------------------------------------

-spec sub(L1 :: nlist() | number(), L2 :: nlist() | number()) -> nlist().
%% @doc
%% Lists substraction.
sub(L1, L2) when (is_list(L1) andalso is_list(L2)) ->
    lists:zipwith(fun jdlib_math:sub/2, L1, L2).

%---------------------------------------------------------------------------------------------------

-spec neg(L :: nlist() | number()) -> nlist().
%% @doc
%% List negate.
neg(L) when is_list(L) ->
    lists:map(fun(X) -> -X end, L).

%---------------------------------------------------------------------------------------------------

-spec mul(L1 :: nlist() | number(), L2 :: nlist() | number()) -> nlist().
%% @doc
%% Lists multiplication.
mul(L1, L2) when (is_list(L1) andalso is_list(L2)) ->
    lists:zipwith(fun jdlib_math:mul/2, L1, L2).

%---------------------------------------------------------------------------------------------------

-spec square(L :: nlist() | number()) -> nlist().
%% @doc
%% Square of list.
square(L) when is_list(L) ->
    mul(L, L).

%---------------------------------------------------------------------------------------------------

-spec dvs(L1 :: nlist() | number(), L2 :: nlist() | number()) -> nlist().
%% @doc
%% Lists division.
dvs(L1, L2) when (is_list(L1) andalso is_list(L2)) ->
    lists:zipwith(fun jdlib_math:dvs/2, L1, L2).

%---------------------------------------------------------------------------------------------------

-spec inv(L :: nlist() | number()) -> nlist().
%% @doc
%% List inversion.
inv(L) when is_list(L) ->
    lists:map(fun(X) -> 1 / X end, L).

%---------------------------------------------------------------------------------------------------

-spec log(L :: nlist() | number()) -> nlist().
%% @doc
%% Log of all elements of list.
log(L) when is_list(L) ->
    lists:map(fun math:log/1, L).

%---------------------------------------------------------------------------------------------------

