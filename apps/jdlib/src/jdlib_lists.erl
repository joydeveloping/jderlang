%% @doc
%% Some additional lists functions realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_lists).

% Export.
-export([head/1, tail/1, last/1, init/1,
         count/2, sorted_histogram/1, merge_sorted_histograms/2, apply_to_any_pair/2]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([sorted_histogram/0]).

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

