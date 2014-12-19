%% @doc
%% Some additional lists functions realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_lists).

% Export.
-export([count/2, sorted_histogram/1]).

%---------------------------------------------------------------------------------------------------
% Functions.
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

-spec sorted_histogram(L :: list()) -> [{term(), integer()}].
%% @doc
%% Sort given list and return its histogram (pairs of unique element and number of its copies).
sorted_histogram([]) ->
    [];
sorted_histogram(L) ->
    [H | T] = lists:sort(L),
    sorted_histogram(T, [{H, 1}]).

-spec sorted_histogram(L :: list(), R :: [{term(), integer()}]) -> [{term(), integer()}].
%% @doc
%% Build list histogram.
sorted_histogram([], R) ->
    lists:reverse(R);
sorted_histogram([H | T], [{H, C} | RT]) ->
    sorted_histogram(T, [{H, C + 1} | RT]);
sorted_histogram([H | T], R) ->
    sorted_histogram(T, [{H, 1} | R]).

%---------------------------------------------------------------------------------------------------

