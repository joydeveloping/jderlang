%% @doc
%% Some additional lists functions realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_lists).

% Export.
-export([count/2]).

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

