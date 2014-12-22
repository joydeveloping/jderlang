%% @doc
%% Some util functions realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_utils).

% Functions export.
-export([list/1, unlist/1]).

%---------------------------------------------------------------------------------------------------
% Lists functions.
%---------------------------------------------------------------------------------------------------

-spec list(T :: term()) -> [term()].
%% @doc
%% If argument is not a list returns list with this single element.
list(T) when is_list(T) ->
    T;
list(T) ->
    [T].

%---------------------------------------------------------------------------------------------------

-spec unlist(L :: term()) -> term().
%% @doc
%% Redundant lists elimination.
%% If given list has single element returns this element.
unlist([T]) ->
    T;
unlist(T) ->
    T.

%---------------------------------------------------------------------------------------------------

