%% @doc
%% Some util functions realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_utils).

% Functions export.
-export([is_lt/3, is_gt/3, is_eq/3, is_le/3, is_ge/3, list/1, unlist/1]).

%---------------------------------------------------------------------------------------------------
% Relation functions.
%---------------------------------------------------------------------------------------------------

-spec is_lt(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Less than check.
is_lt(X, Y, E) ->
    X < Y - E.

%---------------------------------------------------------------------------------------------------

-spec is_gt(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Greater check.
is_gt(X, Y, E) ->
    X > Y + E.

%---------------------------------------------------------------------------------------------------

-spec is_eq(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Equal check.
is_eq(X, Y, E) ->
    not (is_lt(X, Y, E) orelse is_gt(X, Y, E)).

%---------------------------------------------------------------------------------------------------

-spec is_le(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Less then or equal check.
is_le(X, Y, E) ->
    not is_gt(X, Y, E).

%---------------------------------------------------------------------------------------------------

-spec is_ge(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Greater than or equal check.
is_ge(X, Y, E) ->
    not is_lt(X, Y, E).

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

