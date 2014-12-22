%% @doc
%% Mathematical functions realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_math).

% Export.
-export([is_lt/3, is_lt/2, is_gt/3, is_gt/2, is_eq/3, is_eq/2, is_le/3, is_le/2, is_ge/3, is_ge/2,
         fraction/1]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

% Very small value.
-define(EPS, 1.0e-10).

%---------------------------------------------------------------------------------------------------
% Relation functions.
%---------------------------------------------------------------------------------------------------

-spec is_lt(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Less than check.
is_lt(X, Y, E) ->
    X < Y - E.

%---------------------------------------------------------------------------------------------------

-spec is_lt(X :: float(), Y :: float()) -> boolean().
%% @doc
%% Less than check.
is_lt(X, Y) ->
    is_lt(X, Y, ?EPS).

%---------------------------------------------------------------------------------------------------

-spec is_gt(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Greater than check.
is_gt(X, Y, E) ->
    X > Y + E.

%---------------------------------------------------------------------------------------------------

-spec is_gt(X :: float(), Y :: float()) -> boolean().
%% @doc
%% Greater than check.
is_gt(X, Y) ->
    is_gt(X, Y, ?EPS).

%---------------------------------------------------------------------------------------------------

-spec is_eq(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Equal check.
is_eq(X, Y, E) ->
    not (is_lt(X, Y, E) orelse is_gt(X, Y, E)).

%---------------------------------------------------------------------------------------------------

-spec is_eq(X :: float(), Y :: float()) -> boolean().
%% @doc
%% Equal check.
is_eq(X, Y) ->
    is_eq(X, Y, ?EPS).

%---------------------------------------------------------------------------------------------------

-spec is_le(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Less then or equal check.
is_le(X, Y, E) ->
    not is_gt(X, Y, E).

%---------------------------------------------------------------------------------------------------

-spec is_le(X :: float(), Y :: float()) -> boolean().
%% @doc
%% Less then or equal check.
is_le(X, Y) ->
    is_le(X, Y, ?EPS).

%---------------------------------------------------------------------------------------------------

-spec is_ge(X :: float(), Y :: float(), E :: float()) -> boolean().
%% @doc
%% Greater than or equal check.
is_ge(X, Y, E) ->
    not is_lt(X, Y, E).

%---------------------------------------------------------------------------------------------------

-spec is_ge(X :: float(), Y :: float()) -> boolean().
%% @doc
%% Greater than or equal check.
is_ge(X, Y) ->
    is_ge(X, Y, ?EPS).

%---------------------------------------------------------------------------------------------------
% Other functions.
%---------------------------------------------------------------------------------------------------

-spec fraction(X :: number()) -> number().
%% @doc
%% Fraction of number.
fraction(X) ->
    X - trunc(X).

%---------------------------------------------------------------------------------------------------

