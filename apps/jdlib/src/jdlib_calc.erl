%% @doc
%% Calculation methods.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_calc).

% Export.
-export([interpolation_ax_b_cdivx/1]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Interpolation functions.
%---------------------------------------------------------------------------------------------------

-spec interpolation_ax_b_cdivx(L :: list()) -> list().
%% @doc
%% Interpolation with function a * x + b + c / x.
%% Array of tupples {x_i, y_i} is given.
%% Method of least squares is used.
%% System of equations y_i = a * x_i + b + c / x_i.
%% We want to minimize SUM(d_i^2) = SUM((a * x_i + b + c / x_i - y_i)^2).
interpolation_ax_b_cdivx(L) ->
    io:format("~w", [L]).

%---------------------------------------------------------------------------------------------------

