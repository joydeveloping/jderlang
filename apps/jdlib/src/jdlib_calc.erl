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
%% /   \   /                                    \-1  /                \
%% | a |   | SUM(x_i^2) SUM(x_i)    n           |    | SUM(y_i * x_i) |
%% | b | = | SUM(x_i)   n           SUM(x_i^-1) |  * | SUM(y_i)       |
%% | c |   | n          SUM(x_i^-1) SUM(x_i^-2) |    | SUM(y_i / x_i) |
%% \   /   \                                    /    \                /
interpolation_ax_b_cdivx(L) ->
    {X, Y} = lists:unzip(L),
    Sum_X2 = lists:sum(jdlib_lists:square(X)),
    Sum_X = lists:sum(X),
    N = length(X),
    Sum_XM1 = lists:sum(jdlib_lists:inv(X)),
    Sum_XM2 = lists:sum(jdlib_lists:inv(jdlib_lists:square(X))),
    M = [[Sum_X2, Sum_X, N], [Sum_X, N, Sum_XM1], [N, Sum_XM1, Sum_XM2]],
    Sum_YX = lists:sum(jdlib_lists:mul(Y, X)),
    Sum_Y = lists:sum(Y),
    Sum_YdX = lists:sum(jdlib_lists:dvs(Y, X)),
    V = [Sum_YX, Sum_Y, Sum_YdX],
    M1 = jdlib_la3d:m_inv(M),
    jdlib_la3d:m_mul_v(M1, V).

%---------------------------------------------------------------------------------------------------

