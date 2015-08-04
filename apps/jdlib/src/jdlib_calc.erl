%% @doc
%% Calculation methods.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_calc).

% Export.
-export([interpolation_ax_b_cdivx/1,
         interpolation_ax_blogx_c_ddivx/1]).

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
%% We want to minimize SUM(delta_i^2) = SUM((a * x_i + b + c / x_i - y_i)^2).
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
    Sum_Xm1 = lists:sum(jdlib_lists:inv(X)),
    Sum_Xm2 = lists:sum(jdlib_lists:inv(jdlib_lists:square(X))),
    M = [[Sum_X2, Sum_X, N], [Sum_X, N, Sum_Xm1], [N, Sum_Xm1, Sum_Xm2]],
    Sum_YX = lists:sum(jdlib_lists:mul(Y, X)),
    Sum_Y = lists:sum(Y),
    Sum_YdX = lists:sum(jdlib_lists:dvs(Y, X)),
    V = [Sum_YX, Sum_Y, Sum_YdX],
    M1 = jdlib_la3d:m_inv(M),
    jdlib_la3d:m_mul_v(M1, V).

%---------------------------------------------------------------------------------------------------

-spec interpolation_ax_blogx_c_ddivx(L :: list()) -> list().
%% @doc
%% Interpolation with function a * x + b * log(x) + c + d / x.
%% Array of tupples {x_i, y_i} is given.
%% Method of least squares is used.
%% System of equations y_i = a * x_i + b + log(x_i) + c + d / x_i.
%% We want to minimize SUM(delta_i^2) = SUM((a * x_i + b * log(x_i) + c + d / x_i - y_i)^2).
%% /   \   /                                          \-1  /               \
%% | a |   | S(x^2)     S(x * log) S(x)    n          |    | S(y * x)      |
%% | b | = | S(x * log) S(log^2)   S(log)  S(log / x) |  * | S(y * log(x)) |
%% | c |   | S(x)       S(log)     n       S(x^-1)    |    | S(y)          |
%% | d |   | n          S(log / x) S(x^-1) S(x^-2)    |    | S(y / x)      |
%% \   /   \                                          /    \               /
interpolation_ax_blogx_c_ddivx(L) ->
    {X, Y} = lists:unzip(L),
    Log = jdlib_lists:log(X),
    Sum_X2 = lists:sum(jdlib_lists:square(X)),
    Sum_XLog = lists:sum(jdlib_lists:mul(X, Log)),
    Sum_X = lists:sum(X),
    N = length(X),
    Sum_Log = lists:sum(Log),
    Sum_Log2 = lists:sum(jdlib_lists:square(Log)),
    Sum_LogdX = lists:sum(jdlib_lists:dvs(Log, X)),
    Sum_Xm1 = lists:sum(jdlib_lists:inv(X)),
    Sum_Xm2 = lists:sum(jdlib_lists:inv(jdlib_lists:square(X))),
    M = [[Sum_X2,   Sum_XLog,  Sum_X,   N        ],
         [Sum_XLog, Sum_Log2,  Sum_Log, Sum_LogdX],
         [Sum_X,    Sum_Log,   N,       Sum_Xm1  ],
         [N,        Sum_LogdX, Sum_Xm1, Sum_Xm2  ]],
    Sum_YX = lists:sum(jdlib_lists:mul(Y, X)),
    Sum_YLog = lists:sum(jdlib_lists:mul(Y, Log)),
    Sum_Y = lists:sum(Y),
    Sum_YdX = lists:sum(jdlib_lists:dvs(Y, X)),
    V = [Sum_YX, Sum_YLog, Sum_Y, Sum_YdX],
    M1 = jdlib_la4d:m_inv(M),
    jdlib_la4d:m_mul_v(M1, V).

%---------------------------------------------------------------------------------------------------

