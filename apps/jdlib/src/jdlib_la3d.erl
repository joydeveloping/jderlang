%% @doc
%% 3D linear algebra functions.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_la3d).

% Export.
-export([v_x/1, v_y/1, v_z/1, v_i/2,
         v_add/2, v_sub/2, v_neg/1, v_mul/2, v_dvs/2, v_inv/1,
         m_ij/3,
         m_add/2, m_sub/2, m_neg/1, m_mul/2, m_dvs/2, m_inv/1]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([vector/0]).

% Vector.
-type vector() :: [number()].

% Matrix.
-type matrix() :: [vector()].

%---------------------------------------------------------------------------------------------------
% Vector functions.
%---------------------------------------------------------------------------------------------------

-spec v_x(V :: vector()) -> number().
%% @doc
%% X component of vector.
v_x([X, _, _]) ->
    X.

%---------------------------------------------------------------------------------------------------

-spec v_y(V :: vector()) -> number().
%% @doc
%% Y component of vector.
v_y([_, Y, _]) ->
    Y.

%---------------------------------------------------------------------------------------------------

-spec v_z(V :: vector()) -> number().
%% @doc
%% Z component of vector.
v_z([_, _, Z]) ->
    Z.

%---------------------------------------------------------------------------------------------------

-spec v_i(V :: vector(), integer()) -> number().
%% @doc
%% Component of vector.
v_i(V, I) ->
    lists:nth(I, V).

%---------------------------------------------------------------------------------------------------

-spec v_add(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors addition.
v_add([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [X1 + X2, Y1 + Y2, Z1 + Z2];
v_add([X1, Y1, Z1], V2) ->
    [X1 + V2, Y1 + V2, Z1 + V2];
v_add(V1, [_, _, _] = V2) ->
    v_add(V2, V1);
v_add(V1, V2) ->
    V1 + V2.

%---------------------------------------------------------------------------------------------------

-spec v_sub(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors subtraction.
v_sub([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [X1 - X2, Y1 - Y2, Z1 - Z2];
v_sub([X1, Y1, Z1], V2) ->
    [X1 - V2, Y1 - V2, Z1 - V2];
v_sub(V1, [X2, Y2, Z2]) ->
    [V1 - X2, V1 - Y2, V1 - Z2];
v_sub(V1, V2) ->
    V1 - V2.

%---------------------------------------------------------------------------------------------------

-spec v_neg(V :: vector()) -> vector().
%% @doc
%% Negate vector.
v_neg(V) ->
    v_sub(0, V).

%---------------------------------------------------------------------------------------------------

-spec v_mul(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors componentwise multiplication.
v_mul([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [X1 * X2, Y1 * Y2, Z1 * Z2];
v_mul([X1, Y1, Z1], V2) ->
    [X1 * V2, Y1 * V2, Z1 * V2];
v_mul(V1, [_, _, _] = V2) ->
    v_mul(V2, V1);
v_mul(V1, V2) ->
    V1 * V2.

%---------------------------------------------------------------------------------------------------

-spec v_dvs(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors componentwise division.
v_dvs([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [X1 / X2, Y1 / Y2, Z1 / Z2];
v_dvs([X1, Y1, Z1], V2) ->
    [X1 / V2, Y1 / V2, Z1 / V2];
v_dvs(V1, [X2, Y2, Z2]) ->
    [V1 / X2, V1 / Y2, V1 / Z2];
v_dvs(V1, V2) ->
    V1 / V2.

%---------------------------------------------------------------------------------------------------

-spec v_inv(V :: vector()) -> vector().
%% @doc
%% Invert vector.
v_inv(V) ->
    v_dvs(1, V).

%---------------------------------------------------------------------------------------------------
% Matrix functions.
%---------------------------------------------------------------------------------------------------

-spec m_ij(M :: matrix(), I :: integer(), J :: integer()) -> number().
%% @doc
%% Get component of matrix.
m_ij(M, I, J) ->
    v_i(lists:nth(I, M), J).

%---------------------------------------------------------------------------------------------------

-spec m_add(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices addition.
m_add([V11, V12, V13], [V21, V22, V23]) ->
    [v_add(V11, V21), v_add(V12, V22), v_add(V13, V23)];
m_add([V11, V12, V13], M2) ->
    [v_add(V11, M2), v_add(V12, M2), v_add(V13, M2)];
m_add(M1, [_, _, _] = M2) ->
    m_add(M2, M1);
m_add(M1, M2) ->
    M1 + M2.

%---------------------------------------------------------------------------------------------------

-spec m_sub(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices subtraction.
m_sub([V11, V12, V13], [V21, V22, V23]) ->
    [v_sub(V11, V21), v_sub(V12, V22), v_sub(V13, V23)];
m_sub([V11, V12, V13], M2) ->
    [v_sub(V11, M2), v_sub(V12, M2), v_sub(V13, M2)];
m_sub(M1, [V21, V22, V23]) ->
    [v_sub(M1, V21), v_sub(M1, V22), v_sub(M1, V23)];
m_sub(M1, M2) ->
    M1 - M2.

%---------------------------------------------------------------------------------------------------

-spec m_neg(V :: vector()) -> vector().
%% @doc
%% Negate matrix.
m_neg(V) ->
    m_sub(0, V).

%---------------------------------------------------------------------------------------------------

-spec m_mul(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices componentwise multiplication.
m_mul([V11, V12, V13], [V21, V22, V23]) ->
    [v_mul(V11, V21), v_mul(V12, V22), v_mul(V13, V23)];
m_mul([V11, V12, V13], M2) ->
    [v_mul(V11, M2), v_mul(V12, M2), v_mul(V13, M2)];
m_mul(M1, [_, _, _] = M2) ->
    m_mul(M2, M1);
m_mul(M1, M2) ->
    M1 * M2.

%---------------------------------------------------------------------------------------------------

-spec m_dvs(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices componentwise division.
m_dvs([V11, V12, V13], [V21, V22, V23]) ->
    [v_dvs(V11, V21), v_dvs(V12, V22), v_dvs(V13, V23)];
m_dvs([V11, V12, V13], M2) ->
    [v_dvs(V11, M2), v_dvs(V12, M2), v_dvs(V13, M2)];
m_dvs(M1, [V21, V22, V23]) ->
    [v_dvs(M1, V21), v_dvs(M1, V22), v_dvs(M1, V23)];
m_dvs(M1, M2) ->
    M1 / M2.

%---------------------------------------------------------------------------------------------------

-spec m_inv(V :: vector()) -> vector().
%% @doc
%% Invert matrix.
m_inv(V) ->
    m_dvs(1, V).

%---------------------------------------------------------------------------------------------------

