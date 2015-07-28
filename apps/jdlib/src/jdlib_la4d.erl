%% @doc
%% 4D linear algebra functions.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_la4d).

% Export.
-export([v_x/1, v_y/1, v_z/1, v_t/1, v_i/2,
         v_add/2, v_sub/2, v_neg/1, v_scalar/2,
         v_component_mul/2, v_component_dvs/2, v_component_inv/1,
         m_ij/3,
         m_add/2, m_sub/2, m_neg/1, m_component_mul/2, m_component_dvs/2, m_component_inv/1,
         m_det/1, m_trans/1, m_3x3_dets_matrix/1, m_inv/1,
         m_mul_v/2]).

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
v_x([X, _, _, _]) ->
    X.

%---------------------------------------------------------------------------------------------------

-spec v_y(V :: vector()) -> number().
%% @doc
%% Y component of vector.
v_y([_, Y, _, _]) ->
    Y.

%---------------------------------------------------------------------------------------------------

-spec v_z(V :: vector()) -> number().
%% @doc
%% Z component of vector.
v_z([_, _, Z, _]) ->
    Z.

%---------------------------------------------------------------------------------------------------

-spec v_t(V :: vector()) -> number().
%% @doc
%% T component of vector.
v_t([_, _, _, T]) ->
    T.

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
v_add([X1, Y1, Z1, T1], [X2, Y2, Z2, T2]) ->
    [X1 + X2, Y1 + Y2, Z1 + Z2, T1 + T2];
v_add([X1, Y1, Z1, T1], V2) ->
    [X1 + V2, Y1 + V2, Z1 + V2, T1 + V2];
v_add(V1, [_, _, _, _] = V2) ->
    v_add(V2, V1);
v_add(V1, V2) ->
    V1 + V2.

%---------------------------------------------------------------------------------------------------

-spec v_sub(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors subtraction.
v_sub([X1, Y1, Z1, T1], [X2, Y2, Z2, T2]) ->
    [X1 - X2, Y1 - Y2, Z1 - Z2, T1 - T2];
v_sub([X1, Y1, Z1, T1], V2) ->
    [X1 - V2, Y1 - V2, Z1 - V2, T1 - V2];
v_sub(V1, [X2, Y2, Z2, T2]) ->
    [V1 - X2, V1 - Y2, V1 - Z2, V1 - T2];
v_sub(V1, V2) ->
    V1 - V2.

%---------------------------------------------------------------------------------------------------

-spec v_neg(V :: vector()) -> vector().
%% @doc
%% Negate vector.
v_neg(V) ->
    v_sub(0, V).

%---------------------------------------------------------------------------------------------------

-spec v_scalar(V1 :: vector(), V2 :: vector()) -> number().
%% @doc
%% Scalar multiplication.
v_scalar([X1, Y1, Z1, T1], [X2, Y2, Z2, T2]) ->
    X1 * X2 + Y1 * Y2 + Z1 * Z2 + T1 * T2.

%---------------------------------------------------------------------------------------------------

-spec v_component_mul(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors componentwise multiplication.
v_component_mul([X1, Y1, Z1, T1], [X2, Y2, Z2, T2]) ->
    [X1 * X2, Y1 * Y2, Z1 * Z2, T1 * T2];
v_component_mul([X1, Y1, Z1, T1], V2) ->
    [X1 * V2, Y1 * V2, Z1 * V2, T1 * V2];
v_component_mul(V1, [_, _, _, _] = V2) ->
    v_component_mul(V2, V1);
v_component_mul(V1, V2) ->
    V1 * V2.

%---------------------------------------------------------------------------------------------------

-spec v_component_dvs(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Vectors componentwise division.
v_component_dvs([X1, Y1, Z1, T1], [X2, Y2, Z2, T2]) ->
    [X1 / X2, Y1 / Y2, Z1 / Z2, T1 / T2];
v_component_dvs([X1, Y1, Z1, T1], V2) ->
    [X1 / V2, Y1 / V2, Z1 / V2, T1 / V2];
v_component_dvs(V1, [X2, Y2, Z2, T2]) ->
    [V1 / X2, V1 / Y2, V1 / Z2, V1 / T2];
v_component_dvs(V1, V2) ->
    V1 / V2.

%---------------------------------------------------------------------------------------------------

-spec v_component_inv(V :: vector()) -> vector().
%% @doc
%% Componentwise invert vector.
v_component_inv(V) ->
    v_component_dvs(1, V).

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
m_add([V11, V12, V13, V14], [V21, V22, V23, V24]) ->
    [v_add(V11, V21), v_add(V12, V22), v_add(V13, V23), v_add(V14, V24)];
m_add([V11, V12, V13, V14], M2) ->
    [v_add(V11, M2), v_add(V12, M2), v_add(V13, M2), v_add(V14, M2)];
m_add(M1, [_, _, _, _] = M2) ->
    m_add(M2, M1);
m_add(M1, M2) ->
    M1 + M2.

%---------------------------------------------------------------------------------------------------

-spec m_sub(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices subtraction.
m_sub([V11, V12, V13, V14], [V21, V22, V23, V24]) ->
    [v_sub(V11, V21), v_sub(V12, V22), v_sub(V13, V23), v_sub(V14, V24)];
m_sub([V11, V12, V13, V14], M2) ->
    [v_sub(V11, M2), v_sub(V12, M2), v_sub(V13, M2), v_sub(V14, M2)];
m_sub(M1, [V21, V22, V23, V24]) ->
    [v_sub(M1, V21), v_sub(M1, V22), v_sub(M1, V23), v_sub(M1, V24)];
m_sub(M1, M2) ->
    M1 - M2.

%---------------------------------------------------------------------------------------------------

-spec m_neg(M :: matrix()) -> matrix().
%% @doc
%% Negate matrix.
m_neg(M) ->
    m_sub(0, M).

%---------------------------------------------------------------------------------------------------

-spec m_component_mul(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices componentwise multiplication.
m_component_mul([V11, V12, V13, V14], [V21, V22, V23, V24]) ->
    [
        v_component_mul(V11, V21),
        v_component_mul(V12, V22),
        v_component_mul(V13, V23),
        v_component_mul(V14, V24)
    ];
m_component_mul([V11, V12, V13, V14], M2) ->
    [
        v_component_mul(V11, M2),
        v_component_mul(V12, M2),
        v_component_mul(V13, M2),
        v_component_mul(V14, M2)
    ];
m_component_mul(M1, [_, _, _, _] = M2) ->
    m_component_mul(M2, M1);
m_component_mul(M1, M2) ->
    M1 * M2.

%---------------------------------------------------------------------------------------------------

-spec m_component_dvs(M1 :: matrix(), M2 :: matrix()) -> matrix().
%% @doc
%% Matrices componentwise division.
m_component_dvs([V11, V12, V13, V14], [V21, V22, V23, V24]) ->
    [
        v_component_dvs(V11, V21),
        v_component_dvs(V12, V22),
        v_component_dvs(V13, V23),
        v_component_dvs(V14, V24)
    ];
m_component_dvs([V11, V12, V13, V14], M2) ->
    [
        v_component_dvs(V11, M2),
        v_component_dvs(V12, M2),
        v_component_dvs(V13, M2),
        v_component_dvs(V14, M2)
    ];
m_component_dvs(M1, [V21, V22, V23, V24]) ->
    [
        v_component_dvs(M1, V21),
        v_component_dvs(M1, V22),
        v_component_dvs(M1, V23),
        v_component_dvs(M1, V24)
    ];
m_component_dvs(M1, M2) ->
    M1 / M2.

%---------------------------------------------------------------------------------------------------

-spec m_component_inv(M :: matrix()) -> matrix().
%% @doc
%% Componentwisr invert matrix.
m_component_inv(M) ->
    m_component_dvs(1, M).

%---------------------------------------------------------------------------------------------------

-spec m_det(M :: matrix()) -> number().
%% @doc
%% Matrix determinant.
m_det([[A11, A12, A13, A14],
       [A21, A22, A23, A24],
       [A31, A32, A33, A34],
       [A41, A42, A43, A44]]) ->

    % Calculate by first line.
    M11 = [[A22, A23, A24], [A32, A33, A34], [A42, A43, A44]],
    M12 = [[A21, A23, A24], [A31, A33, A34], [A41, A43, A44]],
    M13 = [[A21, A22, A24], [A31, A32, A34], [A41, A42, A44]],
    M14 = [[A21, A22, A23], [A31, A32, A33], [A41, A42, A43]],
    D11 = jdlib_la3d:m_det(M11),
    D12 = jdlib_la3d:m_det(M12),
    D13 = jdlib_la3d:m_det(M13),
    D14 = jdlib_la3d:m_det(M14),
    A11 * D11 - A12 * D12 + A13 * D13 - A14 * D14.

%---------------------------------------------------------------------------------------------------

-spec m_trans(M :: matrix()) -> matrix().
%% @doc
%% Matrix transposition.
m_trans([[A11, A12, A13, A14],
         [A21, A22, A23, A24],
         [A31, A32, A33, A34],
         [A41, A42, A43, A44]]) ->
    [[A11, A21, A31, A41],
     [A12, A22, A32, A42],
     [A13, A23, A33, A43],
     [A14, A24, A34, A44]].

%---------------------------------------------------------------------------------------------------

-spec m_3x3_dets_matrix(M :: matrix()) -> matrix().
%% @doc
%% Matrix, containing dets of matrices minors.
m_3x3_dets_matrix([[A11, A12, A13, A14],
                   [A21, A22, A23, A24],
                   [A31, A32, A33, A34],
                   [A41, A42, A43, A44]]) ->
    M11 = [[A22, A23, A24], [A32, A33, A34], [A42, A43, A44]],
    M12 = [[A21, A23, A24], [A31, A33, A34], [A41, A43, A44]],
    M13 = [[A21, A22, A24], [A31, A32, A34], [A41, A42, A44]],
    M14 = [[A21, A22, A23], [A31, A32, A33], [A41, A42, A43]],
    M21 = [[A12, A13, A14], [A32, A33, A34], [A42, A43, A44]],
    M22 = [[A11, A13, A14], [A31, A33, A34], [A41, A43, A44]],
    M23 = [[A11, A12, A14], [A31, A32, A34], [A41, A42, A44]],
    M24 = [[A11, A12, A13], [A31, A32, A33], [A41, A42, A43]],
    M31 = [[A12, A13, A14], [A22, A23, A24], [A42, A43, A44]],
    M32 = [[A11, A13, A14], [A21, A23, A24], [A41, A43, A44]],
    M33 = [[A11, A12, A14], [A21, A22, A24], [A41, A42, A44]],
    M34 = [[A11, A12, A13], [A21, A22, A23], [A41, A42, A43]],
    M41 = [[A12, A13, A14], [A22, A23, A24], [A32, A33, A34]],
    M42 = [[A11, A13, A14], [A21, A23, A24], [A31, A33, A34]],
    M43 = [[A11, A12, A14], [A21, A22, A24], [A31, A32, A34]],
    M44 = [[A11, A12, A13], [A21, A22, A23], [A31, A32, A33]],
    [
        [
            jdlib_la3d:m_det(M11),
            jdlib_la3d:m_det(M12),
            jdlib_la3d:m_det(M13),
            jdlib_la3d:m_det(M14)
        ],
        [
            jdlib_la3d:m_det(M21),
            jdlib_la3d:m_det(M22),
            jdlib_la3d:m_det(M23),
            jdlib_la3d:m_det(M24)
        ],
        [
            jdlib_la3d:m_det(M31),
            jdlib_la3d:m_det(M32),
            jdlib_la3d:m_det(M33),
            jdlib_la3d:m_det(M34)
        ],
        [
            jdlib_la3d:m_det(M41),
            jdlib_la3d:m_det(M42),
            jdlib_la3d:m_det(M43),
            jdlib_la3d:m_det(M44)
        ]
    ].

%---------------------------------------------------------------------------------------------------

-spec m_inv(M :: matrix()) -> {ok, matrix()} | na.
%% @doc
%% Invert matrix.
m_inv(M) ->
    D = m_det(M),

    % If determinant is zero matrix can not be inverted.
    if
        D == 0 ->
            na;

        true ->
            Dets = m_3x3_dets_matrix(M),
            S = [[1, -1, 1, -1], [-1, 1, -1, 1], [1, -1, 1, -1], [-1, 1, -1, 1]],
            m_component_dvs(m_component_mul(Dets, S), D)
    end.

%---------------------------------------------------------------------------------------------------

-spec m_mul_v(M :: matrix(), V :: vector()) -> vector().
%% @doc
%% Multiplicate matrix on vector.
m_mul_v([V1, V2, V3, V4], V) ->
    [v_scalar(V1, V), v_scalar(V2, V), v_scalar(V3, V), v_scalar(V4, V)].

%---------------------------------------------------------------------------------------------------

