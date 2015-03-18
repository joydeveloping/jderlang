%% @doc
%% 3D geometry functions.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_geom3d).

% Export.
-export([v_make/3, v_make/0, v_is/1, v_x/1, v_y/1, v_z/1, v_xyz/1,
         v_add/2, v_sub/2, v_neg/1, v_mul/2, v_dvs/2, v_inv/1,
         v_add_mul/3, v_med/2, v_inner/3, v_norm/1,
         v_mod_2/1, v_mod/1, v_dist/2, v_scalar/2,
         line_by_two_points/2,
         sphere_make/2, spheres_nest_make/2,
         sphere_line_intersection_t/2, sphere_line_intersection/2,
         spheres_nest_line_intersection_t/2, spheres_nest_line_intersection/2]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([vector/0, line/0, sphere/0, spheres_nest/0]).

% Define vector as record.
-record(vector,
{
    x :: number(),
    y :: number(),
    z :: number()
}).

% Define line as record.
-record(line,
{
    p :: vector(),
    v :: vector()
}).

% Define sphere as record.
-record(sphere,
{
    c :: vector(),
    r :: number()
}).

% Define spheres nest as record.
-record(spheres_nest,
{
    s1 :: sphere(),
    s2 :: sphere()
}).

% Vector.
-type vector() :: #vector{}.

% Line.
-type line() :: #line{}.

% Sphere.
-type sphere() :: #sphere{}.

% Spheres nest.
-type spheres_nest() :: #spheres_nest{}.

%---------------------------------------------------------------------------------------------------
% Vector functions.
%---------------------------------------------------------------------------------------------------

-spec v_make(X :: number(), Y :: number(), Z :: number()) -> vector().
%% @doc
%% Make vector from its components.
v_make(X, Y, Z) ->
    #vector
    {
        x = X,
        y = Y,
        z = Z
    }.

%---------------------------------------------------------------------------------------------------

-spec v_make() -> vector().
%% @doc
%% Make null vector.
v_make() ->
    v_make(0, 0, 0).

%---------------------------------------------------------------------------------------------------

-spec v_is(V :: term()) -> boolean().
%% @doc
%% Check if argument is vector.
v_is(V) ->
    is_record(V, vector).

%---------------------------------------------------------------------------------------------------

-spec v_x(V :: vector()) -> number().
%% @doc
%% Get X component.
v_x(#vector{x = X}) ->
    X;
v_x(V) ->
    throw({badarg, V}).

%---------------------------------------------------------------------------------------------------

-spec v_y(V :: vector()) -> number().
%% @doc
%% Get Y component.
v_y(#vector{y = Y}) ->
    Y;
v_y(V) ->
    throw({badarg, V}).

%---------------------------------------------------------------------------------------------------

-spec v_z(V :: vector()) -> number().
%% @doc
%% Get Z component.
v_z(#vector{z = Z}) ->
    Z;
v_z(V) ->
    throw({badarg, V}).

%---------------------------------------------------------------------------------------------------

-spec v_xyz(V :: vector()) -> {number(), number(), number()}.
%% @doc
%% Get X, Y, Z components.
v_xyz(#vector{x = X, y = Y, z = Z}) ->
    {X, Y, Z};
v_xyz(V) ->
    throw({badarg, V}).

%---------------------------------------------------------------------------------------------------

-spec v_add(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Addition.
v_add(#vector{x = X1, y = Y1, z = Z1}, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(X1 + X2, Y1 + Y2, Z1 + Z2);
v_add(#vector{x = X1, y = Y1, z = Z1}, V2) ->
    v_make(X1 + V2, Y1 + V2, Z1 + V2);
v_add(V1, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(V1 + X2, V1 + Y2, V1 + Z2);
v_add(V1, V2) ->
    V1 + V2.

%---------------------------------------------------------------------------------------------------

-spec v_sub(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Subtraction.
v_sub(#vector{x = X1, y = Y1, z = Z1}, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(X1 - X2, Y1 - Y2, Z1 - Z2);
v_sub(#vector{x = X1, y = Y1, z = Z1}, V2) ->
    v_make(X1 - V2, Y1 - V2, Z1 - V2);
v_sub(V1, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(V1 - X2, V1 - Y2, V1 - Z2);
v_sub(V1, V2) ->
    V1 - V2.

%---------------------------------------------------------------------------------------------------

-spec v_neg(V :: vector() | number()) -> vector() | number().
%% @doc
%% Negate.
v_neg(V) ->
    v_sub(0, V).

%---------------------------------------------------------------------------------------------------

-spec v_mul(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Multiplication (if V1 and V2 are vectors it is component wise multiplication).
v_mul(#vector{x = X1, y = Y1, z = Z1}, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(X1 * X2, Y1 * Y2, Z1 * Z2);
v_mul(#vector{x = X1, y = Y1, z = Z1}, V2) ->
    v_make(X1 * V2, Y1 * V2, Z1 * V2);
v_mul(V1, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(V1 * X2, V1 * Y2, V1 * Z2);
v_mul(V1, V2) ->
    V1 * V2.

%---------------------------------------------------------------------------------------------------

-spec v_dvs(V1 :: vector() | number(), V2 :: vector() | number()) -> vector() | number().
%% @doc
%% Division.
v_dvs(#vector{x = X1, y = Y1, z = Z1}, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(X1 / X2, Y1 / Y2, Z1 / Z2);
v_dvs(#vector{x = X1, y = Y1, z = Z1}, V2) ->
    v_make(X1 / V2, Y1 / V2, Z1 / V2);
v_dvs(V1, #vector{x = X2, y = Y2, z = Z2}) ->
    v_make(V1 / X2, V1 / Y2, V1 / Z2);
v_dvs(V1, V2) ->
    V1 / V2.

%---------------------------------------------------------------------------------------------------

-spec v_inv(V :: vector() | number()) -> vector() | number().
%% @doc
%% Invert.
v_inv(V) ->
    v_dvs(1, V).

%---------------------------------------------------------------------------------------------------

-spec v_add_mul(V1 :: vector(), V2 :: vector(), K :: number()) -> vector().
%% @doc
%% Add to vector V1 vector V2 multiplied on K.
v_add_mul(V1, V2, K) ->
    v_add(V1, v_mul(V2, K)).

%---------------------------------------------------------------------------------------------------

-spec v_med(V1 :: vector(), V2 :: vector()) -> vector().
%% @doc
%% Median of two vecttors.
v_med(V1, V2) ->
    v_mul(0.5, v_add(V1, V2)).

%---------------------------------------------------------------------------------------------------

-spec v_inner(V1 :: vector(), V2 :: vector(), K :: number()) -> vector().
%% @doc
%% Inner vector for K from 0 to 1.
%% When K = 0, inner vector is V1.
%% When K = 1, inner vector is V2.
v_inner(V1, V2, K) when ((K >= 0) andalso (K =< 1)) ->
    v_add_mul(V1, v_sub(V2, V1), K);
v_inner(_, _, K) ->
    throw({badarg, K}).

%---------------------------------------------------------------------------------------------------

-spec v_norm(V :: vector()) -> vector().
%% @doc
%% Normalize vector.
v_norm(V) ->
    M = v_mod(V),
    if
        M == 0 ->
            throw({badarg, V});
        true ->
            v_dvs(V, M)
    end.

%---------------------------------------------------------------------------------------------------

-spec v_mod_2(V :: vector()) -> number().
%% @doc
%% Square of module.
v_mod_2(#vector{x = X, y = Y, z = Z}) ->
    X * X + Y * Y + Z * Z;
v_mod_2(V) ->
    throw({badarg, V}).

%---------------------------------------------------------------------------------------------------

-spec v_mod(V :: vector()) -> number().
%% @doc
%% Module.
v_mod(V) ->
    math:sqrt(v_mod_2(V)).

%---------------------------------------------------------------------------------------------------

-spec v_dist(V1 :: vector(), V2 :: vector()) -> number().
%% @doc
%% Distance.
v_dist(V1, V2) ->
    v_mod(v_sub(V1, V2)).

%---------------------------------------------------------------------------------------------------

-spec v_scalar(V1 :: vector(), V2 :: vector()) -> number().
%% @doc
%% Scalar product.
v_scalar(#vector{x = X1, y = Y1, z = Z1}, #vector{x = X2, y = Y2, z = Z2}) ->
    X1 * X2 + Y1 * Y2 + Z1 * Z2.

%---------------------------------------------------------------------------------------------------
% Line functions.
%---------------------------------------------------------------------------------------------------

-spec line_by_two_points(V1 :: vector(), V2 :: vector()) -> line().
%% @doc
%% Make line by two points.
line_by_two_points(V1, V2) when V1 == V2 ->
    throw({badarg, {V1, V2}});
line_by_two_points(V1, V2) ->
    #line
    {
        p = V1,
        v = v_sub(V2, V1)
    }.

%---------------------------------------------------------------------------------------------------
% Sphere functions.
%---------------------------------------------------------------------------------------------------

-spec sphere_make(C :: vector(), R :: number()) -> sphere().
%% @doc
%% Make sphere.
sphere_make(C, R) ->
    #sphere
    {
        c = C,
        r = R
    }.

%---------------------------------------------------------------------------------------------------

-spec spheres_nest_make(S1 :: sphere(), S2 :: sphere()) -> spheres_nest().
%% @doc
%% Make spheres nest.
spheres_nest_make(S1, S2) ->
    #spheres_nest
    {
        s1 = S1,
        s2 = S2
    }.

%---------------------------------------------------------------------------------------------------
% Intersection.
%---------------------------------------------------------------------------------------------------

-spec sphere_vector_intersection_t(S :: sphere(), V :: vector()) -> [number()].
%% @doc
%% Find T paramenets of intersection sphere and line (0, V)
sphere_vector_intersection_t(#sphere{c = C, r = R}, V) ->
    VV = v_mod_2(V),
    CV = v_scalar(C, V),
    QD = CV * CV - VV * (v_mod_2(C) - R * R),
    if
        CV < 0 ->
            [];
        true ->
            DS = math:sqrt(QD),
            [(CV - DS) / VV, (CV + DS) / VV]
    end.

%---------------------------------------------------------------------------------------------------

-spec sphere_line_intersection_t(S :: sphere(), L :: line()) -> [number()].
%% @doc
%% Find T parameters of intersection sphere and line.
sphere_line_intersection_t(#sphere{c = C, r = R}, #line{p = P, v = V}) ->
    sphere_vector_intersection_t(sphere_make(v_sub(C, P), R), V).

%---------------------------------------------------------------------------------------------------

-spec sphere_line_intersection(S :: sphere(), L :: line()) -> [vector()].
%% @doc
%% Find sphere and line intersection.
sphere_line_intersection(S, #line{p = P, v = V} = L) ->
    case sphere_line_intersection_t(S, L) of
        [] ->
            [];
        [T1, T2] ->
            [v_add_mul(P, V, T1), v_add_mul(P, V, T2)]
    end.

%---------------------------------------------------------------------------------------------------

-spec spheres_nest_vector_intersection_t(SN :: spheres_nest(), V :: vector()) -> [number()].
%% @doc
%% Find T parameters of intersection spheres nest and line (0, V).
spheres_nest_vector_intersection_t(#spheres_nest{s1 = #sphere{c = C1, r = R1},
                                                 s2 = #sphere{c = C2, r = R2}}, V) ->

    % First we have to find spheres intersecting with our vector.
    V12 = v_sub(C2, C1),
    R12 = R2 - R1,
    V12V = v_scalar(V12, V),
    Q = V12V * V12V,
    VV = v_mod_2(V),
    V12V12 = v_mod_2(V12),
    A2 = Q + VV * (R12 * R12 - V12V12),
    C1V = v_scalar(C1, V),
    C1V12 = v_scalar(C1, V12),
    A1 = C1V * V12V + VV * (R1 * R12 - C1V12),
    C1C1 = v_mod_2(C1),
    A0 = C1V * C1V + VV * (R1 * R1 - C1C1),
    Alphas = jdlib_realline:intersection(jdlib_math:solve_square_inequation({A2, 2 * A1, A0}),
                                         jdlib_realline:interval(0, true, 1, true)),

    % The result can be only interval or empty.
    case Alphas of
        {interval, {Alpha1, true}, {Alpha2, true}} ->

            % Try to find extremum points.
            Extrs = jdlib_math:solve_square_equation({A2 * (Q - A2),
                                                      2 * A1 * (Q - A2),
                                                      Q * A0 - A1 * A1}),
            Extrs2 = lists:filter(fun(X) -> (X >= Alpha1) andalso (X =< Alpha2) end,
                                  jdlib_utils:list(Extrs)),
            All_Alphas = lists:append([Alpha1, Alpha2], Extrs2),

            % Find t values by alphas.
            SC_Fun = fun(A) -> v_scalar(v_add_mul(C1, V12, A), V) end,
            SD_Fun = fun(A) -> math:sqrt(A2 * A * A + 2 * A1 * A + A0) end,
            T1_Fun = fun(A) -> (SC_Fun(A) - SD_Fun(A)) / VV end,
            T2_Fun = fun(A) -> (SC_Fun(A) + SD_Fun(A)) / VV end,
            [
                lists:min(lists:map(T1_Fun, All_Alphas)),
                lists:max(lists:map(T2_Fun, All_Alphas))
            ];

        empty ->
            []
    end.

%---------------------------------------------------------------------------------------------------

-spec spheres_nest_line_intersection_t(SN :: spheres_nest(), L :: line()) -> [number()].
%% @doc
%% Find T parameters of intersection spheres nest and line.
spheres_nest_line_intersection_t(#spheres_nest{s1 = #sphere{c = C1, r = R1},
                                               s2 = #sphere{c = C2, r = R2}},
                                 #line{p = P, v = V}) ->
    spheres_nest_vector_intersection_t(spheres_nest_make(sphere_make(v_sub(C1, P), R1),
                                                         sphere_make(v_sub(C2, P), R2)), V).

%---------------------------------------------------------------------------------------------------

-spec spheres_nest_line_intersection(SN :: spheres_nest(), L :: line()) -> [vector()].
%% @doc
%% Find spheres nest and line intersection.
spheres_nest_line_intersection(SN, #line{p = P, v = V} = L) ->
    case spheres_nest_line_intersection_t(SN, L) of
        [] ->
            [];
        [T1, T2] ->
            [v_add_mul(P, V, T1), v_add_mul(P, V, T2)]
    end.

%---------------------------------------------------------------------------------------------------

