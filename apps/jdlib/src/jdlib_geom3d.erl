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
         v_mod2/1, v_mod/1, v_dist/2]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([vector/0]).

% Define vector as record.
-record(vector,
{
    x :: number(),
    y :: number(),
    z :: number()
}).

% Vector.
-type vector() :: #vector{}.

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

-spec v_mod2(V :: vector()) -> number().
%% @doc
%% Square of module.
v_mod2(#vector{x = X, y = Y, z = Z}) ->
    X * X + Y * Y + Z * Z;
v_mod2(V) ->
    throw({badarg, V}).

%---------------------------------------------------------------------------------------------------

-spec v_mod(V :: vector()) -> number().
%% @doc
%% Module.
v_mod(V) ->
    math:sqrt(v_mod2(V)).

%---------------------------------------------------------------------------------------------------

-spec v_dist(V1 :: vector(), V2 :: vector()) -> number().
%% @doc
%% Distance.
v_dist(V1, V2) ->
    v_mod(v_sub(V1, V2)).

%---------------------------------------------------------------------------------------------------

