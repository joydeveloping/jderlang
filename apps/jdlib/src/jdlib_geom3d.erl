%% @doc
%% 3D geometry functions.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_geom3d).

% Export.
-export([v_make/3, v_make/0, v_is/1, v_x/1, v_y/1, v_z/1, v_xyz/1,
         v_add/2, v_sub/2, v_neg/1, v_mul/2, v_dvs/2, v_inv/1]).

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

