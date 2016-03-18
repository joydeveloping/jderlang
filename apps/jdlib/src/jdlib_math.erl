%% @doc
%% Mathematical functions realization.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_math).

% Export.
-export([is_lt/3, is_lt/2, is_gt/3, is_gt/2, is_eq/3, is_eq/2, is_le/3, is_le/2, is_ge/3, is_ge/2,
         add/2, sub/2, mul/2, dvs/2, npow/2, ndigits/1, polynomial/2,
         fraction/1, fact/1,
         solve_linear_equation/1, solve_linear_inequation/1,
         solve_square_equation/1, solve_square_inequation/1]).

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
% Simple operations.
%---------------------------------------------------------------------------------------------------

-spec add(X :: number(), Y :: number()) -> number().
%% @doc
%% Addition.
add(X, Y) ->
    X + Y.

%---------------------------------------------------------------------------------------------------

-spec sub(X :: number(), Y :: number()) -> number().
%% @doc
%% Subtraction.
sub(X, Y) ->
    X - Y.

%---------------------------------------------------------------------------------------------------

-spec mul(X :: number(), Y :: number()) -> number().
%% @doc
%% Multiplication.
mul(X, Y) ->
    X * Y.

%---------------------------------------------------------------------------------------------------

-spec dvs(X :: number(), Y :: number()) -> number().
%% @doc
%% Division.
dvs(X, Y) ->
    X / Y.

%---------------------------------------------------------------------------------------------------

-spec npow(X :: number(), N :: integer()) -> number().
%% @doc
%% Natural power.
npow(_, N) when ((not is_integer(N)) orelse (N < 0)) ->
    throw({badarg, N});
npow(X, N) ->
    if
        X == 0 ->
            0;
        N == 0 ->
            1;
        true ->
            X * npow(X, N - 1)
    end.

%---------------------------------------------------------------------------------------------------

-spec ndigits(N :: number()) -> integer().
%% @doc
%% Digits count for natural number.
ndigits(N) when (N < 10) ->
    1;
ndigits(N) ->
    1 + ndigits(N div 10).

%---------------------------------------------------------------------------------------------------

-spec polynomial(P :: [number()], X :: number()) -> number().
%% @doc
%% Calculate polynomial function.
polynomial([], _) ->
    0;
polynomial([K], _) ->
    K;
polynomial([H | T], X) ->
    X * polynomial(T, X) + H.

%---------------------------------------------------------------------------------------------------
% Other functions.
%---------------------------------------------------------------------------------------------------

-spec fraction(X :: number()) -> number().
%% @doc
%% Fraction of number.
fraction(X) ->
    X - trunc(X).

%---------------------------------------------------------------------------------------------------

-spec fact(N :: integer()) -> integer().
%% @doc
%% Factorial.
fact(1) ->
    1;
fact(N) when (is_integer(N) andalso (N > 1)) ->
    N * fact(N - 1).

%---------------------------------------------------------------------------------------------------
% Linear and square equation and inequation.
%---------------------------------------------------------------------------------------------------

-spec solve_linear_equation({A :: number(), B :: number()}) -> jdlib_realline:locus().
%% @doc
%% Solve linear equation Ax + B = 0.
solve_linear_equation({A, B}) ->
    if
        A /= 0 ->
            jdlib_realline:point(-B / A);
        B == 0 ->
            line;
        true ->
            empty
    end.

%---------------------------------------------------------------------------------------------------

-spec solve_linear_inequation({A :: number(), B :: number()}) -> jdlib_realline:locus().
%% @doc
%% Solve linear inequation Ax + B >= 0.
solve_linear_inequation({A, B}) ->
    if
        A /= 0 ->
            jdlib_realline:ray
            (
                -B / A,
                true,
                if
                    A > 0 ->
                        pos;
                    true ->
                        neg
                end
            );
        B >= 0 ->
            line;
        true ->
            empty
    end.

%---------------------------------------------------------------------------------------------------

-spec solve_square_equation({A :: number(), B :: number()}) -> jdlib_realline:locus().
%% @doc
%% Solve square equation Ax^2 + Bx + C = 0.
solve_square_equation({A, B, C}) ->
    if
        A /= 0 ->
            D = B * B - 4 * A * C,
            if
                D > 0 ->
                    SD = math:sqrt(D),
                    lists:sort([(-B - SD) / (2 * A), (-B + SD) / (2 * A)]);
                D < 0 ->
                    empty;
                true ->
                    -B / (2 * A)
            end;
        true ->
            solve_linear_equation({B, C})
    end.

%---------------------------------------------------------------------------------------------------

-spec solve_square_inequation({A :: number(),
                               B :: number(),
                               C :: number()}) -> jdlib_realline:locus().
%% @doc
%% Solve square inequation Ax^2 + Bx + C >= 0.
solve_square_inequation({A, B, C} = EQ) ->
    if
        A /= 0 ->
            case solve_square_equation(EQ) of
                [X1, X2] ->
                    if
                        A > 0 ->
                            [
                                jdlib_realline:ray(X1, true, neg),
                                jdlib_realline:ray(X2, true, pos)
                            ];
                        true ->
                            jdlib_realline:interval(X1, true, X2, true)
                    end;
                X when is_number(X) ->
                    if
                        A > 0 ->
                            line;
                        true ->
                            X
                    end;
                empty ->
                    if
                        A > 0 ->
                            line;
                        true ->
                            empty
                    end
            end;
        true ->
            solve_linear_inequation({B, C})
    end.

%---------------------------------------------------------------------------------------------------

