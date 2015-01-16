%% @doc
%% Tests for jdlib.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib,
        [fst/1, snd/1,
         flip/1, curry/2, curry_1/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec fst_test() -> ok.
%% @doc
%% Function fst test.
fst_test() ->
    ?assertEqual(a, fst({a, b})),
    ?assertThrow({badarg, _}, fst(a)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec snd_test() -> ok.
%% @doc
%% Function snd test.
snd_test() ->
    ?assertEqual(b, snd({a, b})),
    ?assertThrow({badarg, _}, snd(a)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec flip_test() -> ok.
%% @doc
%% Function flip test.
flip_test() ->
    ?assertThrow({badarg, _}, flip(5)),
    Minus_F = fun(X, Y) -> X - Y end,
    Flip_Minus_F = flip(Minus_F),
    ?assertEqual(5, Minus_F(10, 5)),
    ?assertEqual(-5, Flip_Minus_F(10, 5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec curry_test() -> ok.
%% @doc
%% Function curry_test.
curry_test() ->
    ?assertThrow({badarg, _}, curry(1, 2)),
    F = fun(A, B, C) -> A * B * C end,
    ?assertThrow({too_many_arguments, _}, curry(F, [1, 2, 3, 4])),
    ?assertEqual(24, curry(F, [2, 3, 4])),
    G = curry(F, [2]),
    H = curry(F, [2, 5]),
    ?assertEqual(30, G([3, 5])),
    ?assertEqual(100, H([10])),
    ok.

%---------------------------------------------------------------------------------------------------

