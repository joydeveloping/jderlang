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
        [fst/1, snd/1]).

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

