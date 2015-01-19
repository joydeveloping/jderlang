%% @doc
%% Tests for jdlib_inflists.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_inflists_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_inflists,
        [repeat/1, cycle/1,
         head/1, tail/1,
         take/2, drop/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec repeat_test() -> ok.
%% @doc
%% Function repeat test.
repeat_test() ->
    L = repeat(a),
    L1 = tail(L),
    L2 = tail(L1),
    L3 = tail(L2),
    L4 = tail(L3),
    ?assertEqual({a, a, a, a, a}, {head(L), head(L1), head(L2), head(L3), head(L4)}),
    ok.

%---------------------------------------------------------------------------------------------------

-spec cycle_test() -> ok.
%% @doc
%% Function cycle test.
cycle_test() ->
    ?assertThrow({badarg, []}, cycle([])),
    L = cycle([a, b]),
    L1 = tail(L),
    L2 = tail(L1),
    L3 = tail(L2),
    L4 = tail(L3),
    ?assertEqual({a, b, a, b, a}, {head(L), head(L1), head(L2), head(L3), head(L4)}),
    ok.

%---------------------------------------------------------------------------------------------------

-spec take_test() -> ok.
%% @doc
%% Function take test.
take_test() ->
    ok.

%---------------------------------------------------------------------------------------------------

-spec drop_test() -> ok.
%% @doc
%% Function drop test.
drop_test() ->
    ok.

%---------------------------------------------------------------------------------------------------

