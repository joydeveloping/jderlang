%% @doc
%% Tests for jdlib_lists.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_lists_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_lists,
        [count/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec count_test() -> ok.
%% @doc
%% Function count test.
count_test() ->
    L = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    ?assertEqual(7, count(L, fun(X) -> X < 7.5 end)),
    ?assertEqual(5, count(L, fun(X) -> X rem 2 =:= 0 end)),
    ok.

%---------------------------------------------------------------------------------------------------

