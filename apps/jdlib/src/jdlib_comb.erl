%% @doc
%% Combinatirics functions.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_comb).

% Export.
-export([c/2, c2/2]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

-spec c(K :: integer(), N :: integer()) -> integer().
%% @doc
%% Number of ways to choose K elements from N elements set,
%% disregarging order, if repetitions are not allowed.
c(K, N) when (K < 0) orelse (N < 0) orelse (K > N) ->
    0;
c(0, _) ->
    1;
c(K, N) when K > N div 2 ->
    c(N - K, N);
c(K, N) ->
    Num = jdlib_lists:product(lists:seq(N, N - K + 1, -1)),
    Den = jdlib_lists:product(lists:seq(1, K)),
    Num div Den.

%---------------------------------------------------------------------------------------------------

-spec c2(K :: integer(), N :: integer()) -> integer().
%% @doc
%% Number of ways to choose K elements from N elements set,
%% disregarding order, if repetitions are allowed.
c2(K, N) ->
    c(K, N + K - 1).

%---------------------------------------------------------------------------------------------------

