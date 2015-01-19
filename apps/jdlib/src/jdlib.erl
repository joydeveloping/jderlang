%% @doc
%% General functions of jdlib.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib).

% Export.
-export([fst/1, snd/1,
         flip/1]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([pair/0]).

% Pair.
-type pair() :: {term(), term()}.

%---------------------------------------------------------------------------------------------------
% Functions for pairs.
%---------------------------------------------------------------------------------------------------

-spec fst(P :: pair()) -> term().
%% @doc
%% Get first element of tuple.
fst({T, _}) ->
    T;
fst(P) ->
    throw({badarg, P}).

%---------------------------------------------------------------------------------------------------

-spec snd(P :: pair()) -> term().
%% @doc
%% Get second element of tuple.
snd({_, T}) ->
    T;
snd(P) ->
    throw({badarg, P}).

%---------------------------------------------------------------------------------------------------
% Haskell like flip function.
% g x y = f y x
%---------------------------------------------------------------------------------------------------

-spec flip(F) -> G
      when F :: fun((term(), term()) -> term()),
           G :: fun((term(), term()) -> term()).
%% @doc
%% Haskell like function flip.
%% It has one argument - function of two arguments f(x, y)
%% and returns function of two arguments g(x, y) = f(y, x).
flip(F) when is_function(F, 2) ->
    fun(X, Y) -> F(Y, X) end;
flip(T) ->
    throw({badarg, T}).

%---------------------------------------------------------------------------------------------------

