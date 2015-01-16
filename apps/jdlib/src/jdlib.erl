%% @doc
%% General functions of jdlib.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib).

% Export.
-export([fst/1, snd/1,
         flip/1, curry/2, curry_1/2]).

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
% Haskell like curry function.
%---------------------------------------------------------------------------------------------------

-spec curry(fun(), list()) -> fun((list()) -> term()).
%% @doc
%% Haskell like curry function.
%% Agruments of this funtion are:
%%   - function f of n arguments,
%%   - list args of first m actual arguments (m <= n).
%% Result of this function is new function g with n - m remain arguments new_args collected in list.
%% g new_args = f args
%%
%% Note!
%% After curring new function take not several arguments (remain arguments), but
%% single argument - list of remain arguments.
%% For example:
%% F = fun(A, B, C) -> A + B + C end.
%% G = curry(F, [5]).
%% G is not function fun(B, C), but function fun([B, C]).
curry(F, Args) when (is_function(F) andalso is_list(Args)) ->
    {arity, Arity} = erlang:fun_info(F, arity),
    Args_Count = length(Args),

    % We can curry only if Args_Count <= Arity.
    if
        Args_Count < Arity ->
            fun(Remain) ->
                apply(F, Args ++ Remain)
            end;
        Args_Count =:= Arity ->
            apply(F, Args);
        true ->
            throw({too_many_arguments, Args})
    end;

curry(F, Args) ->
    throw({badarg, {F, Args}}).

%---------------------------------------------------------------------------------------------------

-spec curry_1(fun(), term()) -> fun().
%% @doc
%% Curry case with one given actual argument.
curry_1(F, T) ->
    curry(F, [T]).

%---------------------------------------------------------------------------------------------------

