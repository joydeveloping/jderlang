%% @doc
%% Infinite lists realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_inflists).

% Export.
-export([repeat/1, cycle/1,
         head/1, tail/1,
         take/2, drop/2]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([inflist/0]).

% Inifinite list.
-type inflist() :: {term(), term(), fun()}.

%---------------------------------------------------------------------------------------------------
% Infinite lists constructors.
%---------------------------------------------------------------------------------------------------

-spec repeat(T :: term()) -> inflist().
%% @doc
%% Construct infinite list, containing one repeating element.
%% T -> [T, T, ..]
repeat(T) ->
    {
        T,
        0,
        fun(_, _) ->
            {T, 0}
        end
    }.

%---------------------------------------------------------------------------------------------------

-spec cycle(L :: list()) -> inflist().
%% @doc
%% Construct infinite list, containing infinite number of list L copies.
%% [E1, E2, E2] -> [E1, E2, E3, E1, E2, E3, ..]
cycle([]) ->
    throw({badarg, []});
cycle([H | T]) ->
    {
        H,
        T,
        fun
            (_, []) ->
                {H, T};
            (_, [Cur_H | Cur_T]) ->
                {Cur_H, Cur_T}
        end
    }.

%---------------------------------------------------------------------------------------------------
% Take elements.
%---------------------------------------------------------------------------------------------------

-spec head(IL :: inflist()) -> term().
%% @doc
%% Head of infinite list.
head({H, _, _}) ->
    H.

%---------------------------------------------------------------------------------------------------

-spec tail(IL :: inflist()) -> inflist().
%% @doc
%% Tail of infinite list.
tail({H, Acc, F}) ->
    {New_H, New_Acc} = F(H, Acc),
    {New_H, New_Acc, F}.

%---------------------------------------------------------------------------------------------------

-spec take(IL :: inflist(), N :: integer()) -> list().
%% @doc
%% Take first elements of infinite list.
take(IL, N) ->
    {IL, N}.

%---------------------------------------------------------------------------------------------------

-spec drop(IL :: inflist(), N :: integer()) -> inflist().
%% @doc
%% Drop first elements of infinite list.
drop(IL, N) ->
    {IL, N}.

%---------------------------------------------------------------------------------------------------

