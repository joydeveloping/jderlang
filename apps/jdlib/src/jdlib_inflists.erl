%% @doc
%% Infinite lists realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_inflists).

% Export.
-export([iterate/3, iterate/2,
         repeat/1, cycle/1, seq/2, seq/1, geometric_series/2,
         head/1, tail/1,
         take/2, drop/2]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([inflist/0]).

% Define infinite list as record.
-record(inflist,
{
    h :: term(),
    acc :: term(),
    f :: fun((term(), term()) -> {term(), term()})
}).

% Inifinite list.
-type inflist() :: #inflist{}.

%---------------------------------------------------------------------------------------------------
% Infinite lists constructors.
%---------------------------------------------------------------------------------------------------

-spec iterate(H :: term(), Acc :: term(), F) -> inflist()
      when F :: fun((term(), term()) -> {term(), term()}).
%% @doc
%% Create infinite list with head, accumulator and iterate function.
iterate(H, Acc, F) when is_function(F, 2) ->
    #inflist
    {
        h = H,
        acc = Acc,
        f = F
    };
iterate(_, _, F) ->
    throw({badarg, {F, wrong_arity}}).

%---------------------------------------------------------------------------------------------------

-spec iterate(H :: term(), F :: fun((term()) -> term())) -> inflist().
%% @doc
%% Create infinite list with head and iterate function.
iterate(H, F) when is_function(F, 1) ->
    iterate
    (
        H,
        0,
        fun(Cur_H, _) ->
            {F(Cur_H), 0}
        end
    );
iterate(_, F) ->
    throw({badarg, {F, wrong_arity}}).

%---------------------------------------------------------------------------------------------------

-spec repeat(T :: term()) -> inflist().
%% @doc
%% Construct infinite list, containing one repeating element.
%% T -> [T, T, ..]
repeat(T) ->
    iterate
    (
        T,
        fun(_) ->
            T
        end
    ).

%---------------------------------------------------------------------------------------------------

-spec cycle(L :: list()) -> inflist().
%% @doc
%% Construct infinite list, containing infinite number of list L copies.
%% [E1, E2, E2] -> [E1, E2, E3, E1, E2, E3, ..]
cycle([]) ->
    throw({badarg, []});
cycle([H | T]) ->
    iterate
    (
        H,
        T,
        fun
            (_, []) ->
                {H, T};
            (_, [Cur_H | Cur_T]) ->
                {Cur_H, Cur_T}
        end
    ).

%---------------------------------------------------------------------------------------------------

-spec seq(From :: number(), Step :: number()) -> inflist().
%% @doc
%% Construct inflinite list [From, From + Step, From + 2 * Step, ..].
seq(From, Step) ->
    iterate
    (
        From,
        fun(H) ->
            H + Step
        end
    ).

%---------------------------------------------------------------------------------------------------

-spec seq(From :: number()) -> inflist().
%% @doc
%% Construct infinite list [From, From + 1, From + 2, ..].
seq(From) ->
    seq(From, 1).

%---------------------------------------------------------------------------------------------------

-spec geometric_series(Base :: number(), K :: number()) -> inflist().
%% @doc
%% Construct infinite list [Base, Base * K, Base * K^2, ..].
geometric_series(Base, K) ->
    iterate
    (
        Base,
        fun(H) ->
            H * K
        end
    ).

%---------------------------------------------------------------------------------------------------
% Take elements.
%---------------------------------------------------------------------------------------------------

-spec head(IL :: inflist()) -> term().
%% @doc
%% Head of infinite list.
head(IL) when is_record(IL, inflist) ->
    IL#inflist.h.

%---------------------------------------------------------------------------------------------------

-spec tail(IL :: inflist()) -> inflist().
%% @doc
%% Tail of infinite list.
tail(#inflist{h = H, acc = Acc, f = F} = IL) ->
    {New_H, New_Acc} = F(H, Acc),
    IL#inflist{h = New_H, acc = New_Acc}.

%---------------------------------------------------------------------------------------------------

-spec take(IL :: inflist(), N :: integer()) -> list().
%% @doc
%% Take first elements of infinite list.
take(IL, N) ->
    take(IL, N, []).

-spec take(IL :: inflist(), N :: integer(), R :: list()) -> list().
%% @private
%% @doc
%% Take first elements of infinite list.
take(_, 0, R) ->
    lists:reverse(R);
take(IL, N, R) ->
    take(tail(IL), N - 1, [head(IL) | R]).

%---------------------------------------------------------------------------------------------------

-spec drop(IL :: inflist(), N :: integer()) -> inflist().
%% @doc
%% Drop first elements of infinite list.
drop(IL, 0) ->
    IL;
drop(IL, N) ->
    drop(tail(IL), N - 1).

%---------------------------------------------------------------------------------------------------

