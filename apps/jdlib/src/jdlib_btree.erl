%% @doc
%% Binary tree realization.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_btree).

% Export.
-export([init/3, init/1, data/1, left/1, right/1,
         insert/3, insert/2, is_in/3, is_in/2]).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Tree type.
-export_type([btree/0]).

% Data type.
-type data() :: term().

% Define binary tree as record.
-record(btree,
{
    data :: data(),
    left :: btree() | null,
    right :: btree() | null
}).

% Binary tree.
-type btree() :: #btree{} | null.

%---------------------------------------------------------------------------------------------------
% Create and accessfFunctions.
%---------------------------------------------------------------------------------------------------

-spec init(D :: data(), L :: btree(), R :: btree()) -> btree().
%% @doc
%% Initialize new binary tree.
init(D, L, R) ->
    #btree{data = D, left = L, right = R}.

%---------------------------------------------------------------------------------------------------

-spec init(D :: data()) -> btree().
%% @doc
%% Initialize new binary tree.
init(D) ->
    init(D, null, null).

%---------------------------------------------------------------------------------------------------

-spec data(T :: btree()) -> data().
%% @doc
%% Get binary tree data.
data(#btree{data = D}) ->
    D.

%---------------------------------------------------------------------------------------------------

-spec left(T :: btree()) -> btree().
%% @doc
%% Get left subtree.
left(#btree{left = L}) ->
    L.

%---------------------------------------------------------------------------------------------------

-spec right(T :: btree()) -> btree().
%% @doc
%% Get right subtree.
right(#btree{right = R}) ->
    R.

%---------------------------------------------------------------------------------------------------
% Insert and find functions.
%---------------------------------------------------------------------------------------------------

-spec insert(T :: btree(), New :: data(),
             Is_Gt_Fun :: fun((data(), data()) -> boolean())) -> btree().
%% @doc
%% Insert data in binary tree.
insert(null, New, _) ->
    init(New);
insert(#btree{data = D, left = L, right = R}, New, Is_Gt_Fun) ->
    Is_Gt = Is_Gt_Fun(New, D),
    if
        Is_Gt ->
            init(D, L, insert(R, New, Is_Gt_Fun));
        true ->
            init(D, insert(L, New, Is_Gt_Fun), R)
    end.

%---------------------------------------------------------------------------------------------------

-spec insert(T :: btree(), New :: data()) -> btree().
%% @doc
%% Insert data in binary tree.
insert(T, New) ->
    insert(T, New, fun(X, Y) -> X > Y end).

%---------------------------------------------------------------------------------------------------

-spec is_in(T :: btree(), Data :: data(),
            Is_Gt_Fun :: fun((data(), data()) -> boolean())) -> boolean().
%% @doc
%% Check if data is in binary tree.
is_in(null, _, _) ->
    false;
is_in(#btree{data = D, left = L, right = R}, Data, Is_Gt_Fun) ->
    Is_Gt = Is_Gt_Fun(Data, D),
    Is_Lt = Is_Gt_Fun(D, Data),
    if
        Is_Gt ->
            is_in(R, Data, Is_Gt_Fun);
        Is_Lt ->
            is_in(L, Data, Is_Gt_Fun);
        true ->
            true
    end.

%---------------------------------------------------------------------------------------------------

-spec is_in(T :: btree(), Data :: data()) -> boolean().
%% @doc
%% Check if data is in binary tree.
is_in(T, Data) ->
    is_in(T, Data, fun(X, Y) -> X > Y end).

%---------------------------------------------------------------------------------------------------

