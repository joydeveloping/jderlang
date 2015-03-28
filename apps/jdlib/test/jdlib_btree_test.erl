%% @doc
%% Tests for jdlib_btree.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_btree_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_btree,
        [init/1, data/1, left/1, right/1]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec create_test() -> ok.
%% @doc
%% Binary tree creation tests.
create_test() ->
    T = init(555),
    ?assertEqual({555, null, null}, {data(T), left(T), right(T)}),
    ok.

%---------------------------------------------------------------------------------------------------

