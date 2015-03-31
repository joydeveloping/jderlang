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
        [init/3, init/1, data/1, left/1, right/1,
         height/1,
         insert/3, insert/2, is_in/3, is_in/2,
         to_list/1,
         map/2]).

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

-spec insert_test() -> ok.
%% @doc
%% Binary tree insert / extract functions test.
insert_test() ->
    T1 = init(c),
    T2 = insert(T1, b),
    T3 = insert(T2, d),
    T4 = insert(T3, a),
    MT = map(T4, fun(X) -> [X] end),
    ?assertEqual([a, b, c, d], to_list(T4)),
    ?assertEqual([[a], [b], [c], [d]], to_list(MT)),
    ?assertEqual(3, height(T4)),
    ok.

%---------------------------------------------------------------------------------------------------

