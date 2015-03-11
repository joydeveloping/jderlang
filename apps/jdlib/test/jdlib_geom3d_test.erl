%% @doc
%% Tests for jdlib_geom3d.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_geom3d_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_geom3d,
        [v_make/3, v_make/0, v_is/1, v_x/1, v_y/1, v_z/1, v_xyz/1,
         v_add/2, v_sub/2, v_neg/1, v_mul/2, v_dvs/2, v_inv/1]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec vector_test() -> ok.
%% @doc
%% Vector tests.
vector_test() ->
    V0 = v_make(),
    V1 = v_make(1, 2, 3),
    V2 = v_make(3, 2, 1),
    ?assert(v_is(V0)),
    ?assert(not v_is(a)),
    ?assertThrow({badarg, _}, v_x(a)),
    ?assertEqual({0, 0, 0}, v_xyz(V0)),
    ?assertEqual({1, 2, 3}, v_xyz(V1)),
    ?assertEqual({4, 4, 4}, v_xyz(v_add(V1, V2))),
    ?assertEqual({2, 3, 4}, v_xyz(v_add(V1, 1))),
    ?assertEqual({2, 3, 4}, v_xyz(v_add(1, V1))),
    ?assertEqual(2, v_add(1, 1)),
    ?assertEqual({-2, 0, 2}, v_xyz(v_sub(V1, V2))),
    ?assertEqual({0, 1, 2}, v_xyz(v_sub(V1, 1))),
    ?assertEqual({0, -1, -2}, v_xyz(v_sub(1, V1))),
    ?assertEqual(0, v_sub(1, 1)),
    ?assertEqual({-1, -2, -3}, v_xyz(v_neg(V1))),
    ?assertEqual({3, 4, 3}, v_xyz(v_mul(V1, V2))),
    ?assertEqual({2, 4, 6}, v_xyz(v_mul(V1, 2))),
    ?assertEqual({2, 4, 6}, v_xyz(v_mul(2, V1))),
    ?assertEqual(6, v_mul(2, 3)),
    ?assert({1 / 3, 1, 3} == v_xyz(v_dvs(V1, V2))),
    ?assert({1 / 2, 1, 3 / 2} == v_xyz(v_dvs(V1, 2))),
    ?assert({2, 1, 2 / 3} == v_xyz(v_dvs(2, V1))),
    ?assert(2 == v_dvs(4, 2)),
    ok.

%---------------------------------------------------------------------------------------------------

