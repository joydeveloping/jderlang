%% @doc
%% Tests for jdlib_realline.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_realline_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_realline,
        [point/1, interval/4, ray/3, shape/2,
         is_eq/2,
         measure/1, inf/1, sup/1,
         open/1, close/1,
         union/2, intersection/2, inversion/1, difference/2, symmetric_difference/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec point_test() -> ok.
%% @doc
%% Function point test.
point_test() ->
    ?assert(is_eq(5.0, point(5.0))),
    ?assert(is_eq(5.0, point(5))),
    ?assertThrow({badarg, _}, point(atom)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec interval_test() -> ok.
%% @doc
%% Function interval test.
interval_test() ->
    ?assert(is_eq({interval, {1.0, true}, {10.0, true}}, interval(1, true, 10, true))),
    ?assertThrow({wrong_interval_bounds, _}, interval(10, true, 1, true)),
    ?assert(is_eq(5.0, interval(5, true, 5, true))),
    ?assert(is_eq(empty, interval(5, false, 5, false))),
    ?assertThrow({wrong_singular_interval, _}, interval(5, false, 5, true)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec ray_test() -> ok.
%% @doc
%% Function ray test.
ray_test() ->
    ?assert(is_eq({ray, {5.0, true, neg}}, ray(5, true, neg))),
    ok.

%---------------------------------------------------------------------------------------------------

-spec shape_test() -> ok.
%% @doc
%% Function shape test.
shape_test() ->
    ?assert(is_eq(empty, shape(empty, 5.0))),
    ?assert(is_eq(empty, shape(5.0, empty))),
    ?assert(is_eq(empty, shape(ninf, ninf))),
    ?assert(is_eq(empty, shape(pinf, pinf))),
    ?assert(is_eq(line, shape(ninf, pinf))),
    ?assert(is_eq({ray, {5.0, true, neg}}, shape(ninf, {5, true}))),
    ?assert(is_eq({ray, {5.0, false, pos}}, shape({5, false}, pinf))),
    ?assert(is_eq({interval, {0.0, true}, {10.0, false}}, shape({0, true}, {10, false}))),
    ok.

%---------------------------------------------------------------------------------------------------

-spec measure_test() -> ok.
%% @doc
%% Function measure test.
measure_test() ->
    ?assertEqual(0.0, measure(empty)),
    ?assertEqual(0.0, measure(point(5))),
    ?assertEqual(10.0, measure(interval(0, true, 10, true))),
    ?assertEqual(inf, measure(ray(5, true, neg))),
    ?assertEqual(inf, measure(ray(5, false, pos))),
    ?assertEqual(inf, measure(line)),
    ?assertEqual(10.0, measure([interval(0, false, 5, false),
                                interval(10, true, 15, true)])),
    ok.

%---------------------------------------------------------------------------------------------------

-spec inf_test() -> ok.
%% @doc
%% Function inf test.
inf_test() ->
    ?assertEqual(pinf, inf(empty)),
    ?assertEqual(5.0, inf(point(5))),
    ?assertEqual(0.0, inf(interval(0, false, 10, true))),
    ?assertEqual(ninf, inf(ray(5, true, neg))),
    ?assertEqual(5.0, inf(ray(5, false, pos))),
    ?assertEqual(ninf, inf(line)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sup_test() -> ok.
%% @doc
%% Function sup test.
sup_test() ->
    ?assertEqual(ninf, sup(empty)),
    ?assertEqual(5.0, sup(point(5))),
    ?assertEqual(10.0, sup(interval(0, false, 10, true))),
    ?assertEqual(5.0, sup(ray(5, true, neg))),
    ?assertEqual(pinf, sup(ray(5, false, pos))),
    ?assertEqual(pinf, sup(line)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec open_test() -> ok.
%% @doc
%% Function open test.
open_test() ->
    ?assert(is_eq(empty, open(empty))),
    ?assert(is_eq(empty, open(point(5)))),
    ?assert(is_eq({interval, {0.0, false}, {10.0, false}}, open(interval(0, false, 10, false)))),
    ?assert(is_eq({interval, {0.0, false}, {10.0, false}}, open(interval(0, false, 10, true)))),
    ?assert(is_eq({interval, {0.0, false}, {10.0, false}}, open(interval(0, true, 10, false)))),
    ?assert(is_eq({interval, {0.0, false}, {10.0, false}}, open(interval(0, false, 10, false)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, open(ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, open(ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, open(ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, open(ray(5, true, pos)))),
    ?assert(is_eq(line, open(line))),
    ok.

%---------------------------------------------------------------------------------------------------

-spec close_test() -> ok.
%% @doc
%% Function close test.
close_test() ->
    ?assert(is_eq(empty, close(empty))),
    ?assert(is_eq(5.0, close(point(5)))),
    ?assert(is_eq({interval, {0.0, true}, {10.0, true}}, close(interval(0, false, 10, false)))),
    ?assert(is_eq({interval, {0.0, true}, {10.0, true}}, close(interval(0, false, 10, true)))),
    ?assert(is_eq({interval, {0.0, true}, {10.0, true}}, close(interval(0, true, 10, false)))),
    ?assert(is_eq({interval, {0.0, true}, {10.0, true}}, close(interval(0, false, 10, false)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, close(ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, close(ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, close(ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, close(ray(5, true, pos)))),
    ?assert(is_eq(line, close(line))),

    % Special cases:
    % 1. The closure of negative and positive rays with common point is whole line.
    % 2. The closure of two open intervals with common edge (point) is one interval.
    ?assert(is_eq(line, close([ray(5, false, neg), ray(5, false, pos)]))),
    ?assert(is_eq({interval, {0.0, true}, {10.0, true}}, close([interval(0, false, 3, false),
                                                                interval(3, false, 10, true)]))),

    ok.

%---------------------------------------------------------------------------------------------------

-spec union_test() -> ok.
%% @doc
%% Function union test.
union_test() ->

    % First argument - empty.
    ?assert(is_eq(empty, union(empty, empty))),
    ?assert(is_eq(5.0, union(empty, point(5)))),
    ?assert(is_eq({interval, {0.0, true}, {5.0, true}}, union(empty, interval(0, true, 5, true)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(empty, ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(empty, ray(5, true, pos)))),
    ?assert(is_eq(line, union(empty, line))),

    % First argument - point.
    ?assert(is_eq(5.0, union(point(5), empty))),
    ?assert(is_eq(5.0, union(point(5), point(5)))),
    ?assert(is_eq([3.0, 5.0], union(point(5), point(3)))),
    ?assert(is_eq([3.0, 5.0], union(point(3), point(5)))),
    ?assert(is_eq([{interval, {0.0, true}, {3.0, true}}, 5.0],
                  union(point(5), interval(0, true, 3, true)))),
    ?assert(is_eq({interval, {0.0, true}, {5.0, true}},
                  union(point(5), interval(0, true, 5, false)))),
    ?assert(is_eq({interval, {0.0, true}, {5.0, true}},
                  union(point(5), interval(0, true, 5, true)))),
    ?assert(is_eq({interval, {2.0, true}, {7.0, true}},
                  union(point(5), interval(2, true, 7, true)))),
    ?assert(is_eq({interval, {5.0, true}, {8.0, false}},
                  union(point(5), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {5.0, true}, {8.0, true}},
                  union(point(5), interval(5, true, 8, true)))),
    ?assert(is_eq([5.0, {interval, {10.0, true}, {15.0, true}}],
                  union(point(5), interval(10, true, 15, true)))),
    ?assert(is_eq([{ray, {3.0, false, neg}}, 5.0], union(point(5), ray(3, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(point(5), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(point(5), ray(5, true, neg)))),
    ?assert(is_eq({ray, {8.0, false, neg}}, union(point(5), ray(8, false, neg)))),
    ?assert(is_eq({ray, {3.0, false, pos}}, union(point(5), ray(3, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(point(5), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(point(5), ray(5, true, pos)))),
    ?assert(is_eq([5.0, {ray, {8.0, true, pos}}], union(point(5), ray(8, true, pos)))),
    ?assert(is_eq(line, union(point(5), line))),

    % First argument - interval.
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}}, union(interval(3, true, 5, true), empty))),
    ?assert(is_eq([1.0, {interval, {3.0, true}, {5.0, true}}],
                  union(interval(3, true, 5, true), point(1)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  union(interval(3, false, 5, true), point(3)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  union(interval(3, true, 5, true), point(3)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  union(interval(3, true, 5, false), point(4)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  union(interval(3, false, 5, false), point(5)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  union(interval(3, false, 5, true), point(5)))),
    ?assert(is_eq([{interval, {3.0, true}, {5.0, true}}, 8.0],
                  union(interval(3, true, 5, true), point(8)))),
    ?assert(is_eq([{interval, {1.0, false}, {2.0, false}}, {interval, {3.0, true}, {5.0, true}}],
                  union(interval(3, true, 5, true), interval(1, false, 2, false)))),
    ?assert(is_eq([{interval, {1.0, false}, {3.0, false}}, {interval, {3.0, false}, {5.0, true}}],
                  union(interval(3, false, 5, true), interval(1, false, 3, false)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, true}},
                  union(interval(3, false, 5, true), interval(1, false, 3, true)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, true}},
                  union(interval(3, true, 5, true), interval(1, false, 3, false)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, true}},
                  union(interval(3, true, 5, true), interval(1, false, 3, true)))),
    ?assert(is_eq({interval, {2.0, true}, {5.0, true}},
                  union(interval(3, true, 5, true), interval(2, true, 4, true)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, false}},
                  union(interval(3, false, 5, false), interval(3, false, 4, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  union(interval(3, false, 5, false), interval(3, true, 4, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  union(interval(3, true, 5, false), interval(3, false, 4, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  union(interval(3, true, 5, false), interval(3, true, 4, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  union(interval(3, true, 5, true), interval(3.5, false, 4.5, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  union(interval(3, true, 5, false), interval(4, true, 5, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  union(interval(3, true, 5, false), interval(4, true, 5, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  union(interval(3, true, 5, true), interval(4, true, 5, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  union(interval(3, true, 5, true), interval(4, true, 5, true)))),
    ?assert(is_eq({interval, {3.0, true}, {8.0, true}},
                  union(interval(3, true, 5, true), interval(4, false, 8, true)))),
    ?assert(is_eq([{interval, {3.0, true}, {5.0, false}}, {interval, {5.0, false}, {8.0, false}}],
                  union(interval(3, true, 5, false), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {8.0, false}},
                  union(interval(3, true, 5, false), interval(5, true, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {8.0, false}},
                  union(interval(3, true, 5, true), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {8.0, false}},
                  union(interval(3, true, 5, true), interval(5, true, 8, false)))),
    ?assert(is_eq([{interval, {3.0, true}, {5.0, true}}, {interval, {8.0, true}, {10.0, false}}],
                  union(interval(3, true, 5, true), interval(8, true, 10, false)))),
    ?assert(is_eq([{ray, {1.0, true, neg}}, {interval, {3.0, true}, {5.0, true}}],
                  union(interval(3, true, 5, true), ray(1, true, neg)))),
    ?assert(is_eq([{ray, {3.0, false, neg}}, {interval, {3.0, false}, {5.0, true}}],
                  union(interval(3, false, 5, true), ray(3, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, false, 5, true), ray(3, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, true, 5, true), ray(3, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, true, 5, true), ray(3, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, true, 5, true), ray(4, true, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, union(interval(3, true, 5, false), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, true, 5, false), ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, true, 5, true), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(interval(3, true, 5, true), ray(5, true, neg)))),
    ?assert(is_eq({ray, {8.0, true, neg}}, union(interval(3, true, 5, true), ray(8, true, neg)))),
    ?assert(is_eq({ray, {1.0, false, pos}}, union(interval(3, true, 5, true), ray(1, false, pos)))),
    ?assert(is_eq({ray, {3.0, false, pos}}, union(interval(3, false, 5, true), ray(3, false, pos)))),
    ?assert(is_eq({ray, {3.0, true, pos}}, union(interval(3, false, 5, true), ray(3, true, pos)))),
    ?assert(is_eq({ray, {3.0, true, pos}}, union(interval(3, true, 5, true), ray(3, false, pos)))),
    ?assert(is_eq({ray, {3.0, true, pos}}, union(interval(3, true, 5, true), ray(3, true, pos)))),
    ?assert(is_eq({ray, {3.0, false, pos}}, union(interval(3, false, 5, true), ray(4, false, pos)))),
    ?assert(is_eq([{interval, {3.0, true}, {5.0, false}}, {ray, {5.0, false, pos}}],
                  union(interval(3, true, 5, false), ray(5, false, pos)))),
    ?assert(is_eq({ray, {3.0, true, pos}}, union(interval(3, true, 5, false), ray(5, true, pos)))),
    ?assert(is_eq({ray, {3.0, true, pos}}, union(interval(3, true, 5, true), ray(5, false, pos)))),
    ?assert(is_eq({ray, {3.0, true, pos}}, union(interval(3, true, 5, true), ray(5, true, pos)))),
    ?assert(is_eq([{interval, {3.0, true}, {5.0, true}}, {ray, {8.0, false, pos}}],
                  union(interval(3, true, 5, true), ray(8, false, pos)))),
    ?assert(is_eq(line, union({interval, {3, false}, {5, false}}, line))),

    % First argument - negative ray.
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, true, neg), empty))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5.0, true, neg), point(3)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5.0, false, neg), point(5)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5.0, true, neg), point(5)))),
    ?assert(is_eq([{ray, {5.0, true, neg}}, 8.0], union(ray(5.0, true, neg), point(8)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, true, neg), interval(1, false, 3, false)))),
    ?assert(is_eq({ray, {5.0, false, neg}},
                  union(ray(5, false, neg), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, false, neg), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, true, neg), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, true, neg), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {8.0, false, neg}}, union(ray(5, true, neg), interval(1, false, 8, false)))),
    ?assert(is_eq([{ray, {5.0, false, neg}}, {interval, {5.0, false}, {8.0, false}}],
                  union(ray(5, false, neg), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {8.0, false, neg}}, union(ray(5, false, neg), interval(5, true, 8, false)))),
    ?assert(is_eq({ray, {8.0, false, neg}}, union(ray(5, true, neg), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {8.0, false, neg}}, union(ray(5, true, neg), interval(5, true, 8, false)))),
    ?assert(is_eq([{ray, {5.0, true, neg}}, {interval, {8.0, false}, {10.0, false}}],
                  union(ray(5, true, neg), interval(8, false, 10, false)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, union(ray(5, false, neg), ray(3, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, union(ray(5, false, neg), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, false, neg), ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, true, neg), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, union(ray(5, true, neg), ray(5, true, neg)))),
    ?assert(is_eq({ray, {8.0, false, neg}}, union(ray(5, false, neg), ray(8, false, neg)))),
    ?assert(is_eq(line, union(ray(5, false, neg), ray(3, false, pos)))),
    ?assert(is_eq([{ray, {5.0, false, neg}}, {ray, {5.0, false, pos}}],
                  union(ray(5, false, neg), ray(5, false, pos)))),
    ?assert(is_eq(line, union(ray(5, false, neg), ray(5, true, pos)))),
    ?assert(is_eq(line, union(ray(5, true, neg), ray(5, false, pos)))),
    ?assert(is_eq(line, union(ray(5, true, neg), ray(5, true, pos)))),
    ?assert(is_eq([{ray, {5.0, false, neg}}, {ray, {8.0, false, pos}}],
                  union(ray(5, false, neg), ray(8, false, pos)))),
    ?assert(is_eq(line, union(ray(5, true, neg), line))),

    % First argument - positive ray.
    ?assert(is_eq({ray, {5.0, false, pos}}, union(ray(5, false, pos), empty))),
    ?assert(is_eq([3.0, {ray, {5.0, false, pos}}], union(ray(5, false, pos), point(3)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, false, pos), point(5)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, true, pos), point(5)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, union(ray(5, false, pos), point(8)))),
    ?assert(is_eq([{interval, {1.0, false}, {3.0, false}}, {ray, {5.0, false, pos}}],
                  union(ray(5, false, pos), interval(1, false, 3, false)))),
    ?assert(is_eq([{interval, {1.0, false}, {5.0, false}}, {ray, {5.0, false, pos}}],
                  union(ray(5, false, pos), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {1.0, false, pos}}, union(ray(5, false, pos), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {1.0, false, pos}}, union(ray(5, true, pos), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {1.0, false, pos}}, union(ray(5, true, pos), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {3.0, false, pos}},
                  union(ray(5, false, pos), interval(3, false, 8, false)))),
    ?assert(is_eq({ray, {5.0, false, pos}},
                  union(ray(5, false, pos), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, false, pos), interval(5, true, 8, false)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, true, pos), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, true, pos), interval(5, true, 8, false)))),
    ?assert(is_eq({ray, {5.0, false, pos}},
                  union(ray(5, false, pos), interval(8, false, 10, false)))),
    ?assert(is_eq([{ray, {3.0, false, neg}}, {ray, {5.0, false, pos}}],
                  union(ray(5, false, pos), ray(3, false, neg)))),
    ?assert(is_eq([{ray, {5.0, false, neg}}, {ray, {5.0, false, pos}}],
                  union(ray(5, false, pos), ray(5, false, neg)))),
    ?assert(is_eq(line, union(ray(5, false, pos), ray(5, true, neg)))),
    ?assert(is_eq(line, union(ray(5, true, pos), ray(5, false, neg)))),
    ?assert(is_eq(line, union(ray(5, true, pos), ray(5, true, neg)))),
    ?assert(is_eq(line, union(ray(5, false, pos), ray(8, false, neg)))),
    ?assert(is_eq({ray, {3.0, false, pos}}, union(ray(5, false, pos), ray(3, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, union(ray(5, false, pos), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, false, pos), ray(5, true, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, true, pos), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, union(ray(5, true, pos), ray(5, true, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, union(ray(5, false, pos), ray(8, false, pos)))),
    ?assert(is_eq(line, union(ray(5, false, pos), line))),

    % First argument - line.
    ?assert(is_eq(line, union(line, empty))),
    ?assert(is_eq(line, union(line, point(5)))),
    ?assert(is_eq(line, union(line, interval(3, false, 5, true)))),
    ?assert(is_eq(line, union(line, ray(5, false, neg)))),
    ?assert(is_eq(line, union(line, ray(5, true, pos)))),
    ?assert(is_eq(line, union(line, line))),

    ok.

%---------------------------------------------------------------------------------------------------

-spec intersection_test() -> ok.
%% @doc
%% Function intersection test.
intersection_test() ->

    % First argument - empty.
    ?assert(is_eq(empty, intersection(empty, empty))),
    ?assert(is_eq(empty, intersection(empty, point(5)))),
    ?assert(is_eq(empty, intersection(empty, interval(0, true, 5, true)))),
    ?assert(is_eq(empty, intersection(empty, ray(5, true, neg)))),
    ?assert(is_eq(empty, intersection(empty, ray(5, true, pos)))),
    ?assert(is_eq(empty, intersection(empty, line))),

    % First argument - point.
    ?assert(is_eq(empty, intersection(point(5), empty))),
    ?assert(is_eq(5.0, intersection(point(5), point(5)))),
    ?assert(is_eq(empty, intersection(point(5), point(3)))),
    ?assert(is_eq(empty, intersection(point(3), point(5)))),
    ?assert(is_eq(empty, intersection(point(5), interval(0, true, 3, true)))),
    ?assert(is_eq(empty, intersection(point(5), interval(0, true, 5, false)))),
    ?assert(is_eq(5.0, intersection(point(5), interval(0, true, 5, true)))),
    ?assert(is_eq(5.0, intersection(point(5), interval(2, true, 7, true)))),
    ?assert(is_eq(empty, intersection(point(5), interval(5, false, 8, false)))),
    ?assert(is_eq(5.0, intersection(point(5), interval(5, true, 8, true)))),
    ?assert(is_eq(empty, intersection(point(5), interval(10, true, 15, true)))),
    ?assert(is_eq(empty, intersection(point(5), ray(3, false, neg)))),
    ?assert(is_eq(empty, intersection(point(5), ray(5, false, neg)))),
    ?assert(is_eq(5.0, intersection(point(5), ray(5, true, neg)))),
    ?assert(is_eq(5.0, intersection(point(5), ray(8, false, neg)))),
    ?assert(is_eq(5.0, intersection(point(5), ray(3, false, pos)))),
    ?assert(is_eq(empty, intersection(point(5), ray(5, false, pos)))),
    ?assert(is_eq(5.0, intersection(point(5), ray(5, true, pos)))),
    ?assert(is_eq(empty, intersection(point(5), ray(8, true, pos)))),
    ?assert(is_eq(5.0, intersection(point(5), line))),

    % First argument - interval.
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), empty))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), point(1)))),
    ?assert(is_eq(empty, intersection(interval(3, false, 5, true), point(3)))),
    ?assert(is_eq(3.0, intersection(interval(3, true, 5, true), point(3)))),
    ?assert(is_eq(4.0, intersection(interval(3, true, 5, false), point(4)))),
    ?assert(is_eq(empty, intersection(interval(3, false, 5, false), point(5)))),
    ?assert(is_eq(5.0, intersection(interval(3, false, 5, true), point(5)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), point(8)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), interval(1, false, 2, false)))),
    ?assert(is_eq(empty, intersection(interval(3, false, 5, true), interval(1, false, 3, false)))),
    ?assert(is_eq(empty, intersection(interval(3, false, 5, true), interval(1, false, 3, true)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), interval(1, false, 3, false)))),
    ?assert(is_eq(3.0, intersection(interval(3, true, 5, true), interval(1, false, 3, true)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, true}},
                  intersection(interval(3, true, 5, true), interval(2, true, 4, true)))),
    ?assert(is_eq({interval, {3.0, false}, {4.0, true}},
                  intersection(interval(3, false, 5, false), interval(3, false, 4, true)))),
    ?assert(is_eq({interval, {3.0, false}, {4.0, true}},
                  intersection(interval(3, false, 5, false), interval(3, true, 4, true)))),
    ?assert(is_eq({interval, {3.0, false}, {4.0, true}},
                  intersection(interval(3, true, 5, false), interval(3, false, 4, true)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, true}},
                  intersection(interval(3, true, 5, false), interval(3, true, 4, true)))),
    ?assert(is_eq({interval, {3.5, false}, {4.5, false}},
                  intersection(interval(3, true, 5, true), interval(3.5, false, 4.5, false)))),
    ?assert(is_eq({interval, {4.0, true}, {5.0, false}},
                  intersection(interval(3, true, 5, false), interval(4, true, 5, false)))),
    ?assert(is_eq({interval, {4.0, true}, {5.0, false}},
                  intersection(interval(3, true, 5, false), interval(4, true, 5, true)))),
    ?assert(is_eq({interval, {4.0, true}, {5.0, false}},
                  intersection(interval(3, true, 5, true), interval(4, true, 5, false)))),
    ?assert(is_eq({interval, {4.0, true}, {5.0, true}},
                  intersection(interval(3, true, 5, true), interval(4, true, 5, true)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, true}},
                  intersection(interval(3, true, 5, true), interval(4, false, 8, true)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, false), interval(5, false, 8, false)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, false), interval(5, true, 8, false)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), interval(5, false, 8, false)))),
    ?assert(is_eq(5.0, intersection(interval(3, true, 5, true), interval(5, true, 8, false)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), interval(8, true, 10, false)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), ray(1, true, neg)))),
    ?assert(is_eq(empty, intersection(interval(3, false, 5, true), ray(3, false, neg)))),
    ?assert(is_eq(empty, intersection(interval(3, false, 5, true), ray(3, true, neg)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), ray(3, false, neg)))),
    ?assert(is_eq(3.0, intersection(interval(3, true, 5, true), ray(3, true, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, true}},
                  intersection(interval(3, true, 5, true), ray(4, true, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  intersection(interval(3, true, 5, false), ray(5, false, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  intersection(interval(3, true, 5, false), ray(5, true, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  intersection(interval(3, true, 5, true), ray(5, false, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  intersection(interval(3, true, 5, true), ray(5, true, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  intersection(interval(3, true, 5, true), ray(8, true, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  intersection(interval(3, true, 5, true), ray(1, false, pos)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  intersection(interval(3, false, 5, true), ray(3, false, pos)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  intersection(interval(3, false, 5, true), ray(3, true, pos)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  intersection(interval(3, true, 5, true), ray(3, false, pos)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  intersection(interval(3, true, 5, true), ray(3, true, pos)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, true}},
                  intersection(interval(3, false, 5, true), ray(4, false, pos)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, false), ray(5, false, pos)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, false), ray(5, true, pos)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), ray(5, false, pos)))),
    ?assert(is_eq(5.0, intersection(interval(3, true, 5, true), ray(5, true, pos)))),
    ?assert(is_eq(empty, intersection(interval(3, true, 5, true), ray(8, false, pos)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, false}},
                  intersection(interval(3, false, 5, false), line))),

    % First argument - negative ray.
    ?assert(is_eq(empty, intersection(ray(5, true, neg), empty))),
    ?assert(is_eq(3.0, intersection(ray(5.0, true, neg), point(3)))),
    ?assert(is_eq(empty, intersection(ray(5.0, false, neg), point(5)))),
    ?assert(is_eq(5.0, intersection(ray(5.0, true, neg), point(5)))),
    ?assert(is_eq(empty, intersection(ray(5.0, true, neg), point(8)))),
    ?assert(is_eq({interval, {1.0, false}, {3.0, false}},
                  intersection(ray(5, true, neg), interval(1, false, 3, false)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, false}},
                  intersection(ray(5, false, neg), interval(1, false, 5, false)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, false}},
                  intersection(ray(5, false, neg), interval(1, false, 5, true)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, false}},
                  intersection(ray(5, true, neg), interval(1, false, 5, false)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, true}},
                  intersection(ray(5, true, neg), interval(1, false, 5, true)))),
    ?assert(is_eq({interval, {1.0, false}, {5.0, true}},
                  intersection(ray(5, true, neg), interval(1, false, 8, false)))),
    ?assert(is_eq(empty, intersection(ray(5, false, neg), interval(5, false, 8, false)))),
    ?assert(is_eq(empty, intersection(ray(5, false, neg), interval(5, true, 8, false)))),
    ?assert(is_eq(empty, intersection(ray(5, true, neg), interval(5, false, 8, false)))),
    ?assert(is_eq(5.0, intersection(ray(5, true, neg), interval(5, true, 8, false)))),
    ?assert(is_eq(empty, intersection(ray(5, true, neg), interval(8, false, 10, false)))),
    ?assert(is_eq({ray, {3.0, false, neg}}, intersection(ray(5, false, neg), ray(3, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, intersection(ray(5, false, neg), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, intersection(ray(5, false, neg), ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, intersection(ray(5, true, neg), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, intersection(ray(5, true, neg), ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, intersection(ray(5, false, neg), ray(8, false, neg)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, false}},
                  intersection(ray(5, false, neg), ray(3, false, pos)))),
    ?assert(is_eq(empty, intersection(ray(5, false, neg), ray(5, false, pos)))),
    ?assert(is_eq(empty, intersection(ray(5, false, neg), ray(5, true, pos)))),
    ?assert(is_eq(empty, intersection(ray(5, true, neg), ray(5, false, pos)))),
    ?assert(is_eq(5.0, intersection(ray(5, true, neg), ray(5, true, pos)))),
    ?assert(is_eq(empty, intersection(ray(5, false, neg), ray(8, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, intersection(ray(5, true, neg), line))),

    % First argument - positive ray.
    ?assert(is_eq(empty, intersection(ray(5, false, pos), empty))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), point(3)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), point(5)))),
    ?assert(is_eq(5.0, intersection(ray(5, true, pos), point(5)))),
    ?assert(is_eq(8.0, intersection(ray(5, false, pos), point(8)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), interval(1, false, 3, false)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), interval(1, false, 5, false)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), interval(1, false, 5, true)))),
    ?assert(is_eq(empty, intersection(ray(5, true, pos), interval(1, false, 5, false)))),
    ?assert(is_eq(5.0, intersection(ray(5, true, pos), interval(1, false, 5, true)))),
    ?assert(is_eq({interval, {5.0, false}, {8.0, false}},
                  intersection(ray(5, false, pos), interval(3, false, 8, false)))),
    ?assert(is_eq({interval, {5.0, false}, {8.0, false}},
                  intersection(ray(5, false, pos), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {5.0, false}, {8.0, false}},
                  intersection(ray(5, false, pos), interval(5, true, 8, false)))),
    ?assert(is_eq({interval, {5.0, false}, {8.0, false}},
                  intersection(ray(5, true, pos), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {5.0, true}, {8.0, false}},
                  intersection(ray(5, true, pos), interval(5, true, 8, false)))),
    ?assert(is_eq({interval, {8.0, false}, {10.0, false}},
                  intersection(ray(5, false, pos), interval(8, false, 10, false)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), ray(3, false, neg)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), ray(5, false, neg)))),
    ?assert(is_eq(empty, intersection(ray(5, false, pos), ray(5, true, neg)))),
    ?assert(is_eq(empty, intersection(ray(5, true, pos), ray(5, false, neg)))),
    ?assert(is_eq(5.0, intersection(ray(5, true, pos), ray(5, true, neg)))),
    ?assert(is_eq({interval, {5.0, false}, {8.0, false}},
                  intersection(ray(5, false, pos), ray(8, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, intersection(ray(5, false, pos), ray(3, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, intersection(ray(5, false, pos), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, intersection(ray(5, false, pos), ray(5, true, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, intersection(ray(5, true, pos), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, intersection(ray(5, true, pos), ray(5, true, pos)))),
    ?assert(is_eq({ray, {8.0, false, pos}}, intersection(ray(5, false, pos), ray(8, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, intersection(ray(5, false, pos), line))),

    % First argument - line.
    ?assert(is_eq(empty, intersection(line, empty))),
    ?assert(is_eq(5.0, intersection(line, point(5)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  intersection(line, interval(3, false, 5, true)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, intersection(line, ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, intersection(line, ray(5, true, pos)))),
    ?assert(is_eq(line, intersection(line, line))),

    ok.

%---------------------------------------------------------------------------------------------------

-spec inversion_test() -> ok.
%% @doc
%% Function inversion test.
inversion_test() ->

    % Simple shapes.
    ?assert(is_eq(line, inversion(empty))),
    ?assert(is_eq([{ray, {5.0, false, neg}}, {ray, {5.0, false, pos}}], inversion(point(5)))),
    ?assert(is_eq([{ray, {1.0, true, neg}}, {ray, {5.0, true, pos}}],
                  inversion(interval(1, false, 5, false)))),
    ?assert(is_eq([{ray, {1.0, true, neg}}, {ray, {5.0, false, pos}}],
                  inversion(interval(1, false, 5, true)))),
    ?assert(is_eq([{ray, {1.0, false, neg}}, {ray, {5.0, true, pos}}],
                  inversion(interval(1, true, 5, false)))),
    ?assert(is_eq([{ray, {1.0, false, neg}}, {ray, {5.0, false, pos}}],
                  inversion(interval(1, true, 5, true)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, inversion(ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, inversion(ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, inversion(ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, inversion(ray(5, true, pos)))),
    ?assert(is_eq(empty, inversion(line))),

    ok.

%---------------------------------------------------------------------------------------------------

-spec difference_test() -> ok.
%% @doc
%% Function difference test.
difference_test() ->

    % First argument - empty.
    ?assert(is_eq(empty, difference(empty, empty))),
    ?assert(is_eq(empty, difference(empty, point(5)))),
    ?assert(is_eq(empty, difference(empty, interval(0, true, 5, true)))),
    ?assert(is_eq(empty, difference(empty, ray(5, true, neg)))),
    ?assert(is_eq(empty, difference(empty, ray(5, true, pos)))),
    ?assert(is_eq(empty, difference(empty, line))),

    % First argument - point.
    ?assert(is_eq(5.0, difference(point(5), empty))),
    ?assert(is_eq(empty, difference(point(5), point(5)))),
    ?assert(is_eq(5.0, difference(point(5), point(3)))),
    ?assert(is_eq(3.0, difference(point(3), point(5)))),
    ?assert(is_eq(5.0, difference(point(5), interval(0, true, 3, true)))),
    ?assert(is_eq(5.0, difference(point(5), interval(0, true, 5, false)))),
    ?assert(is_eq(empty, difference(point(5), interval(0, true, 5, true)))),
    ?assert(is_eq(empty, difference(point(5), interval(2, true, 7, true)))),
    ?assert(is_eq(5.0, difference(point(5), interval(5, false, 8, false)))),
    ?assert(is_eq(empty, difference(point(5), interval(5, true, 8, true)))),
    ?assert(is_eq(5.0, difference(point(5), interval(10, true, 15, true)))),
    ?assert(is_eq(5.0, difference(point(5), ray(3, false, neg)))),
    ?assert(is_eq(5.0, difference(point(5), ray(5, false, neg)))),
    ?assert(is_eq(empty, difference(point(5), ray(5, true, neg)))),
    ?assert(is_eq(empty, difference(point(5), ray(8, false, neg)))),
    ?assert(is_eq(empty, difference(point(5), ray(3, false, pos)))),
    ?assert(is_eq(5.0, difference(point(5), ray(5, false, pos)))),
    ?assert(is_eq(empty, difference(point(5), ray(5, true, pos)))),
    ?assert(is_eq(5.0, difference(point(5), ray(8, true, pos)))),
    ?assert(is_eq(empty, difference(point(5), line))),

    % First argument - interval.
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), empty))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), point(1)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, false, 5, true), point(3)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, true, 5, true), point(3)))),
    ?assert(is_eq([{interval, {3.0, true}, {4.0, false}}, {interval, {4.0, false}, {5.0, false}}],
                  difference(interval(3, true, 5, false), point(4)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, false}},
                  difference(interval(3, false, 5, false), point(5)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, false}},
                  difference(interval(3, false, 5, true), point(5)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), point(8)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), interval(1, false, 2, false)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, false, 5, true), interval(1, false, 3, false)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, false, 5, true), interval(1, false, 3, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), interval(1, false, 3, false)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, true, 5, true), interval(1, false, 3, true)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, true}},
                  difference(interval(3, true, 5, true), interval(2, true, 4, true)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, false}},
                  difference(interval(3, false, 5, false), interval(3, false, 4, true)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, false}},
                  difference(interval(3, false, 5, false), interval(3, true, 4, true)))),
    ?assert(is_eq([3.0, {interval, {4.0, false}, {5.0, false}}],
                  difference(interval(3, true, 5, false), interval(3, false, 4, true)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, false}},
                  difference(interval(3, true, 5, false), interval(3, true, 4, true)))),
    ?assert(is_eq([{interval, {3.0, true}, {3.5, true}}, {interval, {4.5, true}, {5.0, true}}],
                  difference(interval(3, true, 5, true), interval(3.5, false, 4.5, false)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, false}},
                  difference(interval(3, true, 5, false), interval(4, true, 5, false)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, false}},
                  difference(interval(3, true, 5, false), interval(4, true, 5, true)))),
    ?assert(is_eq([{interval, {3.0, true}, {4.0, false}}, 5.0],
                  difference(interval(3, true, 5, true), interval(4, true, 5, false)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, false}},
                  difference(interval(3, true, 5, true), interval(4, true, 5, true)))),
    ?assert(is_eq({interval, {3.0, true}, {4.0, true}},
                  difference(interval(3, true, 5, true), interval(4, false, 8, true)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(interval(3, true, 5, false), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(interval(3, true, 5, false), interval(5, true, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), interval(5, false, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(interval(3, true, 5, true), interval(5, true, 8, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), interval(8, true, 10, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), ray(1, true, neg)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, false, 5, true), ray(3, false, neg)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, false, 5, true), ray(3, true, neg)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), ray(3, false, neg)))),
    ?assert(is_eq({interval, {3.0, false}, {5.0, true}},
                  difference(interval(3, true, 5, true), ray(3, true, neg)))),
    ?assert(is_eq({interval, {4.0, false}, {5.0, true}},
                  difference(interval(3, true, 5, true), ray(4, true, neg)))),
    ?assert(is_eq(empty, difference(interval(3, true, 5, false), ray(5, false, neg)))),
    ?assert(is_eq(empty, difference(interval(3, true, 5, false), ray(5, true, neg)))),
    ?assert(is_eq(5.0, difference(interval(3, true, 5, true), ray(5, false, neg)))),
    ?assert(is_eq(empty, difference(interval(3, true, 5, true), ray(5, true, neg)))),
    ?assert(is_eq(empty, difference(interval(3, true, 5, true), ray(8, true, neg)))),
    ?assert(is_eq(empty, difference(interval(3, true, 5, true), ray(1, false, pos)))),
    ?assert(is_eq(empty, difference(interval(3, false, 5, true), ray(3, false, pos)))),
    ?assert(is_eq(empty, difference(interval(3, false, 5, true), ray(3, true, pos)))),
    ?assert(is_eq(3.0, difference(interval(3, true, 5, true), ray(3, false, pos)))),
    ?assert(is_eq(empty, difference(interval(3, true, 5, true), ray(3, true, pos)))),
    ?assert(is_eq({interval, {3.0, false}, {4.0, true}},
                  difference(interval(3, false, 5, true), ray(4, false, pos)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(interval(3, true, 5, false), ray(5, false, pos)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(interval(3, true, 5, false), ray(5, true, pos)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), ray(5, false, pos)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(interval(3, true, 5, true), ray(5, true, pos)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, true}},
                  difference(interval(3, true, 5, true), ray(8, false, pos)))),
    ?assert(is_eq(empty, difference({interval, {3, false}, {5, false}}, line))),

    % First argument - negative ray.
    ?assert(is_eq({ray, {5.0, true, neg}}, difference(ray(5, true, neg), empty))),
    ?assert(is_eq([{ray, {3.0, false, neg}}, {interval, {3.0, false}, {5.0, true}}],
                  difference(ray(5.0, true, neg), point(3)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(ray(5.0, false, neg), point(5)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(ray(5.0, true, neg), point(5)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, difference(ray(5.0, true, neg), point(8)))),
    ?assert(is_eq([{ray, {1.0, true, neg}}, {interval, {3.0, true}, {5.0, true}}],
                  difference(ray(5, true, neg), interval(1, false, 3, false)))),
    ?assert(is_eq({ray, {1.0, true, neg}},
                  difference(ray(5, false, neg), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {1.0, true, neg}},
                  difference(ray(5, false, neg), interval(1, false, 5, true)))),
    ?assert(is_eq([{ray, {1.0, true, neg}}, 5.0],
                  difference(ray(5, true, neg), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {1.0, true, neg}},
                  difference(ray(5, true, neg), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {1.0, true, neg}},
                  difference(ray(5, true, neg), interval(1, false, 8, false)))),
    ?assert(is_eq({ray, {5.0, false, neg}},
                  difference(ray(5, false, neg), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {5.0, false, neg}},
                  difference(ray(5, false, neg), interval(5, true, 8, false)))),
    ?assert(is_eq({ray, {5.0, true, neg}},
                  difference(ray(5, true, neg), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {5.0, false, neg}},
                  difference(ray(5, true, neg), interval(5, true, 8, false)))),
    ?assert(is_eq({ray, {5.0, true, neg}},
                  difference(ray(5, true, neg), interval(8, false, 10, false)))),
    ?assert(is_eq({interval, {3.0, true}, {5.0, false}},
                  difference(ray(5, false, neg), ray(3, false, neg)))),
    ?assert(is_eq(empty, difference(ray(5, false, neg), ray(5, false, neg)))),
    ?assert(is_eq(empty, difference(ray(5, false, neg), ray(5, true, neg)))),
    ?assert(is_eq(5.0, difference(ray(5, true, neg), ray(5, false, neg)))),
    ?assert(is_eq(empty, difference(ray(5, true, neg), ray(5, true, neg)))),
    ?assert(is_eq(empty, difference(ray(5, false, neg), ray(8, false, neg)))),
    ?assert(is_eq({ray, {3.0, true, neg}}, difference(ray(5, false, neg), ray(3, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(ray(5, false, neg), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(ray(5, false, neg), ray(5, true, pos)))),
    ?assert(is_eq({ray, {5.0, true, neg}}, difference(ray(5, true, neg), ray(5, false, pos)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(ray(5, true, neg), ray(5, true, pos)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(ray(5, false, neg), ray(8, false, pos)))),
    ?assert(is_eq(empty, difference(ray(5, true, neg), line))),

    % First argument - positive ray.
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, false, pos), empty))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, false, pos), point(3)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, false, pos), point(5)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, true, pos), point(5)))),
    ?assert(is_eq([{interval, {5.0, false}, {8.0, false}}, {ray, {8.0, false, pos}}],
                  difference(ray(5, false, pos), point(8)))),
    ?assert(is_eq({ray, {5.0, false, pos}},
                  difference(ray(5, false, pos), interval(1, false, 3, false)))),
    ?assert(is_eq({ray, {5.0, false, pos}},
                  difference(ray(5, false, pos), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {5.0, false, pos}},
                  difference(ray(5, false, pos), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {5.0, true, pos}},
                  difference(ray(5, true, pos), interval(1, false, 5, false)))),
    ?assert(is_eq({ray, {5.0, false, pos}},
                  difference(ray(5, true, pos), interval(1, false, 5, true)))),
    ?assert(is_eq({ray, {8.0, true, pos}},
                  difference(ray(5, false, pos), interval(3, false, 8, false)))),
    ?assert(is_eq({ray, {8.0, true, pos}},
                  difference(ray(5, false, pos), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {8.0, true, pos}},
                  difference(ray(5, false, pos), interval(5, true, 8, false)))),
    ?assert(is_eq([5.0, {ray, {8.0, true, pos}}],
                  difference(ray(5, true, pos), interval(5, false, 8, false)))),
    ?assert(is_eq({ray, {8.0, true, pos}},
                  difference(ray(5, true, pos), interval(5, true, 8, false)))),
    ?assert(is_eq([{interval, {5.0, false}, {8.0, true}}, {ray, {10.0, true, pos}}],
                  difference(ray(5, false, pos), interval(8, false, 10, false)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, false, pos), ray(3, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, false, pos), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, false, pos), ray(5, true, neg)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, difference(ray(5, true, pos), ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, pos}}, difference(ray(5, true, pos), ray(5, true, neg)))),
    ?assert(is_eq({ray, {8.0, true, pos}}, difference(ray(5, false, pos), ray(8, false, neg)))),
    ?assert(is_eq(empty, difference(ray(5, false, pos), ray(3, false, pos)))),
    ?assert(is_eq(empty, difference(ray(5, false, pos), ray(5, false, pos)))),
    ?assert(is_eq(empty, difference(ray(5, false, pos), ray(5, true, pos)))),
    ?assert(is_eq(5.0, difference(ray(5, true, pos), ray(5, false, pos)))),
    ?assert(is_eq(empty, difference(ray(5, true, pos), ray(5, true, pos)))),
    ?assert(is_eq({interval, {5.0, false}, {8.0, true}},
                  difference(ray(5, false, pos), ray(8, false, pos)))),
    ?assert(is_eq(empty, difference(ray(5, false, pos), line))),

    % First argument - line.
    ?assert(is_eq(line, difference(line, empty))),
    ?assert(is_eq([{ray, {5.0, false, neg}}, {ray, {5.0, false, pos}}], difference(line, point(5)))),
    ?assert(is_eq([{ray, {3.0, true, neg}}, {ray, {5.0, false, pos}}],
                  difference(line, interval(3, false, 5, true)))),
    ?assert(is_eq({ray, {5.0, true, pos}}, difference(line, ray(5, false, neg)))),
    ?assert(is_eq({ray, {5.0, false, neg}}, difference(line, ray(5, true, pos)))),
    ?assert(is_eq(empty, difference(line, line))),

    ok.

%---------------------------------------------------------------------------------------------------

-spec precision_test() -> ok.
%% @doc
%% Precision test.
precision_test() ->

    % Consider the identity:
    % (sqrt(2.0) + sqrt(3.0))^2 = 5.0 + 2.0 * sqrt(6.0).
    % Because of calculation precision errors these two values are not equal.
    S = math:sqrt(2.0) + math:sqrt(3.0),
    V1 = S * S,
    V2 = 5.0 + 2.0 * math:sqrt(6.0),
    V = 10.0,
    ?assert(is_eq(union(interval(V1, false, V, false), point(V2)),
                  {interval, {V1, true}, {V, false}})),
    ?assert(is_eq(union(interval(V1, false, V, false), point(V2)),
                  {interval, {V2, true}, {V, false}})),
    ?assert(is_eq(union(interval(V2, false, V, false), point(V1)),
                  {interval, {V1, true}, {V, false}})),
    ?assert(is_eq(union(interval(V2, false, V, false), point(V2)),
                  {interval, {V1, true}, {V, false}})),

    ok.

%---------------------------------------------------------------------------------------------------

