%% @doc
%% Real line realization.
%%
%% This module is intended for processing sets of points (locuses) on real line.
%% <ul>
%% <b>Supported sets types:</b>
%%   <li>empty set</li>
%%   <li>simple point, <tt>P</tt></li>
%%   <li>interval, with open or closed edges,
%%       <tt>(P1, P2)</tt>, <tt>(P1, P2]</tt>, <tt>[P1, P2)</tt>, <tt>[P1, P2]</tt></li>
%%   <li>ray, negative or positive, with open or closed edge,
%%       <tt>(ninf, P)</tt>, <tt>(ninf, P]</tt>, <tt>(P, pinf)</tt>, <tt>[P, pinf)</tt></li>
%%   <li>whole line</li>
%% </ul>
%%
%% Module implements some functions on locuses, such as measure, infimum, supremum and so on.
%% <ul>
%% <b>Sets theory operations are impemented too:</b>
%%   <li>iunion</li>
%%   <li>intersection</li>
%%   <li>inversion</li>
%%   <li>difference</li>
%%   <li>symmetric_difference</li>
%% </ul>
%%
%% <ul>
%% <b>Designations:</b>
%%   <li><tt>P</tt> - point (coordinate on <tt>OX</tt> axis)</li>
%%   <li><tt>XP</tt> - extended point</li>
%%   <li><tt>I</tt> - interval</li>
%%   <li><tt>R</tt> - ray</li>
%%   <li><tt>S</tt> - shape</li>
%%   <li><tt>L</tt> - locus (a set of shapes)</li>
%% </ul>
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_realline).

% Functions export.
-export([point/1, interval/4, ray/3, shape/2,
         is_eq/2,
         measure/1, inf/1, sup/1,
         open/1, close/1,
         union/2, intersection/2, inversion/1, difference/2, symmetric_difference/2]).
-compile(export_all).

%---------------------------------------------------------------------------------------------------
% Constants and macroses.
%---------------------------------------------------------------------------------------------------

% Very small value.
-define(EPS, 1.0e-10).

% Less than, greater than and equal check.
% Written as macroses for use in guard expressions.
-define(IS_LT(X, Y), (X < Y - ?EPS)).
-define(IS_GT(X, Y), (X > Y + ?EPS)).
-define(IS_EQ(X, Y), (not (?IS_LT(X, Y) orelse ?IS_GT(X, Y)))).
-define(IS_LE(X, Y), (not ?IS_GT(X, Y))).
-define(IS_GE(X, Y), (not ?IS_LT(X, Y))).

% Check if the shape is point.
% Written as macros for use in guard expressions.
-define(IS_POINT(P), (is_float(P))).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([dir/0,
              point/0, x_point/0, interval/0, ray/0, shape/0,
              locus/0]).

% Direction.
-type dir() :: neg | pos.

% Point.
-type point() :: float().

% Extended point.
-type x_point() :: {point(), boolean()} | ninf | pinf | empty.

% Interval is a set of points between given edges (included or not given edges).
-type interval() :: {interval, {point(), boolean()}, {point(), boolean()}}.

% Ray has start point, direction and start point inclusion attribute.
-type ray() :: {ray, {point(), boolean(), dir()}}.

% Shape.
-type shape() :: empty | point() | interval() | ray() | line.

% Shapes list.
-type shapes_list() :: list(shape()).

% Locus is a set of shapes.
-type locus() :: shape() | shapes_list().

%---------------------------------------------------------------------------------------------------
% Constructors.
%---------------------------------------------------------------------------------------------------

-spec point(P :: number()) -> point().
%% @doc
%% Point constructor.
point(P) when is_number(P) ->

    % If argument is integer it will be converted to float.
    P + 0.0;

% Throw exception if argument is not numeric.
point(T) ->
    throw({badarg, T}).

%---------------------------------------------------------------------------------------------------

-spec interval(P_Min :: number(), Is_Min_In :: boolean(),
               P_Max :: number(), Is_Max_In :: boolean()) -> interval().
%% @doc
%% Interval constructor.
interval(P_Min, Is_Min_In, P_Max, Is_Max_In) ->
    if
        ?IS_GT(P_Min, P_Max) ->
            throw({wrong_interval_bounds, {P_Min, P_Max}});

        ?IS_LT(P_Min, P_Max) ->
            {interval, {point(P_Min), Is_Min_In}, {point(P_Max), Is_Max_In}};

        % If interval edges are equal this is singular interval.
        % In this case both of edges must be included in interval or
        % both of them must not be included.

        % Just point.
        Is_Min_In andalso Is_Max_In ->
            point(P_Min);

        % Empty set.
        (not Is_Min_In) andalso (not Is_Max_In) ->
            empty;

        % Exception.
        true ->
            throw({wrong_singular_interval, {{P_Min, Is_Min_In}, {P_Max, Is_Max_In}}})
    end.

%---------------------------------------------------------------------------------------------------

-spec ray(P :: number(), Is_In :: boolean(), Dir :: dir()) -> ray().
%% @doc
%% Ray constructor.
ray(P, Is_In, Dir) ->
    {ray, {point(P), Is_In, Dir}}.

%---------------------------------------------------------------------------------------------------

-spec shape(XP_Min :: x_point(), XP_Max :: x_point()) -> shape().
%% @doc
%% Shape constructor by given edges.
shape(empty, _) ->
    empty;
shape(_, empty) ->
    empty;

% Point and interval.
shape({P_Min, Is_Min_In}, {P_Max, Is_Max_In}) ->
    interval(P_Min, Is_Min_In, P_Max, Is_Max_In);

% Rays.
shape(ninf, {P, Is_In}) ->
    ray(P, Is_In, neg);
shape({P, Is_In}, pinf) ->
    ray(P, Is_In, pos);

% Line.
shape(ninf, pinf) ->
    line;

% Empty sets.
shape(ninf, ninf) ->
    empty;
shape(pinf, pinf) ->
    empty.

%---------------------------------------------------------------------------------------------------
% Extended points functions.
%---------------------------------------------------------------------------------------------------

-spec invert_x_point(XP :: x_point()) -> x_point().
%% @doc
%% Extended point inversion.
invert_x_point({P, Is_In}) ->
    {P, not Is_In};
invert_x_point(XP) ->
    XP.

%---------------------------------------------------------------------------------------------------

-spec min_x_point(XP1 :: x_point(), XP2 :: x_point()) -> x_point().
%% @doc
%% Minimum of two extended points.
min_x_point(ninf, _) ->
    ninf;
min_x_point(_, ninf) ->
    ninf;
min_x_point(pinf, XP2) ->
    XP2;
min_x_point(XP1, pinf) ->
    XP1;
min_x_point(empty, XP2) ->
    XP2;
min_x_point(XP1, empty) ->
    XP1;
min_x_point({P1, _}, {P2, _}) ->
    min(P1, P2).

%---------------------------------------------------------------------------------------------------

-spec max_x_point(XP1 :: x_point(), XP2 :: x_point()) -> x_point().
%% @doc
%% Maximum of two extended points.
max_x_point(ninf, XP2) ->
    XP2;
max_x_point(XP1, ninf) ->
    XP1;
max_x_point(pinf, _) ->
    pinf;
max_x_point(_, pinf) ->
    pinf;
max_x_point(empty, XP2) ->
    XP2;
max_x_point(XP1, empty) ->
    XP1;
max_x_point({P1, _}, {P2, _}) ->
    max(P1, P2).

%---------------------------------------------------------------------------------------------------
% Shape functions.
%---------------------------------------------------------------------------------------------------

-spec compare_shapes(S1 :: shape(), S2 :: shape()) -> boolean().
%% @doc
%% Two shapes comparison function.
%% Returns true if the first shape is less than the second.
%% Returns false in another case.
compare_shapes(empty, _) ->
    true;
compare_shapes(_, empty) ->
    false;
compare_shapes(line, _) ->
    true;
compare_shapes(_, line) ->
    false;

% Compare point with interval and ray..
compare_shapes(P, S2) when ?IS_POINT(P) ->
    case S2 of
        P2 when ?IS_POINT(P2) ->

            % We use direct relation operations here because
            % the following expression must be satisfied.
            % compare_shapes(P1, P2) = not compare_shapes(P2, P1).
            P < P2;

        % Decide that point is less than interval even if it is
        % equal to interval's minimum edge.
        % In another case we could encounter the strange situation:
        % open interval (P1, P2) is less than point P1.
        {interval, {P_Min, _}, _} ->
            ?IS_LE(P, P_Min);

        {ray, {_, _, neg}} ->
            false;

        % The same logic as in comparison with interval.
        {ray, {P_Ray, _, pos}} ->
            ?IS_LE(P, P_Ray)
    end;

% Compare two intervals.
compare_shapes({interval, {P_Min1, Is_Min_In1}, _}, {interval, {P_Min2, Is_Min_In2}, _}) ->
    if
        ?IS_LT(P_Min1, P_Min2) ->
            true;
        ?IS_GT(P_Min1, P_Min2) ->
            false;
        true ->
            Is_Min_In1 andalso (not Is_Min_In2)
    end;

% Compare interval with ray.
compare_shapes({interval, {P_Min, Is_Min_In}, _}, {ray, {P_Ray, Is_In, Dir}}) ->
    case Dir of
        neg ->
            false;
        pos ->
            if
                ?IS_LT(P_Min, P_Ray) ->
                    true;
                ?IS_GT(P_Min, P_Ray) ->
                    false;
                true ->
                    Is_Min_In andalso (not Is_In)
            end
    end;

% Compare two rays.
compare_shapes({ray, {P1, Is_In1, Dir1}}, {ray, {P2, Is_In2, Dir2}}) ->
    case {Dir1, Dir2} of
        {neg, neg} ->
            if
                ?IS_LT(P1, P2) ->
                    true;
                ?IS_GT(P1, P2) ->
                    false;
                true ->
                    (not Is_In1) andalso Is_In2
            end;
        {neg, pos} ->
            true;
        {pos, neg} ->
            false;
        {pos, pos} ->
            if
                ?IS_LT(P1, P2) ->
                    true;
                ?IS_GT(P1, P2) ->
                    false;
                true ->
                    Is_In1 andalso (not Is_In2)
            end
    end;

% Commutability.
compare_shapes({interval, _, _} = I, P) when ?IS_POINT(P) ->
    not compare_shapes(P, I);
compare_shapes({ray, _} = R, P) when ?IS_POINT(P) ->
    not compare_shapes(P, R);
compare_shapes({ray, _} = R, {interval, _, _} = I) ->
    not compare_shapes(I, R).

%---------------------------------------------------------------------------------------------------
% Shapes union.
%---------------------------------------------------------------------------------------------------

-spec union_interval_interval(I1 :: interval(), I2 :: interval()) -> [interval()] | interval().
%% @doc
%% Two intervals union.
union_interval_interval({interval, {P_Min1, _}, _} = I1,
                        {interval, {P_Min2, _}, _} = I2) when (P_Min1 > P_Min2) ->
    union_interval_interval(I2, I1);

% No intersection and concatenation cases.
union_interval_interval({interval, _, {P_Max1, Is_Max_In1}} = I1,
                        {interval, {P_Min2, Is_Min_In2}, _} = I2) when ?IS_GE(P_Min2, P_Max1) ->
    if
        % No intersection.
        ?IS_GT(P_Min2, P_Max1) ->
            [I1, I2];

        % Concatenation.
        Is_Max_In1 orelse Is_Min_In2 ->
            {{_, {Min, Is_Min_In}, _}, {_, _, {Max, Is_Max_In}}} = {I1, I2},
            {interval, {Min, Is_Min_In}, {Max, Is_Max_In}};

        % Common point is not included in intervals.
        true ->
            [I1, I2]
    end;

% General case.
union_interval_interval({interval, {P_Min1, Is_Min_In1}, {P_Max1, Is_Max_In1}},
                        {interval, {P_Min2, Is_Min_In2}, {P_Max2, Is_Max_In2}}) ->
    XP_Min =
        if
            ?IS_LT(P_Min1, P_Min2) ->
                {P_Min1, Is_Min_In1};
            ?IS_LT(P_Min2, P_Min1) ->
                {P_Min2, Is_Min_In2};
            true ->
                {P_Min1, Is_Min_In1 orelse Is_Min_In2}
        end,
    XP_Max =
        if
            ?IS_GT(P_Max1, P_Max2) ->
                {P_Max1, Is_Max_In1};
            ?IS_GT(P_Max2, P_Max1) ->
                {P_Max2, Is_Max_In2};
            true ->
                {P_Max1, Is_Max_In1 orelse Is_Max_In2}
        end,
    {interval, XP_Min, XP_Max}.

%---------------------------------------------------------------------------------------------------

-spec union_interval_ray(I :: interval(), R :: ray()) -> [shape()] | ray().
%% @doc
%% Union of interval and ray.
%% Result: single ray or initial pair of shapes.
union_interval_ray({interval, {P_Min, Is_Min_In}, {P_Max, Is_Max_In}} = I,
                   {ray, {P_Ray, Is_In, neg}} = R) ->
    if
        ?IS_GT(P_Min, P_Ray)
        orelse (?IS_EQ(P_Min, P_Ray) andalso (not Is_Min_In) andalso (not Is_In)) ->
            [I, R];
        ?IS_LT(P_Max, P_Ray) ->
            R;
        ?IS_GT(P_Max, P_Ray) ->
            {ray, {P_Max, Is_Max_In, neg}};
        true ->
            {ray, {P_Ray, Is_Max_In orelse Is_In, neg}}
    end;
union_interval_ray({interval, {P_Min, Is_Min_In}, {P_Max, Is_Max_In}} = I,
                   {ray, {P_Ray, Is_In, pos}} = R) ->
    if
        ?IS_LT(P_Max, P_Ray)
        orelse (?IS_EQ(P_Max, P_Ray) andalso (not Is_Max_In) andalso (not Is_In)) ->
            [I, R];
        ?IS_GT(P_Min, P_Ray) ->
            R;
        ?IS_LT(P_Min, P_Ray) ->
            {ray, {P_Min, Is_Min_In, pos}};
        true ->
            {ray, {P_Ray, Is_Min_In orelse Is_In, pos}}
    end.

%---------------------------------------------------------------------------------------------------

-spec union_ray_ray(R1 :: ray(), R2 :: ray()) -> [ray()] | ray() | line.
%% @doc
%% Union of two rays.
union_ray_ray({ray, {P1, _, _}} = R1, {ray, {P2, _, _}} = R2) when (P1 > P2) ->
    union_ray_ray(R2, R1);

% Two rays with common point.
union_ray_ray({ray, {P1, Is_In1, Dir1}} = R1,
              {ray, {P2, Is_In2, Dir2}} = R2) when ?IS_EQ(P1, P2) ->

    % Inclusion in final locus.
    Is_In = Is_In1 orelse Is_In2,

    if
        % Codirectional rays.
        Dir1 =:= Dir2 ->
            {ray, {P1, Is_In, Dir1}};

        % Two different directed rays.
        not Is_In ->
            [R1, R2];

        % Whole line.
        true ->
            line
    end;

% General case - different points based rays.
union_ray_ray({ray, {P1, Is_In1, Dir1}} = R1, {ray, {P2, Is_In2, Dir2}} = R2) ->
    case {Dir1, Dir2} of
        {neg, neg} ->
            {ray, {P2, Is_In2, neg}};
        {neg, pos} ->
            [R1, R2];
        {pos, neg} ->
            line;
        {pos, pos} ->
            {ray, {P1, Is_In1, pos}}
    end.

%---------------------------------------------------------------------------------------------------

-spec union_shapes(S1 :: shape(), S2 :: shape()) -> [shape()] | shape().
%% @doc
%% Shapes union.
union_shapes(empty, S2) ->
    S2;
union_shapes(S1, empty) ->
    S1;
union_shapes(line, _) ->
    line;
union_shapes(_, line) ->
    line;

% First argument is point.
union_shapes(P, P2) when (?IS_POINT(P) andalso ?IS_POINT(P2)) ->
    if
        ?IS_EQ(P, P2) ->
            P;
        true ->
            [P, P2]
    end;
union_shapes(P, {interval, {P_Min, _} = XP_Min, {P_Max, _} = XP_Max} = I) when ?IS_POINT(P) ->
    if
        ?IS_LT(P, P_Min) orelse ?IS_GT(P, P_Max) ->
            [P, I];
        ?IS_EQ(P, P_Min) ->
            {interval, {P_Min, true}, XP_Max};
        ?IS_EQ(P, P_Max) ->
            {interval, XP_Min, {P_Max, true}};
        true ->
            I
    end;
union_shapes(P, {ray, {P_Ray, _, Dir}} = R) when ?IS_POINT(P) ->
    if
        ?IS_EQ(P, P_Ray) ->
            {ray, {P, true, Dir}};
        (?IS_GT(P, P_Ray) andalso (Dir =:= pos)) orelse (?IS_LT(P, P_Ray) andalso (Dir =:= neg)) ->
            R;
        true ->
            [P, R]
    end;

% Union interval with interval or ray.
union_shapes({interval, _, _} = I1, {interval, _, _} = I2) ->
    union_interval_interval(I1, I2);
union_shapes({interval, _, _} = I, {ray, _} = R) ->
    union_interval_ray(I, R);

% Two rays union.
union_shapes({ray, _} = R1, {ray, _} = R2) ->
    union_ray_ray(R1, R2);

% Commutability.
union_shapes({interval, _, _} = I, P) when ?IS_POINT(P) ->
    union_shapes(P, I);
union_shapes({ray, _} = R, P) when ?IS_POINT(P) ->
    union_shapes(P, R);
union_shapes({ray, _} = R, {interval, _, _} = I) ->
    union_shapes(I, R).

%---------------------------------------------------------------------------------------------------
% Different shapes intersection.
%---------------------------------------------------------------------------------------------------

-spec intersection_interval_interval(I1 :: interval(),
                                     I2 :: interval()) -> interval() | point() | empty.
%% @doc
%% Two intervals intersection.
intersection_interval_interval({interval, {P_Min1, _}, _} = I1,
                               {interval, {P_Min2, _}, _} = I2) when (P_Min1 > P_Min2) ->
    intersection_interval_interval(I2, I1);

% General case for two intervals.
intersection_interval_interval({interval, {P_Min1, Is_Min_In1}, {P_Max1, Is_Max_In1}},
                               {interval, {P_Min2, Is_Min_In2}, {P_Max2, Is_Max_In2}}) ->
    if
        ?IS_GT(P_Min2, P_Max1) ->
            empty;
        ?IS_LT(P_Min2, P_Max1) ->
            XP_Min =
                if
                    ?IS_LT(P_Min1, P_Min2) ->
                        {P_Min2, Is_Min_In2};
                    ?IS_LT(P_Min2, P_Min1) ->
                        {P_Min1, Is_Min_In1};
                    true ->
                        {P_Min1, Is_Min_In1 andalso Is_Min_In2}
                end,
            XP_Max =
                if
                    ?IS_GT(P_Max1, P_Max2) ->
                        {P_Max2, Is_Max_In2};
                    ?IS_GT(P_Max2, P_Max1) ->
                        {P_Max1, Is_Max_In1};
                    true ->
                        {P_Max1, Is_Max_In1 andalso Is_Max_In2}
                end,
            {interval, XP_Min, XP_Max};
        Is_Max_In1 andalso Is_Min_In2 ->
            P_Max1;
        true ->
            empty
    end.

%---------------------------------------------------------------------------------------------------

-spec intersection_interval_ray(I :: interval(), R :: ray()) -> interval() | point() | empty.
%% @doc
%% Union of interval and ray.
%% Result: interval, point or empty set.
intersection_interval_ray({interval, {P_Min, Is_Min_In} = XP_Min, {P_Max, Is_Max_In}} = I,
                          {ray, {P_Ray, Is_In, neg}}) ->
    if
        ?IS_GT(P_Min, P_Ray) ->
            empty;
        ?IS_EQ(P_Min, P_Ray) ->
            if
                Is_Min_In andalso Is_In ->
                    P_Min;
                true ->
                    empty
            end;
        ?IS_LT(P_Max, P_Ray) ->
            I;
        ?IS_EQ(P_Max, P_Ray) ->
            if
                Is_Max_In andalso Is_In ->
                    I;
                true ->
                    {interval, XP_Min, {P_Max, false}}
            end;
        true ->
            {interval, XP_Min, {P_Ray, Is_In}}
    end;
intersection_interval_ray({interval, {P_Min, Is_Min_In}, {P_Max, Is_Max_In} = XP_Max} = I,
                          {ray, {P_Ray, Is_In, pos}}) ->
    if
        ?IS_LT(P_Max, P_Ray) ->
            empty;
        ?IS_EQ(P_Max, P_Ray) ->
            if
                Is_Max_In andalso Is_In ->
                    P_Max;
                true ->
                    empty
            end;
        ?IS_GT(P_Min, P_Ray) ->
            I;
        ?IS_EQ(P_Min, P_Ray) ->
            if
                Is_Min_In andalso Is_In ->
                    I;
                true ->
                    {interval, {P_Min, false}, XP_Max}
            end;
        true ->
            {interval, {P_Ray, Is_In}, XP_Max}
    end.

%---------------------------------------------------------------------------------------------------

-spec intersection_ray_ray(R1 :: ray(), R2 :: ray()) -> ray() | interval() | point() | empty.
%% @doc
%% Two rays intersection.
%% Result: ray, interval, point or empty set.
intersection_ray_ray({ray, {P1, _, _}} = R1, {ray, {P2, _, _}} = R2) when (P1 > P2) ->
    intersection_ray_ray(R2, R1);

% Common point based rays.
intersection_ray_ray({ray, {P1, Is_In1, Dir1}},
                     {ray, {P2, Is_In2, Dir2}}) when ?IS_EQ(P1, P2) ->

    % Final ray point inclusion attribute.
    Is_In = Is_In1 andalso Is_In2,

    if
        Dir1 =:= Dir2 ->
            {ray, {P1, Is_In, Dir1}};
        Is_In ->
            P1;
        true ->
            empty
    end;

% General case.
intersection_ray_ray({ray, {P1, Is_In1, Dir1}}, {ray, {P2, Is_In2, Dir2}}) ->
    case {Dir1, Dir2} of
        {neg, neg} ->
            {ray, {P1, Is_In1, neg}};
        {neg, pos} ->
            empty;
        {pos, neg} ->
            {interval, {P1, Is_In1}, {P2, Is_In2}};
        {pos, pos} ->
            {ray, {P2, Is_In2, pos}}
    end.

%---------------------------------------------------------------------------------------------------

-spec intersection_shapes(S1 :: shape(), S2 :: shape()) -> shape().
%% @doc
%% Two shapes intersection.
intersection_shapes(empty, _) ->
    empty;
intersection_shapes(_, empty) ->
    empty;
intersection_shapes(line, S2) ->
    S2;
intersection_shapes(S1, line) ->
    S1;

% Intersection of point with point, interval and ray.
intersection_shapes(P, P2) when (?IS_POINT(P) andalso ?IS_POINT(P2)) ->
    if
        ?IS_EQ(P, P2) ->
            P;
        true ->
            empty
    end;
intersection_shapes(P, {interval, {P_Min, Is_Min_In}, {P_Max, Is_Max_In}}) when ?IS_POINT(P) ->
    if
        ?IS_GT(P, P_Min) andalso ?IS_LT(P, P_Max) ->
            P;
        ?IS_EQ(P, P_Min) andalso Is_Min_In ->
            P;
        ?IS_EQ(P, P_Max) andalso Is_Max_In ->
            P;
        true ->
            empty
    end;
intersection_shapes(P, {ray, {P_Ray, Is_In, Dir}}) when ?IS_POINT(P) ->
    if
        (Dir =:= neg) andalso ?IS_LT(P, P_Ray) ->
            P;
        (Dir =:= pos) andalso ?IS_GT(P, P_Ray) ->
            P;
        ?IS_EQ(P, P_Ray) andalso Is_In ->
            P;
        true ->
            empty
    end;

% Intersection of interval with interval or ray.
intersection_shapes({interval, _, _} = I1, {interval, _, _} = I2) ->
    intersection_interval_interval(I1, I2);
intersection_shapes({interval, _, _} = I, {ray, _} = R) ->
    intersection_interval_ray(I, R);

% Two rays intersection.
intersection_shapes({ray, _} = R1, {ray, _} = R2) ->
    intersection_ray_ray(R1, R2);

% Commutability.
intersection_shapes({interval, _, _} = I, P) when ?IS_POINT(P) ->
    intersection_shapes(P, I);
intersection_shapes({ray, _} = R, P) when ?IS_POINT(P) ->
    intersection_shapes(P, R);
intersection_shapes({ray, _} = R, {interval, _, _} = I) ->
    intersection_shapes(I, R).

%---------------------------------------------------------------------------------------------------
% Inversion functions.
%---------------------------------------------------------------------------------------------------

-spec inversion_shapes(S1 :: shape(), S2 :: shape()) -> shape().
%% @doc
%% Inversion space between two shapes.
%% Warning!
%% Given shapes have no intersection and first shape is less than the second.
inversion_shapes(S1, S2) ->
    shape(invert_x_point(x_sup(S1)), invert_x_point(x_inf(S2))).

%---------------------------------------------------------------------------------------------------
% Locus function.
%---------------------------------------------------------------------------------------------------

-spec is_eq(L1 :: locus(), L2 :: locus()) -> boolean().
%% @doc
%% Locuses equality function (with determined precision).
is_eq(empty, empty) ->
    true;
is_eq(P1, P2) when (?IS_POINT(P1) andalso ?IS_POINT(P2)) ->
    ?IS_EQ(P1, P2);
is_eq({interval, {P_Min1, Is_Min_In}, {P_Max1, Is_Max_In}},
      {interval, {P_Min2, Is_Min_In}, {P_Max2, Is_Max_In}}) ->
    ?IS_EQ(P_Min1, P_Min2) andalso ?IS_EQ(P_Max1, P_Max2);
is_eq({ray, {P1, Is_In, Dir}}, {ray, {P2, Is_In, Dir}}) ->
    ?IS_EQ(P1, P2);
is_eq(line, line) ->
    true;
is_eq([], []) ->
    true;
is_eq([H1 | T1], [H2 | T2]) ->
    is_eq(H1, H2) andalso is_eq(T1, T2);
is_eq(_, _) ->
    false.

%---------------------------------------------------------------------------------------------------

-spec measure(L :: locus()) -> float() | inf.
%% @doc
%% Locus measure.
measure(empty) ->
    0.0;
measure(P) when ?IS_POINT(P) ->
    0.0;
measure({interval, {P_Min, _}, {P_Max, _}}) ->
    P_Max - P_Min;
measure({ray, _}) ->
    inf;
measure(line) ->
    inf;
measure(L) when is_list(L) ->
    measure(L, 0.0).

-spec measure(L :: shapes_list(), M :: float()) -> float() | inf.
%% @private
%% @doc
%% Shapes list measure.
measure([], M) ->
    M;
measure([H | T], M) ->
    case measure(H) of
        inf ->
            inf;
        MH ->
            measure(T, M + MH)
    end.

%---------------------------------------------------------------------------------------------------

-spec x_inf(L :: locus()) -> x_point().
%% @doc
%% Minimum extended point of locus.
x_inf(empty) ->
    pinf;
x_inf(P) when ?IS_POINT(P) ->
    {P, true};
x_inf({interval, Min, _}) ->
    Min;
x_inf({ray, {P, Is_In, Dir}}) ->
    case Dir of
        neg ->
            ninf;
        pos ->
            {P, Is_In}
    end;
x_inf(line) ->
    ninf;
x_inf(L) when is_list(L) ->
    x_inf(L, empty).

-spec x_inf(L :: locus(), Cur_Min :: x_point()) -> x_point().
%% @private
%% @doc
%% Minimum extended point of shapes list.
x_inf([], Cur_Min) ->
    Cur_Min;
x_inf([H | T], Cur_Min) ->
    x_inf(T, min_x_point(x_inf(H), Cur_Min)).

%---------------------------------------------------------------------------------------------------

-spec inf(L :: locus()) -> point() | ninf | pinf.
%% @doc
%% Infimum.
inf(L) ->
    case x_inf(L) of
        {P, _} ->
            P;
        P ->
            P
    end.

%---------------------------------------------------------------------------------------------------

-spec x_sup(L :: locus()) -> x_point().
%% @doc
%% Maximum extended point of locus.
x_sup(empty) ->
    ninf;
x_sup(P) when ?IS_POINT(P) ->
    {P, true};
x_sup({interval, _, Max}) ->
    Max;
x_sup({ray, {P, Is_In, Dir}}) ->
    case Dir of
        neg ->
            {P, Is_In};
        pos ->
            pinf
    end;
x_sup(line) ->
    pinf;
x_sup(L) when is_list(L) ->
    x_sup(L, empty).

-spec x_sup(L :: locus(), Cur_Max :: x_point()) -> x_point().
%% @private
%% @doc
%% Maximum extended point of shapes list.
x_sup([], Cur_Max) ->
    Cur_Max;
x_sup([H | T], Cur_Max) ->
    x_sup(T, max_x_point(x_sup(H), Cur_Max)).

%---------------------------------------------------------------------------------------------------

-spec sup(L :: locus()) -> point() | ninf | pinf.
%% @doc
%% Locus supremum.
sup(L) ->
    case x_sup(L) of
        {P, _} ->
            P;
        P ->
            P
    end.

%---------------------------------------------------------------------------------------------------

-spec flatten(L :: locus()) -> locus().
%% @doc
%% Flatten locus to list of simple shapes.
flatten(L) when is_list(L) ->
    lists:flatten(L);
flatten(S) ->

    % If there is single shape locus is already flat.
    S.

%---------------------------------------------------------------------------------------------------

-spec sort(L :: locus()) -> locus().
%% @doc
%% Sorting.
sort(L) when is_list(L) ->
    lists:sort(fun compare_shapes/2, L);
sort(S) ->

    % One simple figure is already sorted.
    S.

%---------------------------------------------------------------------------------------------------

-spec simplify(L :: locus()) -> locus().
%% @doc
%% Locus simplification.
simplify(L) when is_list(L) ->
    simplify(sort(flatten(L)), []);
simplify(S) ->

    % It is no to simplify.
    S.

-spec simplify(L :: locus(), Res :: locus()) -> locus().
%% @private
%% @doc
%% Sorted flat list of shapes simplification.
simplify([], Res) ->
    jdlib_utils:unlist(sort(Res));
simplify([H], Res) ->
    simplify([], [H | Res]);
simplify([H1, H2 | T], Res) ->
    case union_shapes(H1, H2) of

        % Here we need new variable, because
        % order of shapes could be changed.
        [New_H1, New_H2] ->
            simplify([New_H2 | T], [New_H1 | Res]);

        New_H ->
            simplify([New_H | T], Res)
    end.

%---------------------------------------------------------------------------------------------------

-spec open(L :: locus()) -> locus().
%% @doc
%% Open locus.
open(empty) ->
    empty;
open(P) when ?IS_POINT(P) ->
    empty;
open({interval, {P_Min, _}, {P_Max, _}}) ->
    {interval, {P_Min, false}, {P_Max, false}};
open({ray, {P, _, Dir}}) ->
    {ray, {P, false, Dir}};
open(line) ->
    line;
open(L) when is_list(L) ->
    lists:map(fun open/1, L).

%---------------------------------------------------------------------------------------------------

-spec close(L :: locus()) -> locus().
%% @doc
%% Close locus.
close(empty) ->
    empty;
close(P) when ?IS_POINT(P) ->
    P;
close({interval, {P_Min, _}, {P_Max, _}}) ->
    {interval, {P_Min, true}, {P_Max, true}};
close({ray, {P, _, Dir}}) ->
    {ray, {P, true, Dir}};
close(line) ->
    line;
close(L) when is_list(L) ->

    % Closure procedure can lead to shapes concatenaion, for example
    % (ninf, P) U (P, pinf) -> (ninf, P] U [P, pinf) -> line
    simplify(lists:map(fun close/1, L)).

%---------------------------------------------------------------------------------------------------
% Sets theory operations.
%---------------------------------------------------------------------------------------------------

-spec union(L1 :: locus(), L2 :: locus()) -> locus().
%% @doc
%% Union of locuses.
union(L1, L2) ->
    simplify([L1, L2]).

%---------------------------------------------------------------------------------------------------

-spec intersection(L1 :: locus(), L2 :: locus()) -> locus().
%% @doc
%% Intersection of locuses.
intersection(L1, L2) ->
    simplify
    (
        [
            intersection_shapes(S1, S2)
            || S1 <- jdlib_utils:list(flatten(L1)),
               S2 <- jdlib_utils:list(flatten(L2))
        ]
    ).

%---------------------------------------------------------------------------------------------------

-spec inversion(L :: locus()) -> locus().
%% @doc
%% Locus inversion.
inversion(L) ->
    inversion([empty] ++ jdlib_utils:list(simplify(L)) ++ [empty], []).

-spec inversion(L :: shapes_list(), Res :: locus()) -> locus().
%% @private
%% @doc

% All shapes are processed.
inversion([_], Res) ->
    jdlib_utils:unlist(simplify(Res));

% Pairwise shapes processing.
inversion([H1, H2 | T], Res) ->
    inversion([H2 | T], [inversion_shapes(H1, H2) | Res]).

%---------------------------------------------------------------------------------------------------

-spec difference(L1 :: locus(), L2 :: locus()) -> locus().
%% @doc
%% Locuses difference.
difference(L1, L2) ->
    intersection(L1, inversion(L2)).

%---------------------------------------------------------------------------------------------------

-spec symmetric_difference(L1 :: locus(), L2 :: locus()) -> locus().
%% @doc
%% Locuses symmetric difference.
symmetric_difference(L1, L2) ->
    union(difference(L1, L2), difference(L2, L1)).

%---------------------------------------------------------------------------------------------------

