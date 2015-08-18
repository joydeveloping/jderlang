%% @doc
%% General functions of perfan.
%%
%% @author Alexey Rybakov

% Module name.
-module(perfan).

% Export.
-export([parallel_scaling_analyze/5]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
% Functions for pairs.
%---------------------------------------------------------------------------------------------------

-spec parallel_scaling_analyze(Parallel_Factors :: list(),
                               Full_Calc_Time :: number(),
                               Init_Times :: list(),
                               Par_Misc_Times :: list(),
                               Macro_Factor :: number()) -> ok.
%% @doc
%% Analyze parallel scaling performance.
%%   Parallel_Factors - factors of parallel runs,
%%                      for example [1, 3, 5] runs with 1, 3, and 5 parallel nodes,
%%   Full_Calc_Time - full calculation time,
%%   Init_Times - initialization times for each run mode,
%%   Par_Misc_Time - miscellaneous times for parallel calculations support,
%%   Macro_Factor - part of Par_Misc_Times used for calculating macroparameters (ln(N)).
parallel_scaling_analyze(Parallel_Factors,
                         Full_Calc_Time,
                         Init_Time,
                         Par_Misc_Times,
                         Macro_Factor) when is_number(Init_Time) ->

    % If only one init time is given, set this time as init time for all nodes.
    parallel_scaling_analyze(Parallel_Factors, Full_Calc_Time,
                             lists:duplicate(length(Parallel_Factors), Init_Time),
                             Par_Misc_Times, Macro_Factor);

parallel_scaling_analyze([1 | Parallel_Factors_T] = Parallel_Factors,
                         Full_Calc_Time,
                         Init_Times,
                         Par_Misc_Times,
                         Macro_Factor) ->

    % Print all information.
    io:format("Parallel_Scaling_Analyze:~n", []),
    io:format("  Parallel_Factors : ~w~n", [Parallel_Factors]),
    io:format("  Full_Calc_Time : ~w~n", [Full_Calc_Time]),
    io:format("  Init_Times : ~w~n", [Init_Times]),
    io:format("  Par_Misc_Times : ~w~n", [Par_Misc_Times]),
    io:format("  Macro_Factor : ~w~n", [Macro_Factor]),

    % Calculate full times.
    % T(k) = Init_Times(k)
    %        + Par_Misc_Times(k)
    %        + Par_Misc_Times(k) * Macro_Factor * log(Parallel_Factors(k))
    %        + Full_Calc_Time / k
    Logs = jdlib_lists:log(Parallel_Factors),
    Macros = jdlib_lists:mul(Par_Misc_Times, jdlib_lists:mul(Macro_Factor, Logs)),
    Divs = jdlib_lists:dvs(Full_Calc_Time, Parallel_Factors),
    Adds1 = jdlib_lists:add(Init_Times, Par_Misc_Times),
    Adds2 = jdlib_lists:add(Macros, Divs),
    Calc_Times = jdlib_lists:add(Adds1, Adds2),
    io:format("~nCalc_Times : ~w~n", [Calc_Times]),

    % Calculate interpolation of ax + blogx + c + d/x.
    Calc_Times_T = jdlib_lists:tail(Calc_Times),
    Coeffs = jdlib_calc:interpolation_ax_blogx_c_ddivx(lists:zip(Parallel_Factors_T,
                                                                 Calc_Times_T)),
    io:format("~nCalc_Times interpolation : ~w * x + ~w * logx + ~w + ~w / x~n", Coeffs),

    % Find point where y'(x) = 0.
    % a * x + b * logx + c + d / x
    % a + b / x - d / x^2 = 0
    % a * x^2 + b * x - d = 0
    % D = b^2 + 4 * a * d
    % x = (-b (+/-) sqrt(D)) / (2 * a)
    [A, B, _, D] = Coeffs,
    DD = B * B + 4 * A * D,
    if
        DD >= 0 ->
            io:format("y' = 0 in point x = [~w, ~w]~n",
                      [(-B - math:sqrt(DD)) / (2.0 * A), (-B + math:sqrt(DD)) / (2.0 * A)]);
        true ->
            io:format("No point y' = 0~n")
    end,

    % Calculate speed increase.
    Speed_Ups = jdlib_lists:dvs(jdlib_lists:head(Calc_Times), Calc_Times_T),
    io:format("~nSpeed_Ups : ~w~n", [Speed_Ups]),

    ok.

%---------------------------------------------------------------------------------------------------

