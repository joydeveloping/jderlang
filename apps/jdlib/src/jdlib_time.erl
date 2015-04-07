%% @doc
%% Time realization.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_time).

% Functions export.
-export([today/0, days_dates_dist/2]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

-spec today() -> calendar:date().
%% @doc
%% Get today date.
today() ->
    {D, _} = calendar:local_time(),
    D.

%---------------------------------------------------------------------------------------------------

-spec days_dates_dist(Date_From :: calendar:date(), Date_To :: calendar:date()) -> integer().
%% @doc
%% Days count between two dates.
days_dates_dist(Date_From, Date_To) ->
    calendar:date_to_gregorian_days(Date_To) - calendar:date_to_gregorian_days(Date_From).

%---------------------------------------------------------------------------------------------------

