%% @doc
%% Time realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_time).

% Functions export.
-export([days_dates_dist/2]).

%---------------------------------------------------------------------------------------------------
% Functions.
%---------------------------------------------------------------------------------------------------

-spec days_dates_dist(Date_From :: calendar:date(), Date_To :: calendar:date()) -> integer().
%% @doc
%% Days count between two dates.
days_dates_dist(Date_From, Date_To) ->
    calendar:date_to_gregorian_days(Date_To) - calendar:date_to_gregorian_days(Date_From).

%---------------------------------------------------------------------------------------------------

