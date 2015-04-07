%% @doc
%% Tests for jdlib_time.
%%
%% @author Alexey Rybakov

% Module name.
-module(jdlib_time_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_time,
        [today/0, days_dates_dist/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec dates_test() -> ok.
%% @doc
%% Tests for dates.
dates_test() ->
    D = today(),
    ?assertEqual(0, days_dates_dist(D, D)),
    ok.

%---------------------------------------------------------------------------------------------------

