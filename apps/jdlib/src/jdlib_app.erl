-module(jdlib_app).

-behaviour(application).

% Export functions.
-export([start/2, stop/1]).

%---------------------------------------------------------------------------------------------------
% Callback functions.
%---------------------------------------------------------------------------------------------------

start(_Start_Type, _Start_Args) ->
    jdlib_sup:start_link().

%---------------------------------------------------------------------------------------------------

stop(_State) ->
    ok.

%---------------------------------------------------------------------------------------------------

