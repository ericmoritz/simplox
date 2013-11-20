-module(simplox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-define(APP, simplox).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    simplox_sup:start_link(simplox_conf:init()).

stop(_State) ->
    ok.

