-module(simplox_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-define(APP, simplox).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = port(),
    simplox_sup:start_link(Port).

stop(_State) ->
    ok.


%% ===================================================================
%% Internal
%% ===================================================================
-spec port() -> integer().
port() ->
    port(
      os:getenv("PORT"),
      application:get_env(?APP, port)
     ).

port(false, undefined) ->
    8000;
port(OSVar, undefined) ->
    list_to_integer(OSVar);
port(_, AppVar) ->
    AppVar.


