
-module(simplox_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    Dispatch = cowboy_router:compile(
		 [
		  {'_', [
			 {"/service/v1/multi-request/", simplox_multi_request_handler, []}
			]}
		 ]),
    CowboySpec = ranch:child_spec(
		   simplox, 100, 
		   ranch_tcp, [{port, Port}],
		   cowboy_protocol, [{env, [{dispatch, Dispatch}]}]),
    {ok, { {one_for_one, 5, 10}, [CowboySpec]} }.

