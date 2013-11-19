
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
    folsom_metrics:new_counter(multi_request_running),
    folsom_metrics:new_histogram(multi_request_overhead),
    Dispatch = cowboy_router:compile(
		 [
		  {'_', [
			 {"/service/v1/multi-request/", simplox_multi_request_handler, []}
			]}
		 ]),
    ClientSup = {http_client_sup, 
		 {http_client_sup, start_link, []}, 
		 permanent, 1000, supervisor, 
		 [http_client_sup]},

    CowboySpec = ranch:child_spec(
		   simplox, 10, 
		   ranch_tcp, [{port, Port}],
		   cowboy_protocol, [{env, [{dispatch, Dispatch}]}]),

    {ok, { {one_for_one, 1000, 10}, [ClientSup, CowboySpec]} }.

