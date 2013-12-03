
-module(smartcache_mcd_backend_sup).

-behaviour(supervisor).
-include("smartcache_mcd_backend.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Nodes) ->
    supervisor:start_link(?MODULE, [Nodes]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Nodes]) ->
    MCDCluster = {
      ?CLUSTER_NAME, {mcd_starter, start_link, [?CLUSTER_NAME, Nodes]}, 
      permanent, brutal_kill, worker, [mcd_starter]
    },
    {ok, { {one_for_one, 5, 10}, [MCDCluster]} }.

