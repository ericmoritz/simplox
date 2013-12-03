
-module(smartcache_mcd_backend_sup).

-behaviour(supervisor).
-include("smartcache_mcd_backend.hrl").

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Peers, AdditionalPoolProps) ->
    supervisor:start_link(?MODULE, [Peers, AdditionalPoolProps]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Peers, AdditionalPoolProps]) ->
    PoolArgs = [
		{name, {local, ?POOL_NAME}},
		{worker_module, smartcache_mcd_pool_worker}
	       ] ++ AdditionalPoolProps,
    WorkerArgs = [{peers, Peers}],
    PoolSpec = poolboy:child_spec(?POOL_NAME, PoolArgs, WorkerArgs),
    {ok, { {one_for_one, 5, 10}, [PoolSpec]} }.

