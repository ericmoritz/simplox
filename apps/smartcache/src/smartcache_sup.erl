
-module(smartcache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % TODO: Make configurable
    CacheMFA = {smartcache_dets_backend, open, ["/tmp/smartcache.dets"]},

    ManagerSup = {smartcache_prefetch_manager_sup, 
		  {smartcache_prefetch_manager_sup, start_link, []},
		  permanent, 2000, supervisor, [smartcache_prefetch_manager_sup]},

    ClientSup = {smartcache_client_sup,
		 {smartcache_client_sup, start_link, [CacheMFA]},
		 permanent, 2000, supervisor, [smartcache_client_sup]},

    {ok, { {one_for_one, 1000, 3600}, [ManagerSup, ClientSup]} }.

