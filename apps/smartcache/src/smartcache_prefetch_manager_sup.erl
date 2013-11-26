%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The supervisor of the smartcache_prefetch_manager
%%% @end
%%% Created : 25 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
%%%-------------------------------------------------------------------
-module(smartcache_prefetch_manager_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    % the ETS process to make sure it doesn't die if the manager dies
    ETSChild = {smartcache_prefetch_manager_ets, 
		{smartcache_prefetch_manager_ets, start_link, []},
		permanent, 2000, worker, [smartcache_prefetch_manager_ets]
	       },

    % start the manager to keep track of the timers
    ManagerChild = {smartcache_prefetch_manager, 
		    {smartcache_prefetch_manager, start_link, []},
	      permanent, 2000, worker, [smartcache_prefetch_manager]},

    {ok, {{one_for_one, 1000, 3600}, [ETSChild, ManagerChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
