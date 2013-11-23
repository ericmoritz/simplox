%%%-------------------------------------------------------------------
%%% @author Moritz <emoritz@usat-babramson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% A simple one-to-one sup for the http_request processes
%%% @end
%%% Created : 19 Nov 2013 by Moritz <emoritz@usat-babramson.usatoday.us.ad.gannett.com>
%%%-------------------------------------------------------------------
-module(http_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

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
    supervisor:start_link(?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a request
%%
%% @spec start_child(TargetPid, RequestMessage) -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_child(SupPid, TargetPid, RequestMessage) ->
    supervisor:start_child(SupPid, [TargetPid, RequestMessage]).


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
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    % may use 'transient' to add retries, but we'll hold off on that for
    % now.
    Restart = temporary, 
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {http_client, {http_client, start_link, []},
	      Restart, Shutdown, Type, [http_client]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
