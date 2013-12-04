%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This sups the multirequest server and its clients
%%% @end
%%% Created : 23 Nov 2013 by Eric Moritz <eric@themoritzfamily.com>
%%%-------------------------------------------------------------------
-module(simplox_multirequest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, multirequest_pid/1, client_sup_pid/1, logger_pid/1]).

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

multirequest_pid(SupPid) ->
    first(find_children(SupPid, simplox_multirequest_server)).

client_sup_pid(SupPid) ->
    first(find_children(SupPid, http_client_sup)).

logger_pid(SupPid) ->
    first(find_children(SupPid, simplox_multirequest_logger)).

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
    RestartStrategy = one_for_all,

    % we don't restart a multirequest, it just crashes
    % in the future we may want to add retries, but I doubt
    % that it is possible to generalize
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},


    MultiRequestChild = {simplox_multirequest_server,
			 {simplox_multirequest_server, start_link, []},
			 permanent, 1000, worker, 
			 [simplox_multirequest_server]},

    LoggerChild = {simplox_multirequest_logger,
			 {simplox_multirequest_logger, start_link, []},
			 permanent, 1000, worker, 
			 [simplox_multirequest_logger]},

    ClientSup = {http_client_sup, 
		 {http_client_sup, start_link, []}, 
		 permanent, 1000, supervisor, 
		 [http_client_sup]},

    {ok, {SupFlags, [LoggerChild, MultiRequestChild, ClientSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_children(SupPid, Name) ->
    [Pid || {N, Pid, _, _} <- supervisor:which_children(SupPid),
    N == Name].

first([]) ->
    {error, not_found};
first([H|_]) ->
    {ok, H}.
