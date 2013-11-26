%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This service keeps track of the key to mfa mapping and the timeouts
%%% @end
%%% Created : 25 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
%%%-------------------------------------------------------------------
-module(smartcache_prefetch_manager).

-behaviour(gen_server).
-include("smartcache.hrl").

%% API
-export([start_link/0, notify/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    % We may be restarted, start all the known timers again.
    start_timers(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Notifies the manager of a key; this overwrites and reschedules
%% existing keys if the parameters have changed.
%% 
%% @end
%%--------------------------------------------------------------------
notify(Key, ValueGenMFA, Timeout) ->
    gen_server:cast(?SERVER, {notify, Key, ValueGenMFA, Timeout}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    lager:error("Unknown MSG: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({notify, Key, ValueGenMFA, Timeout}, State) ->
    % check the existance of the key.
    case ets:lookup(?MANAGER_KEY_TAB, Key) of
	% If the parameters haven't changed, do nothing
	[{Key, ValueGenMFA, Timeout, _}] -> 
	    pass;
	% if missing or changed, reschedule the timer
	_ ->
	    start_timer(Key, ValueGenMFA, Timeout)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({update, Key, ValueGenMFA, Timeout}, State) ->
    % cast the update to a cache client
    smartcache_client:refresh(Key, ValueGenMFA, Timeout),
    % reschedule the timer
    start_timer(Key, ValueGenMFA, Timeout),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_timers() ->
    % start all the existing timers
    ets:foldl(fun({Key, ValueMFA, Timeout, _}, Acc) ->
		      start_timer(Key, ValueMFA, Timeout),
		      Acc
	      end,
	      0,
	      ?MANAGER_KEY_TAB).

start_timer(Key, ValueMFA, Timeout) ->
    lager:info("Starting timer for ~p", [{Key, ValueMFA, Timeout}]),
    %% cancel the existing timer
    lists:foreach(fun({_, _, _, Ref}) -> erlang:cancel_timer(Ref) end,
		  ets:lookup(?MANAGER_KEY_TAB, Key)),

    %% start the new timer 
    Ref = erlang:send_after(Timeout * 1000, self(), 
			    {update, Key, ValueMFA, Timeout}),
    %% update the record
    ets:insert(?MANAGER_KEY_TAB, {Key, ValueMFA, Timeout, Ref}),
    ok.
