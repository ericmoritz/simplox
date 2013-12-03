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

-record(state, {old_age}).
-record(timerdata, {key, gen, timeout, ref, birthdate}).
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
    % old age defaults to 60 seconds
    {ok, #state{old_age=timer:seconds(20)}}.

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
    case find_timer(Key, ValueGenMFA, Timeout) of
	% if not found, reset the birthdata and start the timer
	{false, TimerData} ->
	    lager:info("notify: new timer ~p", [TimerData]),
	    start_timer(reset_birthdate(TimerData));
	% if found, reset the birthdate and store it
	{true, TimerData} ->
	    lager:debug("notify: existing timer ~p", [TimerData]),
	    store_timer(Key, reset_birthdate(TimerData))
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
handle_info({update, TD=#timerdata{key=Key, gen=ValueGenMFA, timeout=Timeout}}, 
	    State) ->
    case find_timer(Key, ValueGenMFA, Timeout) of
	{true, TimerData} ->
	    Age = timer:now_diff(os:timestamp(), TimerData#timerdata.birthdate) / 1000,
	    if % if the timer has not aged out, refresh
		Age =< State#state.old_age ->
		    smartcache_client:refresh(Key, ValueGenMFA, Timeout),
                    start_timer(TimerData);
		true -> % If the timer has aged out, delete the timer and its data
		    lager:debug("~p is old ~p =< ~p, deleting", 
			       [Key, Age, State#state.old_age]),
		    delete_timer(TimerData)
	    end;
	{false, _} ->
	    lager:debug("stale timer update, purging: ~p", [TD]),
	    delete_timer(TD)
    end,
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
    ets:foldl(fun({_, TimerData}, Acc) ->
		      start_timer(TimerData),
		      Acc
	      end,
	      0,
	      ?MANAGER_KEY_TAB).


cancel_timer(undefined) ->
    false;
cancel_timer(Ref) ->
    erlang:cancel_timer(Ref).

start_timer(TimerData=#timerdata{timeout=Timeout, key=Key}) ->
    lager:debug("Starting timer for ~p", [TimerData]),

    %% cancel the existing timer if scheduled
    cancel_timer(TimerData#timerdata.ref),
    %% start the new timer 
    Ref = erlang:send_after(Timeout * 1000, self(), 
			    {update, TimerData}),

    store_timer(Key, TimerData#timerdata{ref=Ref}),
    ok.
    
reset_birthdate(TimerData) ->
    TimerData#timerdata{birthdate=os:timestamp()}.

find_timer(Key, ValueMFA, Timeout) ->
    case ets:lookup(?MANAGER_KEY_TAB, Key) of 
	[{Key, TimerData=#timerdata{key=Key, gen=ValueMFA, timeout=Timeout}}] ->
		{true, TimerData};
	[] ->
            % if not found, create a new one.
	    {false, 
	     reset_birthdate(#timerdata{key=Key, gen=ValueMFA, timeout=Timeout})}
	end.

store_timer(Key, TimerData) ->
    ets:insert(?MANAGER_KEY_TAB, {Key, TimerData}).

delete_timer(#timerdata{key=Key, ref=Ref}) ->
    cancel_timer(Ref),
    smartcache_client:delete(Key),
    ets:delete(?MANAGER_KEY_TAB, Key).
    
