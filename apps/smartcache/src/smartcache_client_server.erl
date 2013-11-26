%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This gen_server handles talking to the cache service and talking to the 
%%% worker manager if values are inaccessible
%%% @end
%%% Created : 25 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
%%%-------------------------------------------------------------------
-module(smartcache_client_server).

-behaviour(gen_server).
-include("smartcache.hrl").
%% API
-export([start_link/1, get/4, refresh/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {backend_mod}).

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
start_link(BackendMod) ->
    gen_server:start_link(?MODULE, [BackendMod], []).


%%--------------------------------------------------------------------
%% @doc
%% Requests the value from the cache
%% 
%% @spec
%% @end
%%--------------------------------------------------------------------

get(Pid, Key, ValueGenMFA, Timeout) ->
    gen_server:call(Pid, {get, Key, ValueGenMFA, Timeout}).


%%--------------------------------------------------------------------
%% @doc
%% Tells the cache to refresh the value; casted by the manager
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec refresh(pid(), iodata(), mfa(), seconds()) -> ok | {error, any()}.
refresh(Pid, Key, ValueGenMFA, Timeout) ->
    gen_server:cast(Pid, {refresh, Key, ValueGenMFA, Timeout}).

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
init([BackendMod]) ->
    {ok, #state{backend_mod=BackendMod}}.

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
handle_call({get, Key, ValueMFA, Timeout}, _From, State) ->
    % notify the prefetch manager of the existance of this key
    smartcache_prefetch_manager:notify(Key, ValueMFA, Timeout),
    Result = get_or_set(Key, ValueMFA, Timeout, State),
    {reply, Result, State};
handle_call(Msg, _From, State) ->
    lager:error("Unknown MSG: ~p", [Msg]),
    {reply, {error, who_is_this_stop_calling_me}, State}.



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
handle_cast({refresh, Key, ValueGenMFA, Timeout}, State) ->
    update_if_not_found({error, not_found}, Key, ValueGenMFA, Timeout, State),
    % run the valuefun and store the result,
    {noreply, State};
handle_cast(Msg, State) ->
    lager:error("Unknown MSG: ~p", [Msg]),
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
handle_info(_Info, State) ->
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
-spec get_or_set(key(), mfa(), seconds(), #state{}) -> {ok, iodata()} | {error, any()}.
get_or_set(Key, MFA, Timeout, State=#state{backend_mod=Mod}) ->
    update_if_not_found(Mod:get(Key), Key, MFA, Timeout, State).

-spec update_if_not_found({ok, key()} | {error, not_found}, 
      key(), mfa(), seconds(), #state{}) -> {ok, value()}.
update_if_not_found({ok, Value}, _, _, _, _) ->
    {ok, Value}; % pass through
update_if_not_found({error, not_found}, Key, {M,F,A}, Timeout, 
		    #state{backend_mod=Mod}) ->
    lager:info("Cache Miss: ~p", [{Key, {M,F,A}, Timeout}]),
    case erlang:apply(M,F,A) of 
	E={error,_} ->
	    % Don't store an error
	    E;
	{ok, Value} ->
	    Mod:set(Key, Value, Timeout),
	    {ok, Value}
    end;
% pass through any unknown errors
update_if_not_found(E={error, _}, _, _, _, _) ->
    E.

