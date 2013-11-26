%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% A simple DETS based backend for smartcache; the 2GB limit is probably
%%% to small for a production system (*cough* Riak *cough*).
%%% @end
%%% Created : 26 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
%%%-------------------------------------------------------------------
-module(smartcache_dets_backend).

%% API
-export([start_link/1, get/1, set/3]).
-include("smartcache.hrl").
-type dets_name() :: any().

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {tab}).


%%%===================================================================
%%% API
%%%===================================================================


start_link(Name) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [Name], []).


%%--------------------------------------------------------------------
%% @doc
%% Get the value into the dets table
%%
%% @end
%%--------------------------------------------------------------------
-spec get(key()) -> {ok, value()} | {error, not_found}.
get(Key) ->
    gen_server:call(?SERVER, {get, Key}).
	    
%%--------------------------------------------------------------------
%% @doc
%% Store a value into the dets bag
%%
%% @end
%%--------------------------------------------------------------------
-spec set(key(), value(), seconds()) -> ok | {error, any()}.
set(Key, Value, _Timeout) ->
    gen_server:call(?SERVER, {set, Key, Value}).


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
init([Name]) ->
    % I assume that opening, doing one operation and then closing the file
    % is probably not good but for development, I'm going to ignore that fact.
    {ok, Name} = dets:open_file(
		   Name,
		   [
		    {auto_save, 500}
		   ]),
    {ok, #state{tab=Name}}.

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
handle_call({get, Key}, _From, State) ->
    Reply = case dets:lookup(State#state.tab, key(Key)) of
	[{Key, Value}] ->
	    {ok, Value};
	_ ->
	    {error, not_found}
	    end,
    {reply, Reply, State};
handle_call({set, Key, Value}, _From, State) ->
    Reply = dets:insert(State#state.tab, {key(Key), Value}),
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
terminate(_Reason, State) ->
    dets:close(State#state.tab),
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
key(Key) -> iolist_to_binary(Key).
     
