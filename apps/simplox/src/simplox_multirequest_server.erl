%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This cordinates the multirequest
%%% @end
%%% Created : 23 Nov 2013 by Eric Moritz <eric@themoritzfamily.com>
%%%-------------------------------------------------------------------
-module(simplox_multirequest_server).
-include_lib("simplox/include/simplox_pb.hrl").
-behaviour(gen_fsm).

%% API
-export([start_link/0, fetch/2, responses/2]).

%% gen_fsm callbacks
-export([init/1, waiting/3,  started/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {multirequest, 
		procs=dict:new(), 
		start=os:timestamp(),
		client_sup_pid,
		responses=[],
		waiting=[]
	       }).
-record(proc_item, {request_msg, start}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new multirequest
%%
%% @spec fetch(pid(), #multirequest{}) -> 
%%     {ok, started} | {error, started}
%% @end
%%--------------------------------------------------------------------
fetch(SupPid, MultiRequest) ->
    {ok, Pid} = simplox_multirequest_sup:multirequest_pid(SupPid),
    {ok, ClientSupPid} = simplox_multirequest_sup:client_sup_pid(SupPid),
    gen_fsm:sync_send_event(Pid, 
			    {fetch, ClientSupPid, MultiRequest}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of #response{} protobuf binaries
%% @spec responses(pid(), Timeout :: integer() | infinity) -> 
%%     {ok, {done | continue, [binary()]}} | {error, not_started}
%% @end
%%--------------------------------------------------------------------
responses(SupPid, Timeout) ->
    {ok, Pid} = simplox_multirequest_sup:multirequest_pid(SupPid),
    gen_fsm:sync_send_event(Pid, responses, Timeout).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Timeout = 10000,
    {ok, waiting, #state{}, Timeout}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
waiting({fetch, ClientSupPid, MultiRequest}, _From, State) ->
    State2 = spawn_request_procs(ClientSupPid, MultiRequest, State),
    Reply = {ok, started},
    {reply, Reply, started, State2#state{client_sup_pid=ClientSupPid}};
waiting(responses, _From, State) ->
    {reply, {error, not_started}, waiting, State}.

started({fetch, _,_}, _From, State) ->
    {reply, {error, started}, started, State};
started(responses, From, 
	State=#state{responses=Resp, procs=Procs, waiting=Waiting}
       ) ->
    case {Resp, dict:size(Procs)} of
	{Resp, 0} ->
	    {Reply, State2} = responses_reply(Resp, State), 
	    {reply, 
	     Reply,
	     started,
	     State2};
	{[], _} ->
	    % no collected responses, push the From onto the wait list
	    % and defer the reply
	    {next_state, started, State#state{waiting=[From|Waiting]}};
	{Resp, _} ->
	    % reply with the collected responses regardless of
	    % the size
	    {Reply, State2} = responses_reply(Resp, State), 
	    {reply, 
	     Reply,
	     started,
	     State2}
    end.
    % if not responses are waiting to be collected


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({http, Pid, {ok, {response_bin, ResponseBin}}}, 
	    started, 
	    State=#state{waiting=[], responses=Responses}) ->
    % if no one is waiting for a response, push the response onto the 
    % stack
    Resp2 = [ResponseBin|Responses],
    State2 = after_response(Pid, State),
    {next_state, started, State2#state{responses=Resp2}};
handle_info({http, Pid, {ok, {response_bin, ResponseBin}}}, 
	    started,
	   State=#state{waiting=Waiting, responses=Responses}) ->
    % if someone is wating, push the resp onto the stack,
    % update the state and reply to them
    {Reply, State2} = responses_reply(
			[ResponseBin|Responses],
			State),
    State3 = after_response(Pid, State2),
    % send replies
    [gen_fsm:reply(From, Reply) || From <- Waiting],
    % continue
    {next_state, started, State3#state{waiting=[]}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, _, State) ->
    % make sure the proc is removed
    State2 = after_response(Pid, State),
    {next_state, started, State2};
handle_info(timeout, _, State) ->
    io:format("Timeout waiting for responses", []),
    {stop, timeout, State}.
		     


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
spawn_request_procs(ClientSupPid, MultiRequest, State) ->
    State#state{
      multirequest=MultiRequest,
      procs=dict:from_list(lists:map(fun(Req) -> spawn_request(ClientSupPid, Req) end, 
				     MultiRequest#multirequest.requests))}.

spawn_request(ClientSupPid, RequestMessage) ->
    Self = self(),
    Start = os:timestamp(),
    {ok, Pid} = http_client_sup:start_child(
		  ClientSupPid, Self, RequestMessage),
    monitor(process, Pid),
    {Pid, #proc_item{request_msg=RequestMessage, start=Start}}.

responses_reply(Responses, State=#state{procs=Procs}) ->
    State2 = State#state{responses=[]},
    Reply = case dict:size(Procs) of
		0 ->
		    {ok, {done, Responses}};
		_ ->
		    {ok, {continue, Responses}}
	    end,
    {Reply, State2}.

after_response(Pid, State) ->
    State#state{procs=dict:erase(Pid, State#state.procs)}.
