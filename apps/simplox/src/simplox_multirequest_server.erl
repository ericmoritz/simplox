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
-export([init/1, waiting/3,  started/3, stopped/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {multirequest, 
		procs=dict:new(), 
		start=os:timestamp(),
		client_sup_pid,
		log,
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
    {ok, LogPid} = simplox_multirequest_sup:logger_pid(SupPid),
    gen_fsm:sync_send_event(Pid, 
			    {fetch, ClientSupPid, LogPid, MultiRequest}).

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

init([]) ->
    Timeout = 60000,
    {ok, waiting, #state{}, Timeout}.

%%--------------------------------------------------------------------
%% @doc
%% We start in the waiting state, we only accept a single fetch event
%% @end
%%--------------------------------------------------------------------
waiting({fetch, ClientSupPid, LogPid, MultiRequest}, _From, State) ->
    simplox_multirequest_logger:multirequest_start(LogPid, MultiRequest),
    State2 = spawn_request_procs(ClientSupPid, MultiRequest, 
				 State#state{log=LogPid}),
    Reply = {ok, started},
    {reply, Reply, started, State2#state{client_sup_pid=ClientSupPid}};
waiting(_, _From, State) ->
    {reply, {error, not_started}, waiting, State}.

%%--------------------------------------------------------------------
%% @doc
%% Once started, we only accept responses events
%% @end
%%--------------------------------------------------------------------
started(responses, From, State) ->
    case {responses_recieved(State), still_pending(State)} of
	% no responses pending and no more running clients
	% we're done.
	{false, false} ->
	    % reply with a done response
	    {Reply, State2} = responses_reply({[], State}, false), 
	    % switch to the stopped state, we're done.
	    {reply, log_reply(Reply, State2), stopped, State2};
	{true, _} ->
	    % if we've collect some responses, send them to whoever is
	    % waiting including ourselves.
	    State2 = reply_to_waiting(
		       push_waiting(
			 From,
			 State)),
	    {next_state, started, State2};
	{false, true} ->
	    % If we haven't collected any responses but we're still running,
	    % push the client onto the waiting list
	    {next_state, started, push_waiting(From, State)}
    end;
started(_, _From, State) ->
    {reply, {error, started}, started, State}.

%%--------------------------------------------------------------------
%% @doc
%% We eventually move into the stopped state when all the responses
%% are collected
%% @end
%%--------------------------------------------------------------------
stopped(responses, _, State) ->
    {reply, {done, []}, stopped, State};
stopped(_, _, State) ->
    {reply, {error, stopped}, stopped, State}.

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
handle_event(done, _StateName, State) ->
    {next_state, stopped, State}.

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
handle_info({http, Pid, {ok, ResponseBin}}, 
	    started, 
	    State=#state{waiting=[]}) ->
    % if no one is waiting, just push the responses 
    % using on_responsebin and continue
    on_responsebin(
      Pid,
      ResponseBin, 
      State,
      fun(State2) ->
	      {next_state, started, State2}
      end);
handle_info({http, Pid, {ok, ResponseBin}}, 
	    started,
	    State) ->
    % if clients are waiting, reply to waiting
    on_responsebin(
      Pid,
      ResponseBin,
      State,
      fun(State2) -> {next_state, started, reply_to_waiting(State2)} end
    );
handle_info({'DOWN', _Ref, process, Pid, _Reason}, Status, State) ->
    {next_state, Status, proc_delete(Pid, State)};
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

%%--------------------------------------------------------------------
%% @doc
%% Spawn the request procecsses
%% These concurrently fetch the resources
%% @end
%%--------------------------------------------------------------------
-spec spawn_request_procs(pid(), #multirequest{}, #state{}) -> #state{}.
spawn_request_procs(ClientSupPid, MultiRequest, State) ->
    lists:foldl(
      fun(Req, StateAcc) -> spawn_request(ClientSupPid, Req, StateAcc) end,
      State#state{multirequest=MultiRequest},
      MultiRequest#multirequest.requests
     ).

-spec spawn_request(pid(), #request{}, #state{}) -> #state{}.
spawn_request(ClientSupPid, RequestMessage, State=#state{log=Log}) ->
    {ok, Pid} = http_client_sup:start_child(ClientSupPid, self(), RequestMessage),
    simplox_multirequest_logger:request_start(Log, Pid, RequestMessage),
    monitor(process, Pid),
    proc_add(Pid, 
	     #proc_item{request_msg=RequestMessage, start=os:timestamp()}, 
	     State).

%%--------------------------------------------------------------------
%% @doc
%% Creates a response reply based on the current state
%% @end
%%--------------------------------------------------------------------
-spec responses_reply(#state{}) -> {{ok, {done | continue, [binary()]}}, #state{}}.
responses_reply(State) ->
    responses_reply(pop_responses(State), still_pending(State)).

-spec responses_reply({[binary()], #state{}}, integer()) 
		     -> {{ok, {done | continue, [binary()]}}, #state{}}.
responses_reply({Responses, State}, false) ->
    {{ok, {done, Responses}}, State};
responses_reply({Responses, State}, _) ->
    {{ok, {continue, Responses}}, State}.

%%--------------------------------------------------------------------
%% @doc
%% A generic function that handles incomming response binaries
%% It does the general setup and clean before the actual handler does its
%% work.
%% @end
%%--------------------------------------------------------------------
-type on_responsebin_cb() :: fun((#state{}) -> #state{}).
-spec on_responsebin(pid(), binary(), #state{}, on_responsebin_cb()) -> #state{}.
on_responsebin(RequestPid, ResponseBin, State, F) ->
    % push the response onto the response stack and 
    % remove the request pid from the dict
    % log the response
    simplox_multirequest_logger:request_end(State#state.log, RequestPid, ResponseBin),
    F(
      proc_delete(
	RequestPid,
	push_response(ResponseBin, State))).



%%%===================================================================
%%% State field functions
%%%===================================================================
responses_recieved(#state{responses=[]}) ->
    false;
responses_recieved(_) ->
    true.

still_pending(#state{procs=Procs}) ->    
    case dict:size(Procs) of
	0 ->
	    false;
	_ ->
	    true
    end.

proc_add(Pid, ProcItem, State) ->
    State#state{procs=dict:store(Pid, ProcItem, State#state.procs)}.

proc_delete(RequestPid, State) ->
    State#state{procs=dict:erase(RequestPid, State#state.procs)}.    


pop_responses(State) ->
    {State#state.responses, State#state{responses=[]}}.

push_response(ResponseBin, State=#state{responses=ResponseBins}) ->
    State#state{responses=[ResponseBin|ResponseBins]}.


pop_waiting(State) ->
    {State#state.waiting, State#state{waiting=[]}}.

push_waiting(From, State) ->
    State#state{waiting=[From|State#state.waiting]}.

reply_to_waiting(State) ->
    {Reply, State2} = responses_reply(State),
    {Waiting, State3} = pop_waiting(State2),
    [gen_fsm:reply(From, Reply) || From <- Waiting],
    log_reply(Reply, State),
    stop_if_done(Reply),
    State3.

log_reply({ok, {done, _}}, State) ->
    simplox_multirequest_logger:multirequest_end(State#state.log, State#state.multirequest),
    true;
log_reply(_, _) ->
    false.


stop_if_done({ok, {done, _}}) ->
    gen_fsm:send_all_state_event(self(), done),
    true;
stop_if_done(_) ->
    false.
