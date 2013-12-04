%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% This provides event logging service for each multirequest sup
%%% @end
%%% Created :  4 Dec 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
-module(simplox_multirequest_logger).
-include_lib("simplox/include/simplox_pb.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, 
	 multirequest_start/2, multirequest_end/2, 
	 request_start/3, request_end/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mr_start, r_times=dict:new()}).

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
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Casted when the multirequest starts
%% @end
%%--------------------------------------------------------------------
multirequest_start(Pid, MultiRequest) ->
    gen_server:cast(Pid, {multirequest_start, MultiRequest}).

%%--------------------------------------------------------------------
%% @doc
%% Casted when a multirequest ends
%% @end
%%--------------------------------------------------------------------
multirequest_end(Pid, MultiRequest) ->
    gen_server:cast(Pid, {multirequest_end, MultiRequest}).

%%--------------------------------------------------------------------
%% @doc
%% Casted when a request starts
%% @end
%%--------------------------------------------------------------------
request_start(Pid, RequestRef, Request) ->
    gen_server:cast(Pid, {request_start, RequestRef, Request}).

%%--------------------------------------------------------------------
%% @doc
%% Casted when a response ends
%% @end
%%--------------------------------------------------------------------
request_end(Pid, RequestRef, ResponseBin) ->
    gen_server:cast(Pid, {request_end, RequestRef, ResponseBin}).


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
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
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
handle_cast({multirequest_start, _MR}, State) ->
    log_json(
      info,
      State,
      <<"multirequest_start">>,
      []),
    {noreply, start_mr_time(State)};
handle_cast({multirequest_end, _MR}, State) ->
    log_json(
      info,
      State,
      <<"multirequest_end">>,
      [{<<"elapsed">>, us_str(mr_elapsed(State))}]),
    {noreply, State};
handle_cast({request_start, RequestPid, _Request}, State) ->
    log_json(
      info,
      State,
      <<"request_start">>,
      [{<<"request_pid">>, pid_to_binary(RequestPid)}]),
    {noreply, start_r_time(RequestPid, State)};
handle_cast({request_end, RequestPid, ResponseBin}, State) ->
    Response = simplox_pb:decode_response(ResponseBin),
    log_json(
      info,
      State,
      <<"request_end">>,
      [
       {<<"request_pid">>, pid_to_binary(RequestPid)},
       {<<"elapsed">>, us_str(r_elapsed(RequestPid, State))},
       {<<"key">>, cast_str(Response#response.key)},
       {<<"url">>, cast_str(Response#response.url)},
       {<<"method">>, cast_str(Response#response.method)},
       {<<"status">>, cast_str(Response#response.status)},
       {<<"content_length">>, content_length(Response#response.body)}]),
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
log_json(Level, #state{}, Event, JSONData) ->
    JSONPacket = jsx:encode([
		   {<<"pid">>, pid_to_binary(self())},
		   {<<"event">>, Event},
		   {<<"ts">>, now_us(os:timestamp())}
		  ] ++ JSONData),
    log(Level, "application/json, ~p, ~s", [size(JSONPacket), JSONPacket]).


cast_str(undefined) ->
    null;
cast_str(Val) when is_binary(Val) ->
    Val;
cast_str(Val) when is_list(Val) ->
    iolist_to_binary(Val).

content_length(undefined) ->
    null;
content_length(Body) ->
    size(Body).

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

now_us({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

log(debug, Msg, Args) ->
    lager:debug(Msg, Args);
log(info, Msg, Args) ->
    lager:info(Msg, Args).

start_mr_time(State) ->
    State#state{mr_start=os:timestamp()}.

mr_elapsed(#state{mr_start=Start}) ->
    timer:now_diff(os:timestamp(), Start).

start_r_time(Pid, State) ->
    State#state{r_times=dict:store(Pid, os:timestamp(), State#state.r_times)}.

r_elapsed(Pid, #state{r_times=RT}) ->
    timer:now_diff(os:timestamp(), dict:fetch(Pid, RT)).
      
us_str(US) ->
    iolist_to_binary(io_lib:format("~p us", [US])).
