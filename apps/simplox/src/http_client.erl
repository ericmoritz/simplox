%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% An async http client for the multirequests
%%% @end
%%% Created : 15 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
-module(http_client).
-behaviour(gen_server).
-define(CLIENT, httpc).

-include_lib("simplox/include/simplox_pb.hrl").

%% API
-export([start_link/2, start/2, send_req/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {target, msg}).

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
start_link(TargetPid, RequestMessage) ->
    gen_server:start_link(?MODULE, [TargetPid, RequestMessage], []).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(TargetPid, RequestMessage) ->
    gen_server:start(?MODULE, [TargetPid, RequestMessage], []).


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
init([TargetPid, RequestMessage]) ->
    {ok, #state{target=TargetPid, msg=RequestMessage}, 0}.

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
handle_info(timeout, State=#state{target=TargetPid, msg=RequestMsg}) ->
    make_request(TargetPid, RequestMsg),
    {stop, normal, State}; 
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
make_request(TargetPid, RequestMessage) ->
    folsom_metrics:notify({multi_request_running, {inc, 1}}),    
    Result = send_req_cached(RequestMessage),
    folsom_metrics:notify({multi_request_running, {dec, 1}}),    
    TargetPid ! {http, self(), Result}.

send_req_cached(RequestMessage=#request{cache=undefined}) ->
    send_req(
      key(RequestMessage),
      url(RequestMessage),
      headers(RequestMessage),
      content_type(RequestMessage),
      method(RequestMessage),
      body(RequestMessage));
send_req_cached(RequestMessage=#request{cache=Cache}) ->
    Args = [key(RequestMessage),
	    url(RequestMessage),
	    headers(RequestMessage),
	    content_type(RequestMessage),
	    method(RequestMessage),
	    body(RequestMessage)],
    smartcache_client:get(Cache#cache.key,
			  {http_client, send_req, Args},
			  Cache#cache.timeout).


send_req(Key, Url, Headers, ContentType, Method, undefined) ->
    send_req(Key, Url, Headers, ContentType, Method, []);
send_req(Key, Url, Headers, ContentType, Method, Body) ->
    response(Key, Url, timer:tc(fun() -> 
			      reply(client_send_req(?CLIENT, Url, Headers, ContentType, Method, Body))
		      end)).

client_send_req(lhttpc, Url, Headers, _ContentType, Method, Body) ->
    lhttpc:request(Url, Method, Headers, Body, infinity);    
client_send_req(ibrowse, Url, Headers, _ContentType, Method, Body) ->
    %ibrowse:send_req(Url, Headers, Method, Body, []);
    ibrowse:send_req(Url, Headers, Method, Body, []);
client_send_req(httpc, Url, Headers, undefined, Method, _Body) ->
    httpc:request(Method, {Url, Headers}, [], []);
client_send_req(httpc, Url, Headers, ContentType, Method, Body) ->
    httpc:request(Method, {Url, Headers, ContentType, Body}, [], []).


%%% Normalize the reply for the various http clients
reply({ok, {{_Vsn, StatusCode, _ReasonPhrase}, Headers, Body}}) -> % httpc
    {ok, {integer_to_list(StatusCode), Headers, Body}};
reply({ok, {{StatusCode, _ReasonPhrase}, Headers, Body}}) -> % lhttpc
    {ok, {integer_to_list(StatusCode), Headers, Body}};
reply({ok, StatusCode, Headers, Body}) -> % ibrowse
    {ok, {StatusCode, Headers, Body}};
reply(E={error, _}) ->
    E.

response(Key, Url, {RequestTime, {ok, {Status, Headers, Body}}}) ->
    Resp = #response{
       key=Key,
       url=Url, 
       status=Status, 
       headers=[#header{key=N, value=V} || {N,V} <- Headers],
       body=Body,
       request_time=RequestTime},
    simplox_logger:log_response(Resp),
    {ok, iolist_to_binary(simplox_pb:encode_response(Resp))}.

headers(RequestMessage=#request{content_type=CT}) ->
    [{<<"content-type">>, CT} || CT =/= undefined] ++ 
	[{Header#header.key, Header#header.value} || Header <- RequestMessage#request.headers].

url(RequestMessage) ->
    binary_to_list(RequestMessage#request.url).

key(RequestMessage) ->
    RequestMessage#request.key.

body(RequestMessage) ->
    RequestMessage#request.body.

method(R=#request{method=Method}) when is_binary(Method)->
    method(R#request{method=binary_to_list(Method)});
method(#request{method=Method}) when is_list(Method)->
    list_to_atom(Method).

content_type(#request{content_type=CT}) ->
    CT.
     

