%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The cowboy handler that makes the multi-request
%%% @end
%%% Created : 14 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

%%% NOTE: It appears that I can't stream a POST response.
%%% I may need to do some funky stuff to stream out the result in the accept callback.

-module(simplox_multi_request_handler).
-compile([{parse_transform, lager_transform}]).
-record(state, {boundary, multirequest, procs=dict:new(), media_type, start=os:timestamp()}).
-record(proc_item, {request_msg, start}).

-define(CRLF, <<"\r\n">>).
-include_lib("simplox/include/simplox_pb.hrl").

-export([
	 init/3, 
	 rest_init/2,
	 allowed_methods/2,
	 content_types_provided/2, 
	 content_types_accepted/2,
	 multirequest_parser/2,
	 html_get_response/2,
	 streaming_multipart_response/2,
	 rest_terminate/2
	]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
     {<<"application/protobuf+vnd.simplox.multirequest">>, multirequest_parser}
    ], Req, State}.


multirequest_parser(Req, State) ->
    {MediaType, Req1} = cowboy_req:meta(media_type, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    State1 = State#state{media_type=MediaType},
    case decode_multirequest(Body) of 
	{error, Reason} ->
	    Req3 = cowboy_req:set_resp_header(
		     <<"content-type">>,
		     <<"text/plain">>,
		     cowboy_req:set_resp_body(io_lib:format("~s~n", [Reason]), Req2)),
	    {false, Req3, State1};
	{ok, MultiRequest} -> 
	    State2 = spawn_request_procs(MultiRequest, State1),
            {{stream, StreamFun}, Req3, State3} = streaming_multipart_response(Req2, State2),
            Req4 = set_resp_content_type(
		     State3,
		     cowboy_req:set_resp_body_fun(chunked, StreamFun, Req3)),
	    {true, Req4, State3}
    end.

set_resp_content_type(#state{media_type={X = <<"application">>,
					 Y = <<"protobuf+delimited+vnd.simplox.response">>,[]}}, Req) ->
    cowboy_req:set_resp_header(
      <<"content-type">>,
      iolist_to_binary([X, "/", Y]),
      Req);
set_resp_content_type(#state{boundary=Boundary}, Req) ->
    cowboy_req:set_resp_header(
      <<"content-type">>,
      <<"multipart/mixed; boundary=", Boundary>>, Req).


decode_multirequest(Body) ->
    try     
	case simplox_pb:decode_multirequest(Body) of
	    #multirequest{requests=[]} ->
		{error, "MultiRequest.requests required"};
	    Msg ->
		{ok, Msg}
	end
    catch Error ->
	    ErrorStr = io_lib:format("Error decoding body: ~p", [Error]),
	    {error, ErrorStr}
    end.

spawn_request_procs(MultiRequest, State) ->
    State#state{
      multirequest=MultiRequest,
      procs=dict:from_list(lists:map(fun spawn_request/1, 
				     MultiRequest#multirequest.requests))}.


spawn_request(RequestMessage) ->
    Self = self(),
    Start = os:timestamp(),
    {ok, Pid} = http_client_sup:start_child(Self, RequestMessage),
    monitor(process, Pid),
    {Pid, #proc_item{request_msg=RequestMessage, start=Start}}.


content_types_provided(Req, State) ->
    case cowboy_req:method(Req) of
	{<<"GET">>, Req1} ->
	    {[{<<"text/html">>, html_get_response}], Req1, State};
	{<<"POST">>, Req1} ->
	    Boundary = make_boundary(),
	    State1 = State#state{boundary=Boundary},
	    {[
	      {{<<"multipart">>, <<"mixed">>, '*'},
	       '_'},
	      {<<"application/protobuf+delimited+vnd.simplox.response">>,
	       '_'}
	     ], Req1, State1}
    end.


rest_terminate(_Req, _) ->
    %Stop = os:timestamp(),
    %lager:info("~p", [timer:now_diff(Stop, Start) / 1000000]),
    ok.

html_get_response(Req, State) ->
    Body = <<"<html>
<body>
<p>Hello, World!</p>
</body></html>">>,
    {Body, Req, State}.




streaming_multipart_response(Req, State) ->
    {{stream, multipart_streamer(Req, State)}, Req, State}.


multipart_streamer(Req, State) ->
    fun(F) ->
	    stream_loop(Req, State, F)
    end.

stream_loop(Req, State, F) ->
    Result = receive 
		 {http, Pid, {ok, {Status, Headers, Body}}, Props} ->
		     send_response(Pid, 
				   {Status, Headers, Body}, 
				   Props, 
				   F, 
				   State);
		 {http, Pid, {error, Reason}, Props} ->
		     send_error(Pid, Reason, Props, F, State);
		 {'DOWN', _Ref, process, _Pid, normal} ->
		     {continue, State};
		 {'DOWN', _Ref, process, Pid, Reason} ->
		     error_logger:error_msg("Client crash: ~p~n", [Reason]),
		     send_error(Pid, Reason, [], F, State)
	     after 10000 ->
		     io:format("Timeout waiting for responses", []),
		     {done, State}
 	     end,
    case Result of
	{continue, State2} ->
	    stream_loop(Req, State2, F);
	{done, _State2} ->
	    ok
    end.
				  
record_request_time(Pid, Props, #state{procs=Procs}) ->
    End = os:timestamp(),
    #proc_item{start=Start} = dict:fetch(Pid, Procs),
    RequestTime = proplists:get_value(time, Props, 0),
    Overhead = timer:now_diff(End, Start) - RequestTime,
    folsom_metrics:notify({multi_request_overhead, Overhead}).


send_response(Pid, {Status, Headers, Body}, Props, F, State) ->
    #proc_item{request_msg=RequestMessage} = dict:fetch(
					       Pid, 
					       State#state.procs
					      ),
    RequestTime = proplists:get_value(time, Props, 0),
    Headers2 = [{<<"X-Request-Time">>, [integer_to_list(RequestTime), " us"]}
		|Headers],

    F(encode_response({Status, Headers2, Body}, RequestMessage, State)),
    record_request_time(Pid, Props, State),
    after_response(Pid, State).


%% This will change based on the media type
encode_response({Status, Headers, Body}, 
		RequestMessage, 
		#state{media_type={<<"application">>,
				   <<"protobuf+delimited+vnd.simplox.response">>,[]}}) ->
    simplox_pb:encode(
      [
       #response{
	  url=RequestMessage#request.url, 
	  status=Status, 
	  headers=[#header{key=N, value=V} || {N,V} <- Headers],
	  body=Body,
	  key=RequestMessage#request.key
	 }]);
encode_response({Status, Headers, Body}, RequestMessage, State) ->
    Headers2 = [{<<"X-Status">>, Status}, 
		{<<"Content-Location">>, RequestMessage#request.url}]
	++ [{<<"X-Simplox-Key">>, RequestMessage#request.key} ||
	       RequestMessage#request.key =/= undefined]
	++ Headers,
    [
     ?CRLF, <<"--">>, State#state.boundary, ?CRLF,
     lists:map(fun header/1, Headers2),
     ?CRLF, ?CRLF, 
     Body].


send_error(Pid, Reason, Props, F, State) ->
    FakeResult = {
      "500", 
      [{<<"Content-Type">>, <<"text/plain">>}], 
      io_lib:format("~p", [Reason])
    },
    send_response(
      Pid, 
      FakeResult,
      Props,
      F,
      State).

    
after_response(Pid, State) ->
    State2 = State#state{procs=dict:erase(Pid, State#state.procs)},
    case dict:size(State2#state.procs) of
	0 ->
	    {done, State2};
	_ ->
	    {continue, State2}
    end.

header({Name, Value}) ->
    [Name, ": ", Value, ?CRLF].

make_boundary() ->
    % TODO: Do a random boundary function
    <<"gc0p4Jq0M2Yt08jU534c0p">>.

