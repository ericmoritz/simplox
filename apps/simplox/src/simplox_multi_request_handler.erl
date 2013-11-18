%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The cowboy handler that makes the multi-request
%%% @end
%%% Created : 14 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

%%% NOTE: It appears that I can't stream a POST response.
%%% I may need to do some funky stuff to stream out the result in the accept callback.

-module(simplox_multi_request_handler).

-record(state, {boundary, multirequest, procs=dict:new()}).

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
	 send_multipart_response/7
	]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
     {<<"application/protobuf">>, multirequest_parser}
    ], Req, State}.


multirequest_parser(Req, State=#state{boundary=Boundary}) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    MultiRequest = simplox_pb:decode_multirequest(Body),
    State2 = spawn_request_procs(MultiRequest, State),
    %%% It seems like I have to manually do this for POSTs... though I may be doing
    %%% something wrong.
    {{stream, StreamFun}, Req3, State3} = streaming_multipart_response(Req2, State2),
    Req4 = cowboy_req:set_resp_header(
	     <<"content-type">>,
	     <<"multipart/mixed; boundary=", Boundary/binary>>,
	     cowboy_req:set_resp_body_fun(StreamFun, Req3)),
    {true, Req4, State3}.

log(Msg, Val) ->
    error_logger:info_msg(Msg ++ ": ~p~n", [Val]),
    Val.

spawn_request_procs(MultiRequest, State) ->
    State#state{
      multirequest=MultiRequest,
      procs=dict:from_list(lists:map(fun spawn_request/1, 
				     MultiRequest#multirequest.requests))}.


spawn_request(RequestMessage) ->
    Self = self(),
    Pid = http_client:start(Self, RequestMessage),
    monitor(process, Pid),
    {Pid, RequestMessage}.


content_types_provided(Req, State) ->
    case cowboy_req:method(Req) of
	{<<"GET">>, Req1} ->
	    {[{<<"text/html">>, html_get_response}], Req1, State};
	{<<"POST">>, Req1} ->
	    Boundary = make_boundary(),
	    State1 = State#state{boundary=Boundary},
	    {[
	      %% TODO: Make boundary work.
	      {{<<"multipart">>, <<"mixed">>, '*'},
	       streaming_multipart_response}
	     ], Req1, State1}
    end.


html_get_response(Req, State) ->
    Body = <<"<html>
<body>
<p>Hello, World!</p>
</body></html>">>,
    {Body, Req, State}.


streaming_multipart_response(Req, State) ->
    {{stream, multipart_streamer(Req, State)}, Req, State}.


multipart_streamer(Req, State) ->
    fun(Socket, Transport) ->
	    stream_loop(Req, State, Socket, Transport)
    end.


stream_loop(Req, State, Socket, Transport) ->
    Result = receive 
		 {http, Pid, {ok, {Status, Headers, Body}}} ->
		     send_multipart_response(Pid, Status, Headers, Body, 
					     Socket, Transport, State);
		 {http, Pid, {error, Reason}} ->
		     send_multipart_error(Pid, Reason, Socket, Transport, State);
		 {'DOWN', _Ref, process, Pid, normal} ->
		     % process normal exit, just continue
		     {continue, State};
		 {'DOWN', _Ref, process, Pid, Reason} ->
		     send_multipart_error(Pid, Reason, Socket, Transport, State)
	     after 6000 ->
		     io:format("Timeout waiting for responses", []),
		     {done, State}
 	     end,
    case Result of
	{continue, State2} ->
	    stream_loop(Req, State2, Socket, Transport);
	{done, _State2} ->
	    ok
    end.
				  
send_multipart_response(Pid, Status, Headers, Body, Socket, Transport, State) ->
    RequestMessage = dict:fetch(Pid, State#state.procs),
    IOData = [
	      <<"--">>, State#state.boundary, ?CRLF,
	      header({<<"X-Status">>, status_to_iolist(Status)}),
	      header({<<"Content-Location">>, RequestMessage#request.url}),
	      lists:map(fun header/1, Headers),
	      ?CRLF, ?CRLF, 
	      Body],
    Transport:send(Socket, IOData),
    after_multipart_response(Pid, State).

status_to_iolist({Version, Code, Phrase}) ->
    [Version, " ", integer_to_list(Code), " ", Phrase].


send_multipart_error(Pid, Reason, Socket, Transport, State) ->
    send_multipart_response(
      Pid, 
      {"HTTP/1.1", 502, "Bad Gateway"}, 
      [{<<"Content-Type">>, <<"text/plain">>}], 
      io_lib:format("~p", [Reason]),
      Socket,
      Transport,
      State).

    
after_multipart_response(Pid, State) ->
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
