%%% @author Eric Moritz <emoritz@gannett.com>%
%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The cowboy handler that makes the multi-request
%%% @end
%%% Created : 14 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

%%% NOTE: It appears that I can't stream a POST response.
%%% I may need to do some funky stuff to stream out the result in the accept callback.

-module(simplox_dummy_handler).

-record(state, {boundary, multirequest, procs=dict:new()}).

-define(CRLF, <<"\r\n">>).
-include_lib("simplox/include/simplox_pb.hrl").

-export([
	 init/3, 
	 rest_init/2,
	 allowed_methods/2,
	 content_types_provided/2, 
	 content_types_accepted/2,
	 resource_exists/2,
	 multirequest_parser/2,
	 echo_parser/2,
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
     {<<"text/plain">>, echo_parser},
     {<<"application/protobuf">>, multirequest_parser}, %% for heroku
     {<<"application/protobuf+vnd.simplox.multirequest">>, multirequest_parser}
    ], Req, State}.


echo_parser(Req, State=#state{boundary=Boundary}) ->
    Req2 = case cowboy_req:qs_val(<<"add-t-e">>, Req) of
	       {undefined, R} ->
		   R;
	       {_, R} ->
		   cowboy_req:set_resp_header(
		     <<"transfer-encoding">>, <<"identity">>, R)
	   end,
    Req3 = cowboy_req:set_resp_body(dummy_data(),
				      cowboy_req:set_resp_header(
				      <<"content-type">>,
				      <<"multipart/mixed; boundary=", Boundary/binary>>,
				      Req2)),
    {true, Req3, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

multirequest_parser(Req, State=#state{boundary=Boundary}) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case decode_multirequest(Body) of 
	{error, _Reason} ->
	    {false, Req2, State};
	{ok, _MultiRequest} -> 
            {{stream, StreamFun}, Req3, State2} = streaming_multipart_response(Req2, State),
            Req4 = cowboy_req:set_resp_header(
		     <<"content-length">>, <<"399">>,
		     cowboy_req:set_resp_header(
	       <<"content-type">>,
	       <<"multipart/mixed; boundary=", Boundary/binary>>,
	       cowboy_req:set_resp_body_fun(chunked, StreamFun, Req3))),
               {true, Req4, State2}
    end.

decode_multirequest(Body) ->
    try     
	Msg = simplox_pb:decode_multirequest(Body),
	{ok, Msg}
    catch Error ->
	    {error, Error}
    end.

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


rest_terminate(_Req, _State) ->
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

stream_loop(_Req, _State, F) ->
    F(dummy_data()),
    ok.


make_boundary() ->
    % TODO: Do a random boundary function
    <<"gc0p4Jq0M2Yt08jU534c0p">>.

dummy_data() ->
    <<"--gc0p4Jq0M2Yt08jU534c0p
X-Status: 200
Content-Location: http://httpbin.org/get
Connection: keep-alive
Content-Length: 152
Server: gunicorn/0.17.4
Date: Thu, 21 Nov 2013 17:00:33 GMT
Content-Type: application/json
Access-Control-Allow-Origin: *


{
  \"args\": {},
  \"headers\": {
    \"Host\": \"httpbin.org\",
    \"Connection\": \"close\"
  },
  \"url\": \"http://httpbin.org/get\",
  \"origin\": \"159.54.131.7\"
}">>.
