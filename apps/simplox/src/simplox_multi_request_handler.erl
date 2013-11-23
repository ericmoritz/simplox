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
-record(state, {boundary, media_type, multirequest, multirequest_pid}).

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
	 rest_terminate/2
	]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    % TODO: supervise these
    {ok, Pid} = simplox_multirequest_sup:start_link(),
    {ok, Req, #state{multirequest_pid=Pid}}.

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
            Req3 = set_resp_content_type(
		     State2,
		     cowboy_req:set_resp_body_fun(
		       chunked, multipart_streamer(Req2, State2), Req2)),
	    {true, Req3, State2}
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
      [<<"multipart/mixed; boundary=">>, Boundary], Req).


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
    {ok, started} = simplox_multirequest_server:fetch(
		      State#state.multirequest_pid,
		      MultiRequest),
    State#state{multirequest=MultiRequest}.


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

multipart_streamer(Req, State) ->
    fun(F) ->
	    stream_loop(Req, State, F)
    end.

stream_loop(Req, State, F) ->
    {ok, {Status, Responses}} = simplox_multirequest_server:responses(
		     State#state.multirequest_pid, 10000),
    F(encode_responses(Responses, State)),
    case Status of
	continue ->
	    stream_loop(Req, State, F);
	done ->
	    ok
    end.


%% This will change based on the media type
encode_responses([], _) ->
    [];
encode_responses(Responses,
		#state{media_type={<<"application">>,
				   <<"protobuf+delimited+vnd.simplox.response">>,[]}}) ->
    simplox_pb:encode(Responses);
encode_responses(Responses, State) ->
    Mapper = fun(Response) ->
		     %% we can mismatch {Name, Value} and #header{} because header/1 can
		     %% handle both
		     Headers2 = [
				 {<<"X-Status">>, Response#response.status},
				 {<<"X-Request-Time">>, [integer_to_list(Response#response.request_time), " us"]},
				 {<<"Content-Location">>, Response#response.url}
				]
			 ++ [{<<"X-Simplox-Key">>, Response#response.key} ||
				Response#response.key =/= undefined]
			 ++ Response#response.headers,
		     [
		      ?CRLF, <<"--">>, State#state.boundary, ?CRLF,
		      lists:map(fun header/1, Headers2),
		      ?CRLF, 
		      Response#response.body]
	     end,
    lists:map(Mapper, Responses).


header({Name, Value}) ->
    [Name, ": ", Value, ?CRLF];
header(#header{key=Name, value=Value}) ->
    header({Name, Value}).
	      


make_boundary() ->
    % TODO: Do a random boundary function
    <<"gc0p4Jq0M2Yt08jU534c0p">>.

