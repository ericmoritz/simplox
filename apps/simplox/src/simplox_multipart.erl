%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Module to convert a #response{} into a multipart iodata(). 
%%% @end
%%% Created :  4 Dec 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(simplox_multipart).
-include_lib("simplox/include/simplox_pb.hrl").
-export([response/2, make_boundary/0]).
-define(CRLF, <<"\r\n">>).

make_boundary() ->
    % TODO: Do a random boundary function
    <<"gc0p4Jq0M2Yt08jU534c0p">>.

response(Boundary, ResponseBin) when is_binary(ResponseBin) ->
    response(Boundary, simplox_pb:decode_response(ResponseBin));
response(Boundary, Response=#response{}) ->
    [
     boundary(Boundary),
     headers(Response),
     ?CRLF, 
     Response#response.body
    ].


-spec boundary(iodata()) -> iodata().
boundary(Boundary) ->
    [?CRLF, <<"--">>, Boundary, ?CRLF].

-spec headers(#response{}) -> iodata().
headers(Response) ->
    %% we can mismatch {Name, Value} and #header{} because header/1 can
    %% handle both
    lists:map(fun header/1, 
	      [
	       {<<"X-Status">>, Response#response.status},
	       {<<"X-Request-Time">>, [integer_to_list(Response#response.request_time), " us"]},
	       {<<"Content-Location">>, Response#response.url}
	      ]
	      ++ [{<<"X-Simplox-Key">>, Response#response.key} ||
		     Response#response.key =/= undefined]
	      ++ Response#response.headers).


-spec header({iodata(), iodata()} | #header{}) -> iodata().
header({Name, Value}) ->
    [Name, ": ", Value, ?CRLF];
header(#header{key=Name, value=Value}) ->
    header({Name, Value}).
	      
