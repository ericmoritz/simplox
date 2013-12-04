%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% This provides logging utilities for simplox
%%% @end
%%% Created :  4 Dec 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(simplox_logging).
-include_lib("simplox/include/simplox_pb.hrl").
-export([response_end/1]).

%%--------------------------------------------------------------------
%% @doc
%% Log the end of a response
%% @end
%%--------------------------------------------------------------------
response_end(ResponseBin) ->
    spawn(fun() ->
		  Response = simplox_pb:decode_response(ResponseBin),
		  JSONPacket = jsx:encode([
			      {<<"key">>, cast_str(Response#response.key)},
			      {<<"url">>, cast_str(Response#response.url)},
			      {<<"method">>, cast_str(Response#response.method)},
			      {<<"status">>, cast_str(Response#response.status)},
			      {<<"content_length">>, content_length(Response#response.body)}]),
		  lager:info("response_end:~s", [JSONPacket])
	  end).
      
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
