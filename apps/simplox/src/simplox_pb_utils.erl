%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% Some tools for validating the protobufs
%%% @end
%%% Created : 23 Nov 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(simplox_pb_utils).
-include_lib("simplox/include/simplox_pb.hrl").
-export([decode_multirequest/1]).

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


