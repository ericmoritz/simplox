%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Utilities for decoding and validating the multirequest pb
%%% @end
%%% Created :  4 Dec 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(simplox_multirequest_pb).
-include_lib("simplox/include/simplox_pb.hrl").

-export([decode/1]).

-spec decode(binary()) -> {ok, #multirequest{}} | {error, any()}.
decode(MRBin) when is_binary(MRBin) ->
    try     
	validate_multirequest(simplox_pb:decode_multirequest(MRBin))
    catch Error ->
	    ErrorStr = io_lib:format("Error decoding body: ~p", [Error]),
	    {error, ErrorStr}
    end.

-spec validate_multirequest(#multirequest{})
			   -> {ok, #multirequest{}} | {error, any()}.
validate_multirequest(#multirequest{requests=[]}) ->
    {error, "MultiRequest.requests required"};
validate_multirequest(MR) ->
    {ok, MR}.

