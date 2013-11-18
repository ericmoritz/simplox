#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa apps/simplox/ebin
-include_lib("simplox/include/simplox_pb.hrl").

main(URLs) ->
    Requests = [#request{url=Url} || Url <- URLs],
    Msg = simplox_pb:encode_multirequest(
		     #multirequest{requests=Requests}
		    ),
    io:format("~s", [Msg]).
			      
