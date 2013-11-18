%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% An async http client for the multirequests
%%% @end
%%% Created : 15 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

-module(http_client).
-export([start/2]).

-include_lib("simplox/include/simplox_pb.hrl").

start(TargetPid, RequestMessage) ->
    spawn(fun() ->
		  Result = httpc:request(
			     method(RequestMessage), request(RequestMessage),
			     [], [{body_format, binary}]),
		  TargetPid ! {http, self(), Result}
	  end).

request(RequestMessage=#request{content_type=undefined}) ->
    {url(RequestMessage), headers(RequestMessage)};
request(RequestMessage=#request{body=undefined}) ->
    {url(RequestMessage), headers(RequestMessage)};
request(RequestMessage) ->
    {url(RequestMessage), headers(RequestMessage), content_type(RequestMessage), body(RequestMessage)}.

headers(RequestMessage) ->
    [{Header#header.key, Header#header.value} || Header <- RequestMessage#request.headers].

content_type(RequestMessage) ->
    RequestMessage#request.content_type.

url(RequestMessage) ->
    binary_to_list(RequestMessage#request.url).

body(RequestMessage) ->
    RequestMessage#request.body.

method(#request{method=Method}) ->
    list_to_atom(Method).


