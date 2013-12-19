%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% A simple ETS based backend for smartcache; the 2GB limit is probably
%%% too small for a production system (*cough* Riak *cough*).
%%% @end
%%% Created : 26 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
%%%-------------------------------------------------------------------
-module(smartcache_ironcache_backend).

%% API
-export([get/1, set/3, delete/1]).
-type key() :: iolist().
-type value() :: iolist().
-type seconds() :: integer().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get the value into the ets table
%%
%% @end
%%--------------------------------------------------------------------
-spec get(key()) -> {ok, value()} | {error, not_found}.
get(Key) ->
    handle_get_response(make_request(get, {key_url(Key), request_headers()})).
	    
%%--------------------------------------------------------------------
%% @doc
%% Store a value into the ets bag
%%
%% @end
%%--------------------------------------------------------------------
-spec set(key(), value(), seconds()) -> ok | {error, any()}.
set(Key, Value, _) ->
    JSON = [
	    {<<"value">>, base64:encode(iolist_to_binary(Value))} %% fucking utf-8
	   ],
    Payload = jsx:encode(JSON),
    handle_put_response(
      make_request(
	put, 
	{key_url(Key), request_headers(), "application/json", Payload}
       )).

delete(Key) ->
    handle_delete_response(
      make_request(delete, {key_url(Key), request_headers()})
     ).


%%%===================================================================
%%% Internal
%%%===================================================================	
make_request(Method, Request) ->
    httpc:request(Method, Request, [], [{body_format, binary}]).
    

handle_get_response({ok, {{_, 200, _}, _, Body}}) ->
    Props = jsx:decode(base64:decode(Body)),
    {ok, proplists:get_value(<<"value">>, Props)};
handle_get_response({ok, {{_, 404, _}, _, _}}) ->
    {error, not_found}; 
handle_get_response(R) ->
    handle_response(R).

handle_put_response(R) ->
    handle_response(R).

handle_delete_response(R) ->
    handle_response(R).


handle_response({ok, {{_, 200, _}, _, _}}) ->
    ok;
handle_response({ok, {{_, _, _}, _, Body}}) ->
    {error, Body};
handle_response(E={error, _}) ->
    E.



trace(Val) ->
    io:format("~p~n", [Val]),
    Val.


key_url(Key) when is_binary(Key) ->
    key_url(binary_to_list(Key));
key_url(Key) ->
    SafeKey = http_uri:encode(base64:encode_to_string(Key)),
    iron_cache_url() ++ "/items/" ++ SafeKey.



request_headers() ->
    [
     {"Authorization", "OAuth " ++ iron_cache_token()}
    ].

iron_cache_url() ->
    "https://cache-aws-us-east-1.iron.io/1/projects/" ++ iron_cache_project_id() ++ "/caches/" ++ iron_cache_cache_name().

iron_cache_cache_name() ->
    "simplox".
    
iron_cache_project_id() ->
    os:getenv("IRON_CACHE_PROJECT_ID").

iron_cache_token() ->
    os:getenv("IRON_CACHE_TOKEN").
