%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% The memcached backend client
%%% @end
%%% Created : 27 Nov 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(smartcache_mcd_backend).

-export([get/1, set/3]).
-include("smartcache_mcd_backend.hrl").

get(Key) ->
    case mcd:get(?CLUSTER_NAME, Key) of
	{ok, Bin} ->
	    {ok, binary_to_term(Bin)};
	{error, notfound} -> % translate mcd's notfound to smartcache's not_found
	    {error, not_found};
	E={error, _} ->
	    E
    end.

set(Key, Value, Timeout) ->
    case mcd:set(?CLUSTER_NAME, Key, term_to_binary(Value), 0, Timeout + 60) of
	{ok, _} ->
	    ok;
	E={error, _} ->
	    E
    end.
