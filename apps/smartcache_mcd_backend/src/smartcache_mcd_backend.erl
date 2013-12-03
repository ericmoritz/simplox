%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% The memcached backend client
%%% @end
%%% Created : 27 Nov 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(smartcache_mcd_backend).

-export([get/1, set/3, delete/1]).
-include("smartcache_mcd_backend.hrl").


get(Key) ->
    poolboy:transaction(
      ?POOL_NAME, 
      fun(Pid) ->
	      case mcd:get(Pid, Key) of
		  {ok, Bin} ->
		      {ok, Bin};
		  {error, notfound} -> % translate mcd's notfound to smartcache's not_found
		      {error, not_found};
		  E={error, _} ->
		      E
	      end
      end).


set(Key, Bin, Timeout) when is_binary(Bin) ->
    poolboy:transaction(
      ?POOL_NAME,
      fun(Pid) ->
	      case mcd:set(Pid, Key, Bin, 0, Timeout + 60) of
		  {ok, _} ->
		      ok;
		  E={error, _} ->
		      E
	      end
      end).


delete(Key) ->
    poolboy:transaction(
      ?POOL_NAME,
      fun(Pid) ->
	      mcd:do(Pid, delete, Key)
      end).
