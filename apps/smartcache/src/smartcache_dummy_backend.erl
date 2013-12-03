%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% The memcached backend client
%%% @end
%%% Created : 27 Nov 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(smartcache_dummy_backend).
-export([get/1, set/3, delete/1]).

get(_Key) ->
    {error, not_found}.


set(_Key, Bin, _Timeout) when is_binary(Bin) ->
    ok.

delete(_Key) ->
    ok.
