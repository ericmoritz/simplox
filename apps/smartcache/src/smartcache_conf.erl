%%% @author Moritz <emoritz@GCI-EMORITZ-M.local>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Config for simplox
%%% @end
%%% Created : 20 Nov 2013 by Moritz <emoritz@GCI-EMORITZ-M.local>

-module(smartcache_conf).
-define(BACKEND, mcd).

-export([init/0, backend_mod/1, backend_child_spec/1]).

-type proplist() :: list().
-type config() :: proplist().

-spec init() -> config().
init() ->
    application:get_all_env(smartcache).


-spec backend_mod(config()) -> atom().
backend_mod(_Conf) ->
    case ?BACKEND of
	mcd ->
	    smartcache_mcd_backend;
	dets ->
	    smartcache_dets_backend;
	_ ->
	    smartcache_ets_backend
    end.

-spec backend_child_spec(config()) -> supervisor:child_spec().
backend_child_spec(_Conf) ->
    case ?BACKEND of
	dets ->
	    {smartcache_dets_backend_sup,
	     {smartcache_dets_backend_sup, start_link, ["/tmp/smartcache.dets"]},
	     permanent, 2000, supervisor, [smartcache_dets_backend_sup]};
	mcd ->
	    Nodes = [
		     ["127.0.0.1"]
		    ],
	    {smartcache_mcd_backend_sup,
	     {smartcache_mcd_backend_sup, start_link, [Nodes]},
	     permanent, 2000, supervisor, [smartcache_mcd_backend_sup]};
	_ ->
	    {smartcache_ets_backend_sup,
	     {smartcache_ets_backend_sup, start_link, []},
	     permanent, 2000, supervisor, [smartcache_ets_backend_sup]}
    end.

%get_value(Key, Conf) ->
%    proplists:get_value(Key, Conf).

