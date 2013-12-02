%%% @author Moritz <emoritz@GCI-EMORITZ-M.local>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Config for simplox
%%% @end
%%% Created : 20 Nov 2013 by Moritz <emoritz@GCI-EMORITZ-M.local>

-module(smartcache_conf).

-export([init/0, backend_mod/1, backend_child_spec/1]).

-type proplist() :: list().
-type config() :: proplist().

-spec init() -> config().
init() ->
    application:get_all_env(smartcache).


-spec backend_mod(config()) -> atom().
backend_mod(_Conf) ->
    smartcache_ets_backend.

-spec backend_child_spec(config()) -> supervisor:child_spec().
backend_child_spec(_Conf) ->
    {smartcache_ets_backend_sup,
     {smartcache_ets_backend_sup, start_link, []},
     permanent, 2000, supervisor, [smartcache_ets_backend_sup]}.

%get_value(Key, Conf) ->
%    proplists:get_value(Key, Conf).

