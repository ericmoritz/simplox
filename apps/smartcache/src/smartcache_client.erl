%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This provides the public API for the smartcache client
%%% @end
%%% Created : 25 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

-module(smartcache_client).

-export([get/3, refresh/3, delete/1, identity_value_gen/1, fun_value_gen/1]).
-include("smartcache.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Get the value from the cache or generate it for the client
%% 
%% @end
%%--------------------------------------------------------------------
-spec get(key(), mfa(), seconds()) -> {ok, value()} | {error, any()}.
get(Key, ValueGenMFA, Timeout) ->
    smartcache_client_server:get(Key, ValueGenMFA, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% Force a key to be refreshed; called by the 
%% smartcache_prefetch_manager when the timeout is fired
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh(key(), mfa(), seconds()) -> ok.
refresh(Key, ValueGenMFA, Timeout) ->
    lager:info("Refreshing: ~p", [{Key, ValueGenMFA, Timeout}]),
    smartcache_client_server:refresh(Key, ValueGenMFA, Timeout).

delete(Key) ->
    smartcache_client_server:delete(Key).

identity_value_gen(Value) ->
    Value.

fun_value_gen(Fun) ->
    Fun().
