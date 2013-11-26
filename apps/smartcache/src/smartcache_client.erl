%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This provides the public API for the smartcache client
%%% @end
%%% Created : 25 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

-module(smartcache_client).

-export([get/3, refresh/3]).
-include("smartcache.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Get the value from the cache or generate it for the client
%% 
%% @end
%%--------------------------------------------------------------------
-spec get(key(), mfa(), seconds()) -> {ok, value()} | {error, any()}.
get(Key, ValueGenMFA, Timeout) ->
    % spawn a new cache_client using the supervisor
    {ok, Pid} = smartcache_client_sup:start_child(),
    smartcache_client_server:get(Pid, Key, ValueGenMFA, Timeout).

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
    {ok, Pid} = smartcache_client_sup:start_child(),
    smartcache_client_server:refresh(Pid, Key, ValueGenMFA, Timeout).
