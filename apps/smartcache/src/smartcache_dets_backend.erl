%%%-------------------------------------------------------------------
%%% @author Eric Moritz <eric@eric-dev-vm>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% A simple DETS based backend for smartcache; the 2GB limit is probably
%%% to small for a production system (*cough* Riak *cough*).
%%% @end
%%% Created : 26 Nov 2013 by Eric Moritz <eric@eric-dev-vm>
%%%-------------------------------------------------------------------
-module(smartcache_dets_backend).

%% API
-export([open/1, get/2, set/4]).
-include("smartcache.hrl").
-type dets_name() :: any().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
open(Name) ->
    % I assume that opening, doing one operation and then closing the file
    % is probably not good but for development, I'm going to ignore that fact.
    {ok, Name} = dets:open_file(
		   Name,
		   [
		    {auto_save, 500},
		    {type, bag}
		   ]).

%%--------------------------------------------------------------------
%% @doc
%% Get the value into the dets table
%%
%% @end
%%--------------------------------------------------------------------
-spec get(dets_name(), iodata()) -> {ok, iodata()} | {error, not_found}.
get(Name, Key) ->
    
    case dets:lookup(Name, key(Key)) of
	[{Key, Value}] ->
	    {ok, Value};
	_ ->
	    {error, not_found}
    end.
	    
%%--------------------------------------------------------------------
%% @doc
%% Store a value into the dets bag
%%
%% @end
%%--------------------------------------------------------------------
-spec set(dets_name(), iodata(), iodata(), seconds()) -> ok | {error, any()}.
set(Name, Key, Value, _Timeout) ->
    dets:insert(Name, {key(Key), Value}).


%% Internal
key(Key) -> iolist_to_binary(Key).
     
