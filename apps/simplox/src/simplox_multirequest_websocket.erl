%%% @author Eric Moritz <eric@themoritzfamily.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% This provides a demostration of multirequest over websockets.  It needs
%%% to be much more fleshed out.  There's no RPC here, just raw messages flying around.
%%% @end
%%% Created : 23 Nov 2013 by Eric Moritz <eric@themoritzfamily.com>

-module(simplox_multirequest_websocket).
-compile([{parse_transform, lager_transform}]).
-behavior(cowboy_websocket_handler).
-include_lib("simplox/include/simplox_pb.hrl").

-export([init/3, websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-record(state, {clients=dict:new()}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _) ->
    process_flag(trap_exit, true), % this will go away when the multirequest_sup is supervised.
    {ok, Req, #state{}}.

                             
websocket_handle({binary, <<"application/protobuf+vnd.simplox.multirequest=", Packet/binary>>}, Req, State) -> 
    case simplox_pb_utils:decode_multirequest(Packet) of
	{ok, Msg} ->
	    State2 = start_multirequest(Msg, State),
	    {ok, Req, State2};
	{error, Reason} ->
	    Data = [
		    <<"text/plain=Error: ">>, 
		    io_lib:format("~p", Reason)],
	    {reply, {binary, Data}, Req, State}
    end;
websocket_handle(Msg, Req, State) -> 
    lager:info("Unknown msg: ~p", [Msg]),
    {ok, Req, State}.

websocket_info(
  {simplox_multirequest_server, Pid, {ok, {Status, Responses}}}, Req, State) -> 
                
    Prefix = <<"application/protobuf+vnd.simplox.response=">>,
    Frames = [{binary, [Prefix, simplox_pb:encode_response(R)]} ||
		 R <- Responses],
    State2 = maybe_remove_client(Status, Pid, State),
    {reply, Frames, Req, State2};
websocket_info({'EXIT', Pid, _Reason}, Req, State) -> 
    % delete any dead clients
    {ok, Req, maybe_remove_client(done, Pid, State)};
websocket_info(Msg, Req, State) -> 
    lager:info("Unknown MSG: ~p~n~p", [Msg, State]),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, _State) ->
    ok. % The link will kill all the clients.

%% Internal
start_multirequest(Msg, State) ->
    {ok, Pid} = simplox_multirequest_sup:start_link(),
    simplox_multirequest_server:fetch_async(Pid, Msg),
    State#state{clients=dict:store(Pid, Msg, State#state.clients)}.

maybe_remove_client(done, Pid, State) ->
    State#state{clients=dict:erase(Pid, State#state.clients)};
maybe_remove_client(_, _, State) ->
    State.

