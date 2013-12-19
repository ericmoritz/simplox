%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%% The cowboy handler that makes the multi-request
%%% @end
%%% Created : 14 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

%%% NOTE: It appears that I can't stream a POST response.
%%% I may need to do some funky stuff to stream out the result in the accept callback.

-module(simplox_multi_request_handler).
-compile([{parse_transform, lager_transform}]).
-record(state, {boundary, media_type, multirequest, multirequest_pid}).
-type request() :: cowboy_req:request().

-include_lib("simplox/include/simplox_pb.hrl").

-export([
	 init/3, 
	 rest_init/2,
	 allowed_methods/2,
	 content_types_provided/2, 
	 content_types_accepted/2,
	 multirequest_handler/2,
	 multirequest_base64_handler/2,	 
	 html_resource/2,
	 rest_terminate/2
	]).


%%%===================================================================
%%% cowboy rest callbacks
%%%===================================================================
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    % TODO: supervise these
    {ok, Pid} = simplox_multirequest_sup:start_link(),
    {ok, Req, #state{multirequest_pid=Pid}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
     {<<"application/protobuf+vnd.simplox.multirequest">>, multirequest_handler}, % backwards compatibility
     {<<"application/vnd.simplox.multirequest+protobuf">>, multirequest_handler}, % correct mime-type
     {<<"application/vnd.simplox.multirequest.base64+protobuf">>, multirequest_base64_handler}
    ], Req, State}.

content_types_provided(Req, State) ->
    case cowboy_req:method(Req) of
	{<<"GET">>, Req1} ->
	    {[{<<"text/html">>, html_resource}], Req1, State};
	{<<"POST">>, Req1} ->
	    % TODO: move this to the multipart only code
	    Boundary = simplox_multipart:make_boundary(),
	    State1 = State#state{boundary=Boundary},

	    % NOTE: the '_' is here because the resource callback isn't used for
	    % a POST
	    {[
	      {{<<"multipart">>, <<"mixed">>, '*'}, '_'},
	      {<<"application/protobuf+delimited+vnd.simplox.response">>, '_'}
	     ], 
	     Req1, State1}
    end.


rest_terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% Resource callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% The multirequest POST body handler. 
%% It parses the POST body and sets the response streamer
%% @end
%%--------------------------------------------------------------------
-spec multirequest_handler(request(), #state{}) -> {true | false, request(), #state{}}.
multirequest_handler(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    multirequest_handler(Body, Req1, State).

multirequest_base64_handler(Req, State) ->
    {ok, Body64, Req1} = cowboy_req:body(Req),
    lager:debug("Body: ~s", [Body64]),
    Body = base64:decode(Body64),
    multirequest_handler(Body, Req1, State).
    
multirequest_handler(Body, Req, State) ->
    {MediaType, Req1} = cowboy_req:meta(media_type, Req),
    State1 = State#state{media_type=MediaType},
    handle_multirequest(Req1, State1, simplox_multirequest_pb:decode(Body)).
    

%%--------------------------------------------------------------------
%% @doc
%% The generic response for a GET request
%% @end
%%--------------------------------------------------------------------
-spec html_resource(request(), #state{}) -> {iodata(), request(), #state{}}.
html_resource(Req, State) ->
    Body = <<"<html>
<body>
<p>No docs yet.</p>
</body></html>">>,
    {Body, Req, State}.


%%%===================================================================
%%% Internal
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Handle the decoded multirequest.
%% This response with an error response if the validation failed
%% or sets the response streamer
%% @end
%%--------------------------------------------------------------------
-spec handle_multirequest(request(), #state{}, {ok, #multirequest{}} | {error, any()}) 
			 -> {true | false, request(), #state{}}.
handle_multirequest(Req, State, E={error, _}) ->
    {false, handle_error(Req, E), State};
handle_multirequest(Req, State, {ok, MultiRequest}) ->
    State2 = spawn_request_procs(MultiRequest, State),
    Req2 = set_resp_content_type(
	     State2,
	     cowboy_req:set_resp_body_fun(
	       chunked, 
	       fun(F) -> multiresponse_wait_loop(Req, State, F) end,
	       Req)),
    {true, Req2, State2}.

%%--------------------------------------------------------------------
%% @doc
%% The generic {error, any()} response
%% @end
%%--------------------------------------------------------------------
-spec handle_error(request(), {error, any()}) -> request().
handle_error(Req, E={error, _}) ->
    cowboy_req:set_resp_header(
      <<"content-type">>,
      <<"text/plain">>,
      cowboy_req:set_resp_body(io_lib:format("~p~n", [E]), Req)).
    
%%--------------------------------------------------------------------
%% @doc
%% Updates the content-type header according to the accepted media type
%% @end
%%--------------------------------------------------------------------
-spec set_resp_content_type(#state{}, request()) -> request().
set_resp_content_type(#state{media_type={X = <<"application">>,
					 Y = <<"protobuf+delimited+vnd.simplox.response">>,[]}}, Req) ->
    cowboy_req:set_resp_header(
      <<"content-type">>,
      iolist_to_binary([X, "/", Y]),
      Req);
set_resp_content_type(#state{boundary=Boundary}, Req) ->
    cowboy_req:set_resp_header(
      <<"content-type">>,
      [<<"multipart/mixed; boundary=">>, Boundary], Req).


%%--------------------------------------------------------------------
%% @doc
%% Spawns the multirequest server FSM
%% @end
%%--------------------------------------------------------------------
-spec spawn_request_procs(#multirequest{}, #state{}) -> #state{}.
spawn_request_procs(MultiRequest, State) ->
    {ok, started} = simplox_multirequest_server:fetch(
		      State#state.multirequest_pid,
		      MultiRequest),
    State#state{multirequest=MultiRequest}.


%%--------------------------------------------------------------------
%% @doc
%% Waits for responses from the multirequest server
%% @end
%%--------------------------------------------------------------------
-type chunked_fun() :: fun((iodata()) -> ok | {error, atom()}).
-spec multiresponse_wait_loop(request(), #state{}, chunked_fun()) -> ok.
multiresponse_wait_loop(Req, State, F) ->
    % TODO: handle errors
    {ok, {Status, Responses}} = simplox_multirequest_server:responses(
		     State#state.multirequest_pid, 10000),
    F(encode_responses(Responses, State)),
    case Status of
	continue ->
	    multiresponse_wait_loop(Req, State, F);
	done ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Encodes the responses based on the accepted media type
%% @end
%%--------------------------------------------------------------------
-spec encode_responses([binary()], #state{}) -> iodata().
encode_responses([], _) ->
    [];
encode_responses(ResponseBins,
		#state{media_type={<<"application">>,
				   <<"protobuf+delimited+vnd.simplox.response">>,[]}}) ->
    map_responses(fun delimited_protobin/1, ResponseBins);
encode_responses(ResponseBins, 
		 #state{boundary=Boundary,
			media_type={<<"multipart">>,
				    <<"mixed">>,[]}}) ->
    map_responses(fun(ResBin) -> simplox_multipart:response(Boundary, ResBin) end, ResponseBins).


%%--------------------------------------------------------------------
%% @doc
%% Builds an protobuff delimited iodata list
%% This is simply a protobuf varint size prefix; pretty simple
%%
%% This may be better of in another module if we end up with more
%% protocols that use it.
%% @end
%%--------------------------------------------------------------------
-spec delimited_protobin(binary()) -> iodata().
delimited_protobin(ProtoBin) when is_binary(ProtoBin) ->
    [protobuffs:encode_varint(size(ProtoBin)),
     ProtoBin].
    

%%--------------------------------------------------------------------
%% @doc
%% A generic function for mapping each response bin to a streamed
%% response.  
%% @end
%%--------------------------------------------------------------------
-type response_mapper() :: fun((binary()) -> iodata()).
-spec map_responses(response_mapper(), [binary()]) -> iodata().
map_responses(Mapper, ResponseBins) ->
    [Mapper(R) || R <- ResponseBins].

