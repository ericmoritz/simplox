
-module(simplox_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Conf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Conf]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([Conf]) ->
    lager:start(),
    folsom_metrics:new_counter(multi_request_running),
    folsom_metrics:new_histogram(multi_request_overhead),


    Dispatch = cowboy_router:compile(
		 [
		  {'_', [
			 {"/simplox/v1/dummy/", simplox_dummy_handler, []},
			 {"/simplox/v1/multi-request/", simplox_multi_request_handler, []},
			 {"/simplox/v1/multirequest/", simplox_multi_request_handler, []},
			 {"/simplox/v1/multirequest.ws", simplox_multirequest_websocket, []},
			 {"/simplox/", cowboy_static, {priv_file, simplox, "www/index.html"}},
			 {"/simplox/static/proto/[...]", cowboy_static, 
			  {priv_dir, simplox, "www/static/proto", [{mimetypes, {<<"text">>, <<"plain">>, []}}]}
			 },

			 {"/simplox/[...]", cowboy_static, {priv_dir, simplox, "www"}}
			]}
		 ]),
    ClientSup = {http_client_sup, 
		 {http_client_sup, start_link, []}, 
		 permanent, 1000, supervisor, 
		 [http_client_sup]},
    RanchProps = simplox_conf:http_ranch_tcp(Conf),
    CowboySpec = ranch:child_spec(
		   simplox, simplox_conf:acceptors(Conf), 
		   ranch_tcp, RanchProps,
		   cowboy_protocol, [{env, [{dispatch, Dispatch}]}]),
    lager:info("Listening on ~p", [RanchProps]),
    {ok, { {one_for_one, 1000, 10}, [ClientSup, CowboySpec]} }.

