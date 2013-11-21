%%% @author Moritz <emoritz@GCI-EMORITZ-M.local>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% Config for simplox
%%% @end
%%% Created : 20 Nov 2013 by Moritz <emoritz@GCI-EMORITZ-M.local>

-module(simplox_conf).

-export([init/0, acceptors/1, http_ranch_tcp/1]).

-type proplist() :: list().
-type config() :: proplist().

-spec init() -> config().
init() ->
    application:get_all_env(simplox).


-spec acceptors(config()) -> integer().
acceptors(Conf) ->
    get_value(acceptors, Conf).


-spec http_ranch_tcp(config()) -> proplist().
http_ranch_tcp(Conf) ->
    %% Allow for $PORT env override, this is for Heroku mainly
    case os:getenv("PORT") of
	false ->
	    get_value(http, Conf);
	PortStr ->
	    [{port, list_to_integer(PortStr)}]
    end.


get_value(Key, Conf) ->
    proplists:get_value(Key, Conf).

