%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

-module(simplox).

-export([start/0]).


start() ->
    apptools:ensure_started(?MODULE).

