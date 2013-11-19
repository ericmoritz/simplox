%%% @author Eric Moritz <emoritz@gannett.com>
%%% @copyright (C) 2013, Eric Moritz
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2013 by Eric Moritz <eric@eric-dev-vm>

-module(simplox).

-export([start/0, stats/0]).


start() ->
    apptools:ensure_started(?MODULE).

stats() ->
    [
     {multi_request_running, folsom_metrics:get_metric_value(multi_request_running)},    
     {multi_request_overhead, folsom_metrics:get_histogram_statistics(multi_request_overhead)}].
     
