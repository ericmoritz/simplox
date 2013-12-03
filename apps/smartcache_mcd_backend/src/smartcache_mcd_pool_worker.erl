%%% @author Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>
%%% @copyright (C) 2013, Moritz
%%% @doc
%%% This is a shim to make mcd work with poolboy
%%% @end
%%% Created :  3 Dec 2013 by Moritz <emoritz@usat-tecarlson.usatoday.us.ad.gannett.com>

-module(smartcache_mcd_pool_worker).
-behavior(poolboy_worker).

-export([start_link/1]).


start_link(Props) ->
    PeerAddresses = proplists:get_value(peers, Props),
    case supervisor:start_link(mcd_starter, PeerAddresses) of
      {ok, SupRef} ->
	    io:format("started mcd_cluster: ~p~n", [SupRef]),
	    Peers = [{Name, Pid, Weight} ||
			{{Name, Weight}, Pid, worker, _} <- supervisor:which_children(SupRef),
			is_pid(Pid)],
	    {ok, ClusterPid} = supervisor:start_child(
			SupRef, { mcd_cluster,
				{ mcd_cluster, start_link, [Peers] },
				  permanent, 60000, worker, [mcd_cluster, dht_ring] }),
	    {ok, ClusterPid};
	Error -> Error
    end.
