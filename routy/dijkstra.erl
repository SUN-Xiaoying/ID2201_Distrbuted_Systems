-module(dijkstra).
-export([table/2, route/2]).

%% returns the length of the shortest path to the node or 0 if the node is not found.
%% dijkstra:entry(berlin, [{berlin,2,paris}, {zurich,3,paris}]).
%% 2
entry(Node, Sorted) ->
    case lists:keysearch(Node,1,Sorted) of
	{value,{_, Metric, _}} ->
	    Metric;
	false -> 0
    end.

%% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list should of course be sorted.
%% dijkstra:replace(berlin, 4, lucerne, [{berlin,2,paris}, {zurich,3,paris}]).
%% [{zurich,3,paris},{berlin,4,lucerne}]
replace(Node, N, Gateway, Sorted) ->
    NewSorted = lists:keydelete(Node, 1, Sorted),
    NewSorted2 = [{Node, N, Gateway}|NewSorted],
    lists:keysort(2, NewSorted2).

%% update the list Sorted given the information that Node can be reached in N hops using Gateway. If no entry is found then no new entry is added. Only if we have a better (shorter) path should we replace the existing entry.
%% dijkstra:update(london, 1, stockholm, [{berlin, 2, paris}, {london, 3, paris}]).
%% [{london,1,stockholm},{berlin,2,paris}]
update(Node, N, Gateway, Sorted) ->
    %%find the metric of the existing node in list
    Metric = entry(Node, Sorted),
    %%compare metric with new value N
    if N < Metric ->
	    %%update list with new metric
	    replace(Node, N, Gateway, Sorted);
       true ->
	    %%nothing to update; return list as it is
	    Sorted
    end.		

%% construct a table given a sorted list of nodes, a map and a table constructed so far.
iterate([], _, Table) ->
    Table;
iterate([{_, inf, _}|_], _, Table) ->
    Table;
iterate([{Node, N, Gateway}|Nodes], Map, Table) ->
    %% get destination router for node
    Dest = map:reachable(Node, Map),

    %% add node and destination to table
    NewTable = [{Node,Gateway}|Table],

    %% update the interface map
    UpdatedNodes = lists:foldl(fun(Nd, Acc) ->
        update(Nd, N+1, Gateway, Acc) end, Nodes, Dest),

    %% iterate with the updated node map
    iterate(UpdatedNodes, Map, NewTable).

%% table(Gws, Map) will generate a routing table given a set of gateways and a map. The generated routing table is a list of entries {Dest, Gw} where Gw is the gatways leading to the shortest path.
%% dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]).
table(Gateways, Map) ->
    %io:format("Dijkstra Debug: Available Gateways: ~w~n",[Gateways]),
    Nodes = map:all_nodes(Map),
    %io:format("Dijkstra Debug: All known Nodes: ~w~n",[Nodes]),
    Rest = lists:filter(fun (X) -> not lists:member(X, Gateways) end, Nodes),
    %io:format("Dijkstra Debug: Remote Nodes: ~w~n",[Rest]),
    DirectConn = lists:map(fun (Nd) -> {Nd, 0, Nd} end, Gateways),
    %io:format("Dijkstra Debug: Directly connected Nodes: ~w~n",[DirectConn]),
    IndirectConn = lists:map(fun (Nd) -> {Nd, inf, unknown} end, Rest),
    %io:format("Dijkstra Debug: Indirectly connected Nodes: ~w~n",[IndirectConn]),
    Sorted = lists:append(DirectConn, IndirectConn),
    iterate(Sorted, Map, []).

%% search the routing table and return the gateway suitable to route messages to a node. If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
    case lists:keysearch(Node, 1, Table) of
	{value, {_, unknown}} ->
	    notfound;
	{value, {_, Gateway}} ->	   
	    {ok, Gateway};
	false ->
	    notfound
    end.
	    
