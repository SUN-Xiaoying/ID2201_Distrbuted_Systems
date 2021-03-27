-module(map).
-export([new/0, new/1, update/3, reachable/2, all_nodes/1]).


%%returns an empty map (a empty list)
new() ->
    [].
new(Name) ->
    update(Name, [Name], []). 

%%updates the Map to reflect that Node has directional links to all nodes in the list Links. The old entry is removed.
update(Node, Links, Map) ->
    case lists:keysearch(Node, 1, Map) of
	{value, {_, _}} ->
	    NewMap = lists:keydelete(Node, 1, Map),
	    [{Node, Links}|NewMap];
	false ->
	    [{Node, Links}|Map]
    end.

%%resturns the list of nodes directly reachable from Node.
reachable(Node, Map) ->
    case lists:keysearch(Node,1,Map) of
	{_,{_,List}} ->
	    List;
	false -> []
    end.

%%returns a list of all nodes in the map, also the ones without outgoing links. So if berlin is linked to london but london does not have any outgoing links (and thus no entry in the list), berlin should still be in the returned list.
all_nodes(Map) ->
    NodeList = listAllNodes(Map),
    lists:usort(NodeList).

%%add all tupel elements to the list
listAllNodes([]) ->
    [];
listAllNodes([{Node,Links}|Rest])->
    CurrElement=[Node|Links],
    lists:append(CurrElement,listAllNodes(Rest)).



