% (c)2011, Thomas Galliker
% Please report updates/corrections to
% thomas_galliker@bluewin.ch
% 
% Thank you

-module(test2).
-export([start/3]).
-define(CHARSET, "abcdefghijklmnopqrstuvwxyz").
-define(NODESETUPTIME, 400). % avg time per node to setup the ring

start(Module, NodeCount, WriteCount)->
    A = apply(Module, start, [0]),
    register(nodestart, A),
    WaitForRing = (2+NodeCount) * ?NODESETUPTIME,
    TestProc = spawn(fun() -> run({nodestart, node()}, WriteCount, 0) end),
    setupRing(Module, A, NodeCount-1, NodeCount, [TestProc], WriteCount, WaitForRing),
    A.

% Setup a ring of nodes on this local erlang node
setupRing(_, _, 0, _, TestProcList, _, WaitForRing) ->
    io:format("Waiting ~w sec until the ring is set up...~n",
	      [WaitForRing/1000]),
    timer:sleep(WaitForRing),
    startTest(TestProcList),
    io:format("All tests started!~n", []);
setupRing(Module, RefNode, NodeCount, MaxNodeCount, TestProcList, WriteCount, WaitForRing) ->
    % to get significant results, we have to make sure
    % we use evenly distributed nodes.
    NodeId = NodeCount * (WriteCount div MaxNodeCount), 
    X = apply(Module, start, [NodeId, nodestart]),
    Nodename = list_to_atom(lists:flatten(string:concat("node_",get_random_string(4)))),
    register(Nodename, X),
    %timer:sleep(10),
    TestProc = spawn(fun() -> run({Nodename, node()}, WriteCount, 0) end),
    io:format("~w: Registering ~n", [Nodename]),
    NewTestProcList = [TestProc|TestProcList],
    io:format("-----NEWTestProcList----- ~n~w", [NewTestProcList]),
    setupRing(Module, RefNode, NodeCount-1, MaxNodeCount, NewTestProcList, WriteCount, WaitForRing).

%Signal test processes to start!
startTest([TestProc|TestProcList]) ->
    TestProc ! start,
    startTest(TestProcList);
startTest([]) ->
    true.

run(Node, Count, Offset) ->
    %io:format("Node ~w waiting for start signal...~n", [Node]),
    receive
	start ->
	    io:format("~w: Starting ~w WRITES ~n", [Count, Node]),
	    Start = now(),
	    doWrite(Node, Count, Offset)
    end,
    %io:format("Starting ~w READS from ~w~n", [Count, Node]),
    doRead(Node, Count, Offset),
    Finish = now(),
    T =  timer:now_diff(Finish, Start) / 1000,
    io:format("Time for ~w entries: ~wms~n", [Count, T]),
    run(Node, Count, Offset).

doWrite(_, 0, _) ->
    [];
doWrite(Node, Count, Offset) ->
    %io:format("SENDING {add, ~w, value, ref, self()} to ~w~n", [Count, Node]),
    Node ! {add, Count+Offset, value, ref, self()},
    doWrite(Node, Count-1, Offset).

doRead(_, 0, _) ->
	[];
doRead(Node, Count, Offset) ->
    Node ! {lookup, Count+Offset, Count, self()},
    receive
	{Count, Result, Key} -> %[]
	    io:format("~w: Reading ~w from node ~w~n", [Node, Result, Key])
    end,
    doRead(Node, Count-1, Offset).

get_random_string(Length) ->
    lists:foldl(fun(_, Acc) -> 
			[lists:nth(random:uniform(length(?CHARSET)), ?CHARSET)]
			    ++ Acc
		end, [], lists:seq(1, Length)).

