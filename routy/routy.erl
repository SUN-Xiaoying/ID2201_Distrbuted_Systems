-module(routy).
-export([start/1, stop/1]).

start(Name) ->
    P = spawn(fun() -> init(Name) end),
    register(Name, P),

    M = spawn(fun() -> refreshRI_Timer(Name) end),
	io:format("Router: ~w~s~p~s Manage_process: ~p~n", [Name,"\t", P,"\t",M]).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(Name),
    %Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    self() ! {add, Name, self()}, %% add a loopback entry to our own network
    router(Name, 0, Hist, Intf, Table, Map).

%% Performs a periodical management process which broadcasts routing information after a configured timeout and rebuilds the routing table and network map.
refreshRI_Timer(RouterPID) ->
    Delay = (1000*rand:uniform(30))+30000,
    timer:sleep(Delay),
    %io:format("~w: Automatic updating~n", [RouterPID]),
    RouterPID ! broadcast,
    RouterPID ! update,
    refreshRI_Timer(RouterPID).

%% this function implements the recurring routing process itself.
%% - Name = The configured name of the router, sometimes also called as Node
%% - N = The message tracking number to avoid old messages from being sent endlessly trough the network
%% - Hist = The history track sheet which contains message tracking numbers of all known Nodes
%% - Intf = The interface list with all know neightbors (gateways)
%% - Table = The actual routing table with all directly reachable nodes and the valid gateways as well as the metrices
%% - Map - A network map with all network nodes and the gateway to send packages to
router(Name, N, Hist, Intf, Table, Map) ->
    receive
	{add, Node, Pid} ->
	    io:format("~w: Adding '~w'~n", [Name, Node]),
	    Ref = erlang:monitor(process,Pid),
	    Intf1 = intf:add(Node, Ref, Pid, Intf),
	    self() ! broadcast,
	    self() ! update,
	    router(Name, N, Hist, Intf1, Table, Map);
	{remove, Node} ->
	    io:format("~w: Removing ~n", [Name]),
	    {ok, Ref} = intf:ref(Node, Intf),
	    erlang:demonitor(Ref),
	    Intf1 = intf:remove(Node, Intf),
	    self() ! broadcast,
	    self() ! update,
	    router(Name, N, Hist, Intf1, Table, Map);
	{'DOWN', Ref, process, _, _} ->
	    {ok, Down} = intf:name(Ref, Intf),
	    io:format("~w: Exit received from ~w~n", [Name, Down]),
	    Intf1 = intf:remove(Down, Intf),
	    %self() ! broadcast,
	    %self() ! update,
	    router(Name, N, Hist, Intf1, Table, Map);
	{links, Name, _, _} -> 
	    %% ignore all link-state messages coming from myself; they're old!
	    router(Name, N, Hist, Intf, Table, Map);
	{links, Node, R, Links} ->
	    %io:format("~w: LSA received...~n", [Name]),
	    case hist:update(Node, R, Hist) of
		{new, Hist1} ->
		    Message = {links, Node, R, Links},
		    %io:format("~w: Forwarding routing information~n", [Name]),
		    intf:broadcast(Message, Intf),
		    %io:format("~w: Rebuilding Map...~n", [Name]),
		    Map1 = map:update(Node, Links, Map),
		    self() ! update,		    
		    router(Name, N, Hist1, Intf, Table, Map1);
		old ->
		    %io:format("~w: Link-state message from ~w was ignored since it was too old~n", [Name, Node]),
		    router(Name, N, Hist, Intf, Table, Map)
	    end;	

	%% Message Routing
	{route, Name, From, Message} ->
	    io:format("~w: Received message '~s' from ~w~n", [Name, Message, From]),
	    router(Name, N, Hist, Intf, Table, Map);
	{route, To, From, Message} ->
	    io:format("~w: Routing message ('~s') from ~w to ~w~n", [Name, Message, From, To]),
	    case dijkstra:route(To, Table) of
		{ok, Gateway} ->
		    case intf:lookup(Gateway, Intf) of
			{ok, Pid} ->
			    Pid ! {route, To, From, Message};
			notfound ->
			    ok
		    end;
		notfound ->
		    ok
	    end,
	    router(Name, N, Hist, Intf, Table, Map);

	%% Commands
	{status, From} ->
	    io:format("~w: Status requested by ~w~n", [Name, From]),
	    From ! {status, {Name, N, Hist, Intf, Table, Map}},
	    router(Name, N, Hist, Intf, Table, Map);
	info ->
	    io:format("~n"
				"Name: ~w~n"
				"------------------------------------~n"
				"Process ID: ~p~n"
				"------------------------------------~n"
				"Table:~n"
				"~w~n"
				"------------------------------------~n"
				"Interface:~n"
				"~w~n"
				"------------------------------------~n"
				"Map:~n"
				"~w~n"
				"------------------------------------~n"
				"Hist:~n"
				"~w~n"
				"------------------------------------~n~n",
		      [Name, self(), Table, Intf, Map, Hist]),
	    router(Name, N, Hist, Intf, Table, Map);
	update ->
	    %io:format("~w: Updating routing table~n", [Name]),
	    Table1 = dijkstra:table(intf:list(Intf), Map),
	    %io:format("~w: New table: ~w~n", [Name, Table1]),
	    router(Name, N, Hist, Intf, Table1, Map);
	broadcast ->
	    Message = {links, Name, N, intf:list(Intf)},
	    %io:format("~w: Broadcasting: ~w~n", [Name, Message]),
	    intf:broadcast(Message, Intf),
	    router(Name, N+1, Hist, Intf, Table, Map);
	{send, To, Message} ->
	    self() ! {route, To, Name, Message},
	    router(Name, N, Hist, Intf, Table, Map);
	stop ->
	    io:format("~w: Router stopped~n", [Name]),
	    ok;  
	_ ->
	    io:format("~w: Unknown command received.~n", [Name]),
	    router(Name, N, Hist, Intf, Table, Map)
    end.

