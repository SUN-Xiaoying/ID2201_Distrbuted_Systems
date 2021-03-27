-module(node1).
-export([start/1,start/2]).
-define(STABILIZE, 1000).
-define(TIMEOUT, 1000).

start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
	%Returns a unique reference among connected nodes.
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
	{Qref, Skey} ->
	    % Take the connected node as successor
	    {ok, {Skey, Peer}}
    after ?TIMEOUT ->
	    io:format("~w: Request timed out.~n",[Id]),
	    % Take ourselfs as successor
	    {ok, {Id, self()}}
    end.

node(Id, Predecessor, Successor) ->
    receive
	{key, Qref, Peer} ->
	    %io:format("~w: Key request from ~w~n", [Id, Peer]),
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor);
	{notify, New} ->
	    %io:format("~w: NOTIFY by ~w~n", [Id, New]),
	    Pred = notify(New, Id, Predecessor),
	    node(Id, Pred, Successor);
	{request, Peer} ->
	    request(Peer, Predecessor),
	    node(Id, Predecessor, Successor);
	{status, Pred} ->
	    Succ = stabilize(Pred, Id, Successor),
	    node(Id, Predecessor, Succ);
	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor);
	info ->
	    io:format("------------------------~n"
			  "Node ID:\t ~w~n"
		      "Predecessor ID:\t ~w~n"
		      "Successor ID:\t ~w~n",[Id, Predecessor, Successor]),
	    node(Id, Predecessor, Successor);
	ring ->
	    % Start ring broadcast
	    {_, Spid} = Successor,
	    Spid ! {ring, {Id, []}},
	    node(Id, Predecessor, Successor);
	{ring, {Id, Info}} ->
	    % Show ring broadcast result
	    io:format("~w: Ring finished:~n"
		      "~w~n", [Id, Info]),
	    node(Id, Predecessor, Successor);
	{ring, {CallerId, Info}} ->
	    % Pass ring broadcast further
	    io:format("~w: Ring passed by~n"
		      "~w~n", [Id, Info]),
	    {_, Spid} = Successor,
	    Spid ! {ring, {CallerId, [Id|Info]}},
	    node(Id, Predecessor, Successor);

	probe ->
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor);
	{probe, Id, Nodes, T} ->
	    % If the Probe is equal to the Id of the node
	    % we know that we sent it and can report
	    % the time it took to pass it around the ring.
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor);
	{probe, Ref, Nodes, T} ->
	    forward_probe(Id, Ref, T, Nodes, Successor),
	    node(Id, Predecessor, Successor);

	UnknownMessage ->
	    io:format("~w: Error: Unknown message: ~w~n", [Id, UnknownMessage]),
	    node(Id, Predecessor, Successor)
	end.

schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
	nil ->
	    % Ponit to me!
	    Spid ! {notify, {Id, self()}},
	    Successor;
	{Id, _} ->
	    % We are pointing to ourselfs.
	    Successor;
	{Skey, _} ->
	    % Our successor uses a predecessor which points to itself.
		% Let's notify him, that he can point to us.
		io:format("~w: ~w, point to me!~n",[Id,Skey]),
	    Spid ! {notify, {Id, self()}},
	    Successor;
	{Xkey, Xpid} ->
	    case key:between(Xkey, Id, Skey) of
		true ->
		    % If the key of the predecessor of our successor (Xkey)
		    % is between us(Id) and our successor(Skey) we should adopt
		    % this node as our successor and run stabilization again.
		    stabilize(Pred, Id, {Xkey, Xpid});	    
		false ->
		    % If we should be in between the nodes
		    % we inform our successor of our existence.
		    Spid ! {notify, {Id, self()}},			    
		    Successor
		end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil};
	{Pkey, Ppid} ->
	    Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
	nil ->
	    % If our own predecessor is set to nil
	    % we take the received predecessor
	    {Nkey, Npid};
	{Pkey, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    % Nkey is the better (closer) predecessor
		    % than Pkey! Let's take the new predecessor
		    {Nkey, Npid};
		false ->
		    Predecessor
	    end
    end.

create_probe(Id, {_, Spid}) ->
	Spid ! {probe, Id, [Id], now()}.

remove_probe(T, Nodes) ->
	{_, BeforeS, BeforeNs} = T,
	{_, NowS, NowNs} = now(),
	Time = (NowS-BeforeS) * 1000000 + (NowNs-BeforeNs),
	io:format("Probe successfully finished:~n"
		  "Round trip time: ~w ns~n"
		  "Ring nodes: ~w~n",[Time, Nodes]).

forward_probe(Id, Ref, T, Nodes, {_, Spid}) ->
	Spid ! {probe, Ref, [Id|Nodes], T}.
