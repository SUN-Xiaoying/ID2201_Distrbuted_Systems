-module(node2).
-export([start/1,start/2]).
-compile({no_auto_import,[demonitor/1]}).
-define(STABILIZE, 200).
-define(TIMEOUT, 1000).

start(Id) ->
    start(Id, nil).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    Store = storage:create(),
    Replica = storage:create(),
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, nil, Store, Replica).

connect(Id, nil) ->
    {ok, {Id, monitor(self()), self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
	{Qref, Skey} ->
	    % Take the connected node as successor
	    {ok, {Skey, monitor(Peer), Peer}}
    after ?TIMEOUT ->
	    % Watch for temporarily unavailable 
	    {ok, {Id, monitor(self()), self()}}
    end.

node(Id, Predecessor, Successor, Next, Store, Replica) ->
    receive
	{key, Qref, Peer} ->
	    %io:format("~w: Key request from ~w~n", [Id, Peer]),
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{notify, New} ->
	    %io:format("~w: Notification from ~w~n", [Id, New]),
	    {NewPredecessor, NewStore, NewReplica} = notify(New, Id, Predecessor, Store, Replica),

	    %io:format("NewPredecessor=~w: ~n", [NewPredecessor]),
	    %io:format("NewStore=~w: ~n", [NewStore]),
	    node(Id, NewPredecessor, Successor, Next, NewStore, NewReplica);
	{request, Peer} ->
	    %io:format("~w: REQUEST~n", [Id]),
	    request(Peer, Predecessor, Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{status, Pred, Nx} ->    
	    {NewSuccessor, NewNx} = stabilize(Pred, Nx, Id, Successor),
	    {OldSkey,_,_} = Successor,        % THESE LINES ARE OBSOLETE!
	    {NewSkey,_,NewSpid} = NewSuccessor,
	    node(Id, Predecessor, NewSuccessor, NewNx, Store, Replica);
	stabilize ->
	    %io:format("~w: STABILIZE~n", [Id]),
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{'DOWN', Ref, process, _, _} ->
	    {Pred, Succ, Nxt, NewStore, NewReplica} = down(Id, Ref, Predecessor, Successor, Next, Store, Replica),
	     io:format("~n-----~w: Store after crash-----~n~w~n"
		       "-----Replica after crash-----~n~w~n", [Id, NewStore, NewReplica]),
	    node(Id, Pred, Succ, Nxt, NewStore, NewReplica);
      	{replicate, Key, Value} ->   
	    NewReplica = storage:add(Key, Value, Replica),
	    io:format("~w: Key '~w' with value '~w' stored in replica.~n", [Id, Key, Value]),
	    node(Id, Predecessor, Successor, Next, Store, NewReplica);
	{handover, Elements} ->
	    MergedStore = storage:merge(Store, Elements),
	    {Skey,_,Spid} = Successor,
	    newReplica(Id, MergedStore, Spid),
	    node(Id, Predecessor, Successor, Next, MergedStore, Replica);
     	{handoverReplica, Elements} ->    
	    MergedReplica = storage:sort(storage:merge(Replica, Elements)),
	    %io:format("~w: Merged replica: ~w~n", [Id, MergedReplica]),
	    node(Id, Predecessor, Successor, Next, Store, MergedReplica);
	{newReplica, NewReplica} ->    
	    %io:format("~w: Got NEW replica: ~w (OLD replica:~w)~n", [Id, NewReplica, Replica]),
	    node(Id, Predecessor, Successor, Next, Store, NewReplica);
	{mergeReplica, NewReplica} ->  
	    MergedReplica = storage:sort(storage:merge(Replica, NewReplica)),
	    %io:format("~w: Replica has been merged ~w (~nOLD replica:~w)~n", [Id, NewReplica, Replica]),
	    node(Id, Predecessor, Successor, Next, Store, MergedReplica);

	info ->
	    {Pkey, _, Ppid} = Predecessor,
	    {Skey, _, Spid} = Successor,
	    io:format("------------------------------~n"
			  "Predecessor ID: ~w \t|  Pid: ~w~n"
		      "Local Node ID:  ~w \t|  Pid: ~w~n"
		      "Successor ID:   ~w \t|  Pid: ~w~n",
		      [Pkey, Ppid, Id, self(), Skey, Spid]),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	probe ->
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{probe, Id, Nodes, T} ->
	    % If the Probe is equal to the Id of the node
	    % we know that we sent it and can report
	    % the time it took to pass it around the ring.
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	{probe, Ref, Nodes, T} ->
	    forward_probe(Id, Ref, T, Nodes, Successor),
	    node(Id, Predecessor, Successor, Next, Store, Replica);

	% Key store related commands
	{add, Key, Value, Qref, Client} ->
	    AddedStore = add(Key, Value, Qref, Client,
			Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Next, AddedStore, Replica);
	{lookup, Key, Qref, Client} ->
	    lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Next, Store, Replica);

	print ->
	    io:format("------~w: Store------~n"
		      "~p~n", [Id, Store]),
	    node(Id, Predecessor, Successor, Next, Store, Replica);
	printreplica ->
	    io:format("-----~w: Replica-----~n"
		      "~p~n", [Id, Replica]),
	    node(Id, Predecessor, Successor, Next, Store, Replica);

	stop ->
	    io:format("~w: STOP!~n", [Id]);
	stopall ->
	    io:format("~w: STOPALL!~n", [Id]),
	    forward_stop(Successor);

	UnknownMessage ->
	    io:format("~w: Error: Unknown message: ~w~n", [Id, UnknownMessage]),
	    node(Id, Predecessor, Successor, Next, Store, Replica)
	end.

schedule_stabilize() ->
    timer:send_interval(?STABILIZE, self(), stabilize).

stabilize({_, _, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Next, Id, Successor) ->
    {Skey, Smon, Spid} = Successor,
    case Pred of
	nil ->
	    % We do not know about a predecessor?
	    Spid ! {notify, {Id, self()}},
	    {Successor, Next};
	{Id, _} ->
	    % We are pointing to ourselfs
	    {Successor, Next};
	{Skey, _} ->
	    % Our successor uses a predecessor which points to itself.
		% Let's notify him, that he can point to us.
		io:format("~w: ~w, Point to me!~n",[Id, Skey]),
	    Spid ! {notify, {Id, self()}},
	    {Successor, Next};
	{Xkey, Xpid} ->
	    case key:between(Xkey, Id, Skey) of
		true ->
		    % If the key of the predecessor of our successor (Xkey)
		    % is between us and our successor we should adopt
		    % this node as our successor and run stabilization again.
		    demonitor(Smon),
		    stabilize(Pred, Next, Id, {Xkey, monitor(Xpid), Xpid});	    
		false ->
		    % If we should be in between the nodes
		    % we inform our successor of our existence.
		    Spid ! {notify, {Id, self()}},
		    {Successor, Next}
		end
    end.

request(Peer, Predecessor, {Skey, _, Spid}) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil, {Skey, Spid}};
	{Pkey, _, Ppid} ->
	    Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store, Replica) ->
    case Predecessor of
	nil ->
	    % If our own predecessor is set to nil
	    % we take the received predecessor
	    io:format("~w: Predecessor found: ~w ~n", [Id,Nkey]),
	    Keep = handover(Id, Store, Nkey, Npid),
	    KeepReplica = handoverReplica(Id, Replica, Nkey, Npid),
	    {{Nkey, monitor(Npid), Npid}, Keep, KeepReplica};
	{Pkey, Pmon, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    % Nkey is the better (closer) predecessor
		    % than Pkey! Let's take this new predecessor
		    io:format("~w: New predecessor: ~w~n", [Id,Nkey]),
		    Keep = handover(Id, Store, Nkey, Npid),
		    KeepReplica = handoverReplica(Id, Replica, Nkey, Npid),
		    demonitor(Pmon),	    
		    {{Nkey, monitor(Npid), Npid}, Keep, KeepReplica};
		false ->
		    {Predecessor, Store, Replica}
	    end
    end.

% Split provided store at a certain position (Nkey)
% to hand it over to another node.
handover(Id, Store, Nkey, Npid) ->
    io:format("~w: -----Uncut store----- ~n~w~n", [Id,Store]),
    %{Leave, Keep} = storage:split(Nkey, Store),
    {Leave, Keep} = storage:splitstore(Store, Nkey, Id),
    SortedLeave = storage:sort(Leave),
    SortedKeep = storage:sort(Keep),
    io:format("-----~w: HANDOVER STORE(Nkey=~w)-----~n~w~n", [Id,Nkey,SortedLeave]),
    io:format("-----~w: KEEP STORE----- ~n~w~n", [Id,SortedKeep]),
    Npid ! {handover, SortedLeave},
    SortedKeep.

% Split provided replica at a certain position (Nkey)
% to hand it over to another node.
handoverReplica(Id, Replica, Nkey, Npid) ->
    io:format("-----~w: Uncut replica-----~n~w~n", [Id,Replica]),
    %if Nkey < Id ->
		{Leave, Keep} = storage:splitreplica(Replica, Nkey, Id),
		
    io:format("-----~w: Handover replica(Nkey=~w)-----~n~w~n", [Id,Nkey,Leave]),
    io:format("-----~w: Keep replica-----~n~w~n", [Id,Keep]),
    Npid ! {handoverReplica, Leave},
    Keep.

% Hand over given replica to given node (Pid).
newReplica(Id, Replica, Pid) ->
    %io:format("~w: Handing over full replica (~w) to ~w~n", [Id,Replica,Pid]),
    Pid ! {newReplica, Replica}.

% Hand over given replica to given node (Pid).
mergeReplica(Id, Replica, Pid) ->
    %io:format("~w: Handing over replica (~w) to merge on ~w~n", [Id,Replica, Pid]),
    Pid ! {mergeReplica, Replica}.

create_probe(Id, {_, _, Spid}) ->
	Spid ! {probe, Id, [Id], now()}.

remove_probe(T, Nodes) ->
	{_, BeforeS, BeforeNs} = T,
	{_, NowS, NowNs} = now(),
	Time = (NowS-BeforeS) * 1000000 + (NowNs-BeforeNs),
	io:format("Probe successfully finished:~n"
		  "Round trip time: ~w ns~n"
		  "Ring nodes: ~w~n",[Time, Nodes]).

forward_probe(Id, Ref, T, Nodes, {_, _, Spid}) ->
	Spid ! {probe, Ref, [Id|Nodes], T}.

forward_stop({_, _, Spid}) ->
	Spid ! stopall.

% To add a new key value we must first determine
% if our node is the node that should take care of the key.
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
    % A node will take care of all keys from (but not including) the identifier
    % of its predecessor to (and including) the identifier of itself.
    % If we are not responsible we simply send a add message to our successor.
    case key:between(Key, Pkey, Id) of
	true ->
	    Client ! {Qref, ok},
	    % Add key to the replica store
	    Spid ! {replicate, Key, Value},
            % Add key to the store
	    storage:add(Key, Value, Store);
	false ->
        % If we are not responsible we simply
	    % send a add message to our successor.
	    Spid ! {add, Key, Value, Qref, Client},
	    Store
    end.


lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    %Use the same distinction as in the add function
    case key:between(Key, Pkey, Id) of
	true ->
	    Result = storage:lookup(Key, Store),
	    Client ! {Qref, Result, Id};
	false ->
	    {_, _, Spid} = Successor,
	    Spid ! {lookup, Key, Qref, Client}    
    end.

monitor(Pid) ->
    erlang:monitor(process, Pid).
demonitor(nil) ->
    ok;
demonitor(Pid) ->
    erlang:demonitor(Pid, [flush]).

% This is executed if the down message comes from a nodes' predecessor
down(Id, Ref, {Pkey, Ref, _}, Successor, Next, Store, Replica) ->
    io:format("~w: Received DOWN from Pred ~w~n", [Id, Pkey]),
    % If our predecessor dies, we have to take care
    % about its keys, means, we merge our store with the replica!
    NewStore = storage:sort(storage:merge(Store, Replica)),
    % Since we have new keys in our store, we also have to inform
    % our successor to add these keys to his replica
    {Skey, _, Spid} = Successor,
    newReplica(Id, NewStore, Spid),
    {nil, Successor, Next, NewStore, storage:create()};

% This is executed if the down message comes from a nodes' successor
% In this case we have to remove the old monitor and create a new one,
% pointing to the new successor.
% It is also important that we give away our current store as replica
% for our new successor.
down(Id, Ref, Predecessor, {Skey, Ref, _}, {Nkey, Npid}, Store, Replica) ->
    io:format("~w: Received DOWN from Succ ~w~n", [Id, Skey]),
    demonitor(Ref),
    Nref = monitor(Npid),
    newReplica(Id, Store, Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.

