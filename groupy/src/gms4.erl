-module(gms4).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, start/2, init/3, init/4]).
-define(argh, 200).
-define(timeout, 2000).

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master,1, [], [Master],[]).

start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Master, Self},
	receive
		{view, N, [Leader|Slaves], Group} ->
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
	after ?timeout ->
		Master ! {error, "No reply from leader."}
	end.


leader(Id, Master,N, Slaves, Group, MsgList) ->
	receive
		{mcast, Msg} ->			
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			NewMsgList = [{N,Msg}|MsgList], %%store all multicasted msg
			leader(Id, Master, N+1, Slaves, Group, NewMsgList);
		{join, Wrk, Peer} ->
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master, N+1, Slaves2, Group2, MsgList);
		{resendMsg, I, P} ->			
			case lists:keyfind(I, 1, MsgList) of 
				{S,Message} ->
					io:format("(Leader): Resending msgs~n"),
					P ! {resend_msg, S,Message},
					leader(Id, Master, N, Slaves, Group, MsgList);
				false ->
					io:format("(Leader): Msg not found in list~n"),
					leader(Id, Master, N, Slaves, Group, MsgList)
				end;
		stop ->
			io:format("(Leader ~p): Stopped~n", [Id]),
			ok;
		Error ->
			io:format("(Leader ~p): Unknown msg received: ~w~n", [Id, Error])

	end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->	
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{join, Wrk, Peer} ->
			io:format("(Slave ~p): Join peer ~p~n",[Id,Peer]),
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);		
		{msg, I, _} when I < N ->
			io:format("(Slave ~p): Old Msg detected (~p<~p)~n", [Id, I, N]), 			
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, I, Msg} when I > N ->			
			Ns = lists:seq(N,I),			
			io:format("(Slave ~w): Msg with seq ~w lost!~n",[Id,Ns]),
			lists:map(fun(Missing) -> Leader ! {resendMsg, Missing, self()}  end, Ns),
			Master ! Msg,
			slave(Id, Master, Leader, I+1, {msg,I,Msg}, Slaves, Group);
		{msg, N, Msg} ->
			Master ! Msg,
			slave(Id, Master, Leader, N+1, {msg,N,Msg}, Slaves, Group);
		{view, I, [Leader|Slaves2], Group2} ->
			Master ! {view, Group2},
			slave(Id, Master, Leader, I+1, {view, I, [Leader|Slaves2], Group2}, Slaves2, Group2);
		{'DOWN', _Ref, process, Leader, _Reason} ->	
			io:format("(Slave ~p): Received DOWN~n", [Id]),		
			election(Id, Master, N, Last, Slaves, Group);
		{resend_msg, S,_} ->			
			io:format("(Slave ~w): Lost msg seq ~w received again~n",[Id,S]),
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		stop ->
			io:format("(Slave ~p): Stopped~n", [Id]),
			ok;
		Error ->
	    	io:format("(Slave ~p): Unknown msg received: ~w~n", [Id, Error])

end.

%% ====================================================================
%% Internal functions
%% ====================================================================


bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> 	 dropMsg(Node, Msg) end, Nodes).

crash(Id) ->
	case random:uniform(?argh) of
		?argh ->
			io:format("(Leader ~p): Crashed!~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.

dropMsg(Node, Msg) ->
	case random:uniform(?argh) of
		?argh ->
			io:format("Msg dropped for node ~w~n", [Node]),
			drop;
		_ ->
			Node ! Msg
	end.

% N is the expected sequence number of next message
% Last is a copy of the last message
election(Id, Master,N, Last, Slaves, [_|Group]) ->
	io:format("~p: Electing new leader...~n", [Id]),
	Self = self(),
	case Slaves of
		[Self|Rest] ->
			bcast(Id, Last, Rest),
			io:format("New leader: ~p!~n", [Id]),
			bcast(Id, {view,N,Slaves,Group},Rest),
			Master ! {view, Group},
			leader(Id, Master, N+1, Rest, Group,[]);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest, Group)
	end.
