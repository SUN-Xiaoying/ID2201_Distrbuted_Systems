-module(loggy).
-export([start/1, stop/1]).

%% raw
start(Nodes) ->
    spawn(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(_) ->
    loop([], []).

loop(Queue, Workers) ->
    receive
	{log, From, Time, Msg} ->
	    %Update workers list with actual time stamps, sort them to get the lowest time value top most
	    NewWorkers = lists:keystore(From, 1, Workers, {From, Time}),
	    SortedWorkers = lists:keysort(2, NewWorkers),
	    %Get the lowest worker clock and store it in MinWorkerTime
	    [{_, MinWorkerTime}|_] = SortedWorkers,
	    %io:format("Workers (sorted): ~p~n", [SortedWorkers]),
	   
	    AppendedQueue = lists:keysort(2, [{From, Time, Msg}|Queue]),
		{OutputQueue, BufferQueue} = lists:splitwith(fun({_,T,_})-> T<MinWorkerTime end, AppendedQueue),

		%list_print(BufferQueue),
	    print(OutputQueue),
	    loop(BufferQueue, NewWorkers);	
	    
	stop ->
	    ok
    end.

print([]) ->
    ok;
print(Queue) ->
    SortedQueue = lists:keysort(2, Queue),
	%list_print(SortedQueue),

    % Counter = lists:foldl(fun(_, I) -> I+1 end, 0, Queue),
    % io:format("- Queue length: ~w~n", [Counter]),
	lists:foreach(fun(X) -> log(X) end, SortedQueue).
	
	% list_print([])-> [];
	% list_print([H|T]) when H rem 2 /= 0 ->
	% 	list_print(T);
	% list_print([H|T]) ->
	% 	io:format("\t~p~n", [H]),
	% 	[H|list_print(T)].

log({From, Time, Msg}) ->
	io:format("---------------------~n"),
	io:format("log: ~w ~w ~p~n", [From, Time, Msg]).


