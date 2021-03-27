-module(worker).
-export([start/5, stop/1]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
	{peers, Peers} ->
	    loop(Name, Log, Peers, Sleep, Jitter, 0);
	stop ->
	    ok
    end.

loop(Name, Log, Peers, Sleep, Jitter, LocalTime)->
    Wait = random:uniform(Sleep),
    receive
	{msg, RemoteTime, Msg} ->
		%Lamport clock
	    NewTime = (erlang:max(LocalTime, RemoteTime)+1),
	    Log ! {log, Name, NewTime, {received, Msg}},
	    loop(Name, Log, Peers, Sleep, Jitter, NewTime);
	stop ->
	    ok;
	Error ->
	    Log ! {log, Name, time, {error, Error}}
    after Wait ->
	    Selected = select(Peers),
	    NewTime = LocalTime + 1,
	    Delay = random:uniform(Jitter),
	    Message = {hello, Delay},
	    Selected ! {msg, NewTime, Message},
	    timer:sleep(Delay),
	    Log ! {log, Name, NewTime, {sending, Message}},
	    loop(Name, Log, Peers, Sleep, Jitter, NewTime)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).
