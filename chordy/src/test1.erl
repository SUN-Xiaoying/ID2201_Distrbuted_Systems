-module(test1).
-export([start/1]).

start(Count)->
    A = node1:start(random:uniform(Count)),

    lists:map(fun(_) -> node1:start(random:uniform(Count),A),
			timer:sleep(10) end, 
	      lists:seq(0, Count, 1)),
    A.