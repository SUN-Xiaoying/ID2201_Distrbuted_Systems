-module(test).
-export([start/0, stop/1]).

start() ->
    Log = loggy:start([aa, bb, cc, dd]),
    A = worker:start(aa, Log, 10, 1000, 100),
    B = worker:start(bb, Log, 34, 1000, 100),
    C = worker:start(cc, Log, 45, 1000, 100),
    D = worker:start(dd, Log, 19, 1000, 100),
    A ! {peers, [B, C, D]},
    B ! {peers, [A, C, D]},
    C ! {peers, [A, B, D]},
    D ! {peers, [A, B, C]},
    [Log, A, B, C, D].

stop([Log, A, B, C, D]) ->
    Log ! stop,
    A ! stop,
    B ! stop,
    C ! stop,
    D ! stop.
