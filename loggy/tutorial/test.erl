-module(test).
-export([run/2]).

run(Sleep, Jitter)->
    Log = loggy:start([aa, bb, cc, dd]),
    A = worker:start(aa, Log, 13, 1000, 100),
    B = worker:start(bb, Log, 23, 1000, 100),
    C = worker:start(cc, Log, 36, 1000, 100),
    D = worker:start(dd, Log, 49, 1000, 100),
    worker:peers(A,[B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(5000),
    loggy:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).
