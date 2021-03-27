-module(vtest).
-export([test/0]).


test() ->
    A = vclock:fresh(),
    B = vclock:fresh(),
    C = vclock:fresh(),

    C1 = vclock:increment(c, C),
    B1 = vclock:increment(b, B),
    C1B1 = vclock:merge([C1, B1]),
    B2 = vclock:increment(b, C1B1),

    A1 = vclock:increment(a, A),
    B2A1 = vclock:merge([B2, A1]),
    A2 = vclock:increment(a, B2A1),


    B3 = vclock:increment(b, B2),
    A2B3 = vclock:merge([A2, B3]),

    B3C1 = vclock:merge([B3, C1]),


    io:format("C1 happened before B2 = ~w (should be true)~n", [vclock:descends(B3, C1)]),
    io:format("C1 happened before B2C1 = ~w (should be true)~n", [vclock:descends(B3C1, C1)]),
    io:format("B2A1 happened before A2B3 = ~w (should be false)~n", [vclock:descends(B3C1, A2B3)]).




