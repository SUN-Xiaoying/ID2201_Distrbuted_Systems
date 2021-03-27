-module(storage).
-export([create/0,add/3,lookup/2,splitreplica/3,splitstore/3,merge/2,sort/1]).

create() ->
    [].

% Add a key-value pair to a store
% and return the new store
add(Key, Value, Store) ->
    NewStore = [{Key, Value}|Store],
    sort(NewStore).

% Find a value for a given key
lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

% Splits the storage into two pieces given a key
splitreplica(Store, Key, Skey) when Key < Skey ->
    {Part1, Part2} = lists:splitwith(fun({A,_}) -> A =< Key end, sort(Store)),
    {Part3, Part4} = lists:splitwith(fun({A,_}) -> A =< Skey end, Part1),
    {sort(Part4), sort(Part2++Part3)};
splitreplica(Store, Key, Skey) when Key >= Skey ->
    {Part1, Part2} = lists:splitwith(fun({A,_}) -> A =< Key end, sort(Store)),
    {Part3, Part4} = lists:splitwith(fun({A,_}) -> A =< Skey end, Part1),
    {sort(Part2++Part3), sort(Part4)}.


% Returns {LEAVE, KEEP}
splitstore(Store, Key, Skey) when Key >= Skey ->
    {Part1, Part2} = lists:splitwith(fun({A,_}) -> A =< Key end, sort(Store)),
    {Part3, Part4} = lists:splitwith(fun({A,_}) -> A =< Skey end, Part1),
    {sort(Part4), sort(Part2++Part3)};
splitstore(Store, Key, Skey) when Key < Skey ->
    {Part1, Part2} = lists:splitwith(fun({A,_}) -> A =< Key end, sort(Store)),
    {Part3, Part4} = lists:splitwith(fun({A,_}) -> A =< Skey end, Part2),
    {sort(Part1++Part4), sort(Part3)}.


% Merges two stores together
%merge(Store, []) ->
%    Store;
%merge([], Store) ->
%    Store;
merge(StoreA, StoreB) ->
    lists:append(StoreA, StoreB).
    %NewStore = lists:append(lists:keysort(1, StoreA), lists:keysort(1, StoreB))
    %lists:keysort(1, NewStore).

sort(Store) ->
    lists:keysort(1, Store).
