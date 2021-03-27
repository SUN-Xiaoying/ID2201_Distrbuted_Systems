-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

%% return an empty set of interfaces.
new() ->
    [].

%% add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
    %delete existing interfaces with given Name
    NewIntf = remove(Name, Intf),
    %append new entry to Intf list
    [{Name, Ref, Pid}|NewIntf].

%% remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

%% find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf)  ->
    case lists:keysearch(Name, 1, Intf) of
	{value, {_, _, Pid}} ->
	    {ok, Pid};
	 false ->
	    notfound
    end.

%% find the reference given a name and return {ok, Ref} or notfound.
ref(Name, Intf) ->
    case lists:keysearch(Name, 1, Intf) of
	{value, {_, Ref, _}} ->
	    {ok, Ref};
	 false ->
	    notfound
    end.

%% find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) ->
    case lists:keysearch(Ref, 2, Intf) of
	{value, {Name, _, _}} ->
	    {ok, Name};
	 false ->
	    notfound
    end.

%% return a list with all names.
list(Intf) ->
    lists:map(fun({N,_,_}) -> N end, Intf).

%list(Intf) ->
%    listAllNames(Intf).
%listAllNames([]) ->
%    [];
%listAllNames([{Name,_,_}|Rest]) ->
%    lists:append([Name], listAllNames(Rest)).

%% send the message to all interface processes.
%broadcast(Message, Intf) ->
%    sendMessage(Message, Intf).
%sendMessage(_, []) ->
%    [];
%sendMessage(Message, [{_,_,Pid}|Rest]) ->
%    Pid ! Message,
%    sendMessage(Message, Rest).

broadcast(Message, Intf) ->
 lists:map(fun({_,_,C}) -> C ! Message end, Intf).
