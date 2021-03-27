-module(key).
-export([generate/0,between/3]).
-define(HASHSIZE, 1000000000).

generate() ->
    random:uniform(?HASHSIZE).

between(Key, From, To) ->
    if From < To ->
	    if From < Key, Key =< To -> %andalso?
		    true; %(1, 0, 2),(1, 0, 1)
	       true ->
		    false
	    end;
   
       From > To ->
	    if From < Key; Key =< To -> %orelse, allow alternate guards in the same clause
		    true; % (2. 1. 0)
	       true ->
		    false
	    end;
       From == To ->
	    true
    end.
