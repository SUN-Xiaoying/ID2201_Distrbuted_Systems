-module(hist).
-export([new/1, update/3]).

%% Return a new history, where messages from Name will always be seen as old.
new(Name) ->
    [{Name, old}].

%% Check if message number N from the Node is old or new. If it is old then return old but if it new return {new, Updated} where Updated is the updated history.
update(Name, TrackingNr, History) ->
    case lists:keysearch(Name, 1, History) of
	{value, {Name, HistoryNr}} ->
	    if TrackingNr > HistoryNr ->
		    NewHistory = lists:keydelete(Name, 1, History),
		    {new, [{Name, TrackingNr}|NewHistory]};
	       true ->
		    old
	    end;
	%% no link-state messages of "Name" in History
	%% therefore we create a new entry
	false ->
	    {new, [{Name, TrackingNr}|History]}
end.
