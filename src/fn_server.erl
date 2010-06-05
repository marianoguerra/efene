-module(fn_server).
-export([start/0, stop/0, set/2, get/1, server/1]).

start() ->
    case whereis(fn_server) of
        undefined ->
            Pid = spawn(?MODULE, server, [dict:new()]),
            register(fn_server, Pid),
            Pid;
        P -> P
    end.

stop() ->
    fn_server ! stop.

set(Key, Value) ->
    fn_server ! {set, self(), Key, Value},
    ok.

get(Key) ->
    fn_server ! {get, self(), Key},
    receive Response -> Response end.

server(Data) ->
    receive
        stop ->
            normal;
        {set, Pid, Key, Value} ->
            NewData = store(Pid, Data, Key, Value),
            server(NewData);
        {get, Pid, Key} ->
            Pid ! fetch(Pid, Data, Key),
            server(Data)
    end.

store(Pid, Dict, Key, Value) ->
    case dict:is_key(Pid, Dict) of
        true ->
            PidDict = dict:fetch(Pid, Dict),
            NewPidDict = dict:store(Key, Value, PidDict),
            dict:store(Pid, NewPidDict, Dict);
        false ->
            PidDict = dict:new(),
            NewPidDict = dict:store(Key, Value, PidDict),
            dict:store(Pid, NewPidDict, Dict)
    end.

fetch(Pid, Dict, Key) ->
    case dict:is_key(Pid, Dict) of
        true ->
            PidDict = dict:fetch(Pid, Dict),
            dict:find(Key, PidDict);
        false ->
            error
    end.

