-module(ex_12).

-export([lookup/1, start/0, store/2]).

processes_list() -> [pid1, pid2].

start() -> start_aux(processes_list()).

start_aux([]) -> true;
start_aux([H | TL]) ->
    register(H, spawn(fun () -> loop() end)), start_aux(TL).

store(Key, Value) ->
    store_aux(processes_list(), Key, Value).

store_aux([], _Key, _Value) -> true;
store_aux([H | TL], Key, Value) ->
    rpc(H, {store, {Key, Value}}),
    store_aux(TL, Key, Value).

lookup(Key) ->
    PList = processes_list(),
    Pid = lists:nth(rand:uniform(length(PList)), PList),
    rpc(Pid, {lookup, Key}).

rpc(To, Q) ->
    To ! {self(), Q},
    receive
      {true, Result} -> Result;
      {false, Reason} ->
	  io:format("Error in rpc call due to ~p\n", [Reason])
    end.

loop() ->
    receive
      {From, {store, {Key, Value}}} ->
	  put(Key, Value),
	  From ! {true, true},
	  loop();
      {From, {lookup, Key}} -> From ! {true, get(Key)}, loop()
    end.
