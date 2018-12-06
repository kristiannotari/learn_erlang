-module(processes).
-compile(export_all).

start(N,A) -> spawn(processes, loop, [N,A]).

loop(0,A) -> io:format("~p(~p) ~p~n", [A, self(), stops]);
loop(N,A) -> io:format("~p(~p) ~p~n", [A, self(), N]), loop(N-1,A).