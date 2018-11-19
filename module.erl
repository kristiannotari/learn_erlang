-module(module).
-export([hello/0, add/2]).
-define(macroAdd(A,B), A + B).

hello() -> io:format("Hello world\n").

add(A,B) -> ?macroAdd(A,B).