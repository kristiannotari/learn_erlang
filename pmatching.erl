-module(pmatching).
-export([
    head/1,
    second/1,
    same/2
]).

head([H|_]) -> H.

second([_,X|_]) -> X.

same(X,X) -> true;
same(_,_) -> false.