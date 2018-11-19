-module(ifs).
-export([
    test/0,
    rightTest/0,
    moreReadableTest/0
]).

%% if as guard (should warn when compiled for no true branch).
%% the warning is complaining about the absence of an "else" statement which
%% can be reached if all ifs evaluate to false
test() ->
    if 1 =:= 1 -> true
    end,
    if 1 =:= 2 -> true
    end,
    if 1 =:= 2, 1 =:= 1 -> false
    end.

%% if as guard (should be fine).
%% the "true" acts like an "else" statement
rightTest() ->
    if 1 =:= 1 -> true;
    true -> false
    end.

%% this is correct as guard and is the preferable way to write an else statement
%% try to avoid "else" or "true" branches
moreReadableTest() -> 
    if 1 =:= 1 -> true
     ; 1 =/= 1 -> false
    end.