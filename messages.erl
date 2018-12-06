-module(messages).
-export([t_converter/0]).

t_converter() -> 
    receive
        {toF, C} -> io:format("~p °C is ~p °F~n", [C, 32+C*9/5]), t_converter();
        {toC, F} -> io:format("~p °F is ~p °C~n", [F, (F-32)*5/9]), t_converter();
        {stop} -> io:format("Stopping!~n");
        Other -> io:format("Unknown: ~p~n", [Other]), t_converter()
    end.