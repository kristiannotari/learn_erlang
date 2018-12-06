-module(ex_04).
-compile(export_all).

start() -> 
    case whereis(echo) of 
        undefined ->
            Pid = spawn(ex_04, body, []),
            register(echo, Pid),
            ok;
        _ -> ok
    end.   
    

print(Term) -> echo ! Term.

stop() -> 
    case whereis(echo) of 
        undefined ->
            ok;
        Pid -> 
            Pid ! {stop, "stopped by interface"},
            unregister(echo),
            ok
    end.

body() ->
    receive
        {stop, Reason} -> exit(Reason);
        Term -> 
            io:format("echo printing ~p~n", [Term]),
            body()
    end.