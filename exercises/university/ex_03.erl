-module(ex_03).
-compile(export_all).

start_by_master(N,M,Message) -> start_by_master_aux(N,M,Message,[]).
 
start_by_master_aux(0,_M,_Message,[First|Ring]) ->
    CompleteRing = lists:append([First|Ring], [First]),
    io:format("Ring: ~p~n", [CompleteRing]),
    link_rings(CompleteRing);
start_by_master_aux(N,M,Message,Ring) ->
    Pid = spawn(ex_03, ring_process_start, [M,Message]),
    io:format("Spawning ~p~n", [Pid]),
    start_by_master_aux(
        N-1,
        M,
        Message,
        [Pid|Ring]
    ).

link_rings([]) -> io:format("Finished linking~n", []);
link_rings([_R]) -> io:format("Finished linking~n", []);
link_rings([R1|[R2|Ring]]) ->
    R1 ! {next, R2},
    link_rings([R2|Ring]).

ring_process_start(M,Message) ->
    receive 
        {next, Pid} -> 
            io:format("~p linked to ~p~n", [self(), Pid]),
            ring_send(M,Message,Pid)
    end.

ring_send(0,_Message,Dest) -> 
    io:format("~p finished sending messages to ~p~n", [self(), Dest]),
    io:format("~p started listening to messages~n", [self()]),
    ring_listen();
ring_send(M,Message,Dest) -> 
    Dest ! {message, Message},
    ring_send(M-1,Message,Dest).
    
ring_listen() ->
    receive 
        {message, Message} -> 
            io:format("~p received ~p~n", [self(), Message]),
            ring_listen();
        {stop, Message} ->
            io:format("~p stopped due to ~p~n", [self(), Message])
    end.