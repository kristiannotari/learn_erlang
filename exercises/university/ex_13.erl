-module(ex_13).

-export([start/1, to_slave/2]).

start(N) when N < 1 ->
    throw({error, "N should be >= 1"});
start(N) ->
    register(master, spawn(fun () -> master_init(N) end)).

master_init(N) ->
    process_flag(trap_exit, true),
    Slaves = master_start_slaves(N, []),
    io:format("~p: List of active slaves: ~p\n", [self(), Slaves]),    
    master_relay(Slaves).

master_start_slaves(0, Acc) -> Acc;
master_start_slaves(N, Acc) ->
    Pid = spawn_link(fun () -> slave() end),
    master_start_slaves(N - 1, [Pid | Acc]).

master_relay(Slaves) ->
    receive
      {_From, {Message, Slave}} ->
        case length(Slaves) >= Slave of
            true -> 
                lists:nth(Slave, Slaves) ! {master, Message},
                case master_listen_to_slave() of
                    restart -> master_relay(master_restart(Slaves, Slave));
                    _ -> master_relay(Slaves)
                end;
            false ->
                io:format("~p: Master received invalid Slave number", [self()]),
                master_relay(Slaves)
        end
    end.

master_listen_to_slave() ->
    receive
      {'EXIT', Pid, Reason} -> io:format("~p: Master received that process ~p has exited with reason ~p", [self(), Pid, Reason]), restart;
      _ -> true
    end.

master_restart(Slaves, Slave) ->
    io:format("\n~p: Restarting slave: ~p", [self(), Slave]),
    NewSlaves = lists:sublist(Slaves, Slave - 1) ++
      [spawn_link(fun () -> slave() end)] ++
	lists:sublist(Slaves, Slave + 1, length(Slaves)),
    io:format("\n~p: List of active slaves: ~p", [self(), NewSlaves]),
    NewSlaves.

to_slave(Message, N) -> master ! {self(), {Message, N}}, true.

slave() ->
    receive
      {_From, Message} ->
	  case Message of
	    die -> io:format("~p: Received 'die' message\n", [self()]), exit("Received 'die' message");
	    _ -> io:format("~p: Received: ~p\n", [self(), Message]), slave()
	  end
    end.
