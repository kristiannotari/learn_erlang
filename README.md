# learn_erlang

Repository for grouping erlang exercises and concepts

## Intro

Erlang is (mostly) a functional programming language which is based on prolog (in fact syntax is similar).

It was created for communication, and has several ways of distribute itself in different nodes and processes, also on different machines, and link them togheter to form a concurrent working environment.

It uses parallelism when possible to run multiple processes at the same time.

Concurrency is handled by the actor model. Processes send messages one another, and can only communicate waiting (for) and sending them. Every actor has a mailbox where messages arrives (order is not observed) and are processed from the oldest one to the new one.

All communications are performed asynchronously.

## List comprehensions

You can create new lists by applying a special construct: list comprehension.

```erlang
[ X+2 || X <- List, P(X)]
```

This construct can be seen as a set definition. In the final set, we will have every `X+2` numbers which are taken from a list `List`, and which succesfully pass the predicate `P` (or every boolean operations). So the common structure of the list comprehension is:

`[ E || Q1, Q2, Q3, ... ]`

where

- `E` is an arbitatry expression which return the *current* element to put in the new list
- `Q1, Q2, Q3` is a series of qualifiers, which can be generators, for example `X <- [1, 2, 3]` or filters, boolean conditions which should be true to add the *current element under revision* in the list, for example `X > 2`

## Guards

A guard is a series of booleand conditions which only admits a subset of erlang functions, in order to avoid side effects.

In fact they're used as boolean requirement to access a function body definition.

```erlang
echo_only_positive(X) when X <= 0 -> {error, "Only positive numbers"}.
echo_only_positive(X) -> X.
```

Valid guards expressions are:

- the atom true and some other constants
- calls to some built-in functions
- arithmetic and boolean expressions
- short-circuit expressions (`andalso`/`orelse`)

## Processes

Every process has a pid inside the virtual machine. You can spawn new ones (attached to functions) with the `spawn` function, which return the pid of the newly created process.

### Registering processes

You can register the name of a process with the `register` function, or undo it by using `unregister`.

### Sending messages

You can send a message to another process using the special construct:

```erlang
Dest ! Message
```

where `Message` can be any expression which evaluates to the object to be sent and `Dest` must evaluates to an actor identifier (Pid or registered name).

### Receiving messages

To receive messages there's a special construct also:

```erlang
receive
    Any -> do_something()
end
```

where pattern matching is used to capture what has been sent (so you can replace `Any` with whatever you need).

To avoid waiting forever there's also a special `after` clause which takes a number (milliseconds) to wait for, then wake up the receiver

```erlang
receive
    Any -> do_something()
after 2000 -> do_something_else()
end
```

## Error handling

Processes can be linked (either in a two way link or a one way link, which is called monitor).

Linked processes are affected by the behaviour of each one. For example we can monitor one process to react when it dies. Or to die togheter with him.

The set of processes linked to a given process is called `link set`.

### Exit signals

They're generated by a process when it dies and are broadcast to all the processes in its link set. They could be generated automically or explicitly with the `exit` function (which can also fake deaths).

Exit signals have a reason parameter which is set to `normal` if the process "naturally died".

### Receiving an exit signal

- a non system process that receives a exit signal dies too
- a system process, instead, receives the signal as a normal message in its mailbox

### System processes

A process can evolve to a system process by calling:

```erlang
process_flag(trap_exit, true)
```

### Examples

- I don’t care if a process I create crashes

```erlang
Pid = spawn(fun() ->... end)
```

- I want to die if a process I create crashes

```erlang
Pid = spawn_link(fun() ->... end)
```

- I want to handle errors if a process I create crashes. We  set us as a system process then spawn link a new process

```erlang
process_flag(trap_exit, true)
Pid = spawn_link(fun() ->... end)
```

## Distribution

Erlang provides two models of distribution:

- distributed Erlang
- socket based distribution

### Distributed Erlang

- applications run on a set of tightly coupled computers called Erlang nodes
- processes can be spawned on every node
- apart from the spawning, all things still work as always

Each node is a self contained Erlang system VM with its own address space and own set of processes. The access is secured by a cookie system. Each node has a cookie and in order to communicate with another node, each one cookie has to be the same.

A set of nodes with the same cookie define a cluster.

Three ways to set the cookie:

1. storing the cookie in $HOME/.erlang.cookie file
2. through the option `-setcookie`: `erl -setcookie "Magic Cookie"`
3. by using the BIF `erlang:set_cookies`:

```erlang
erlang:set_cookie(node(), 'Magic Cookie')
```

### Socket based distribution

- it can run in an untrusted environment
- less powerful (restricted conventions)
- fine grained control on what can be executed on a node

The problem with spawn-based distribution (distributed erlang) is that the client can spawn any process on the server machine. That is perfect when you own all the machines and you want to control them from a single one but is not suited when different people own the machines and want control what is in execution.

Socket-based distribution use a restricted form of spawn, where the owner of a machine has explicit control over what is run on his machine.

Usually there's a config file where ports and services are listed. A service is described by a name, a password, a module, a function of the module and arguments for that function.

Then when we want to connect to the server, it spawn the service we configured.