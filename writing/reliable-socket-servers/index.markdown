---
title: How to write reliable socket servers that survive crashes and restart?
date: 2022-05-08
---

This past weekend I was researching how to do zero-downtime deployments and
found the wonderful blog post [Dream Deploys: Atomic, Zero-Downtime
Deployments](https://alangrow.com/blog/dream-deploys-atomic-zero-downtime-deployments).

In it, Alan describes how separating listening on a socket and accepting
connections on it into different processes can keep a socket "live" at all
times even during a restart.

In this blog post I want to share that trick and document my understanding of
it.

## TODO

* Make service double multiplication service

## The problem with a crashing server

To illustrate the problem with a crashing server, we will use the example
server below.

`server-listen.py`:

```python
import socket

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    s.listen()
    print("listening on port 9000")
    print("accepting connections")
    while True:
        conn, addr = s.accept()
        with conn:
            data = conn.recv(100)
            number = int(data)
            conn.sendall(f"got number {number}\n".encode("ascii"))
```

This is a TCP server, listening on port 9000, reading numbers from clients, and
echoing them back. It assumes that the data received can be parsed as an
integer. If the parsing fails, the server crashes.

To test the behavior of the server, we have the following client:

`client.py`:

```python
import socket
import time

for i in range(21):
    if i > 0:
        try:
            with socket.socket() as s:
                s.connect(("localhost", 9000))
                if i == 5:
                    s.sendall(b"five\n")
                else:
                    s.sendall(f"{i}\n".encode("ascii"))
                message = s.recv(100).decode("ascii").rstrip()
                diff = int((time.perf_counter() - prev) * 1000)
                print(f"{diff}ms {message}")
        except:
            print(f"{i} failed")
        time.sleep(0.01)
    prev = time.perf_counter()
```

It sends 20 requests to the server with a 0.01s delay between them. However, on
the fifth request, instead of sending the number `5` it sends the string `five`
to cause the server to crash.

If we start the server, then the client, the output looks as follows:

`server output`

```
$ python server-listen.py 
listening on port 9000
accepting connections
Traceback (most recent call last):
  File "/home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-listen.py", line 13, in <module>
    number = int(data)
ValueError: invalid literal for int() with base 10: b'five\n'
```

`client output`

```
$ python client.py 
0ms got number 1
0ms got number 2
0ms got number 3
0ms got number 4
0ms 
6 failed
7 failed
8 failed
9 failed
10 failed
11 failed
12 failed
13 failed
14 failed
15 failed
16 failed
17 failed
18 failed
19 failed
20 failed
```

In the client output, we see that request number five never receives a response
from the server and that subsequent requests fail because the server has
crashed, and there is no one listening on port 9000.

## Solution: restart server in loop

In order for subsequent requests to succeed, we need to start the server again
after it has crashed. One way to do that is to run the server program in an
infinite loop.

`loop.sh`:

```bash
while true; do
    echo "$@"
    "$@" || echo "restarting"
done
```

This Bash script takes a command to run as argument and runs that program in a
loop, ignoring any exit code.

Invoking the server and client again, we get the following output:

`server output`

```
$ bash loop.sh python server-listen.py 
python server-listen.py
listening on port 9000
accepting connections
Traceback (most recent call last):
  File "/home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-listen.py", line 13, in <module>
    number = int(data)
ValueError: invalid literal for int() with base 10: b'five\n'
restarting
python server-listen.py
listening on port 9000
accepting connections
```

`client output`

```
$ python client.py 
0ms got number 1
0ms got number 2
0ms got number 3
0ms got number 4
0ms 
6 failed
7 failed
8 failed
9 failed
10 failed
11 failed
12 failed
13 failed
14 failed
15 failed
16 failed
0ms got number 17
0ms got number 18
0ms got number 19
0ms got number 20
```

In the server output, we see that the server starts again after the crash and
starts listening to port 9000.

In the client output, we see that request five fails the same way, but after a
few more request, it starts getting responses again at request 17.

## The problem with a restarting server

Running the server in a loop is an improvement. Instead of dropping all
subsequent requests, we only drop a few.

But during the time between the server crash and a new process been started,
there is no one listening on port 9000 and we still drop connections.

How can we make sure to not drop any connections?

## Solution: separate listening on a socket and accepting connections into different processes

Here is `server-listen-loop.py`:

```python
import os
import socket

with socket.socket() as s:
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(("localhost", 9000))
    s.listen()
    print("listening on port 9000")
    os.dup2(s.fileno(), 0)
    os.close(s.fileno())
    os.execvp("bash", ["bash", "loop.sh", "python", "server-accept.py"])
```

Here is `server-accept.py`:

```python
import socket

with socket.socket(fileno=0) as s:
    print("accepting connections")
    while True:
        conn, addr = s.accept()
        with conn:
            data = conn.recv(100)
            number = int(data)
            conn.sendall(f"got number {number}\n".encode("ascii"))
```

Again:

```
$ python server-listen-loop.py 
listening on port 9000
python server-accept.py
accepting connections
Traceback (most recent call last):
  File "/home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-accept.py", line 9, in <module>
    number = int(data)
ValueError: invalid literal for int() with base 10: b'five\n'
restarting
python server-accept.py
accepting connections
```

```
$ python client.py 
0ms got number 1
0ms got number 2
0ms got number 3
0ms got number 4
0ms 
108ms got number 6
0ms got number 7
1ms got number 8
0ms got number 9
0ms got number 10
0ms got number 11
0ms got number 12
0ms got number 13
0ms got number 14
0ms got number 15
0ms got number 16
0ms got number 17
0ms got number 18
0ms got number 19
0ms got number 20
```

We can see that request number six takes longer to complete. That is because
the server needs to start and `accept` the socket. But it doesn't fail. The
client will not get connection errors.

## Questions & Answers

### How long will a socket wait before timing out?

### Can we improve on the long startup time?

* Have another process in standby and tell it to start listening somehow?

### Why socket option REUSE?

### Is this how supervisor works?

Seems like it closes socket upon restart:

    [fcgi-program:test]
    socket=tcp://localhost:9000
    command=python /home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-accept.py

    2022-05-10 21:46:28,734 INFO exited: test (exit status 1; not expected)
    2022-05-10 21:46:28,734 INFO Closing socket tcp://localhost:9000
    2022-05-10 21:46:29,736 INFO Creating socket tcp://localhost:9000
    2022-05-10 21:46:29,737 INFO spawned: 'test' with pid 561624
    2022-05-10 21:46:30,740 INFO success: test entered RUNNING state, process has stayed up for > than 1 seconds (startsecs)

Can we solve it with a dummy process just to keep the socket open?

It can call `loop.sh python server-accept.py` instead. Of course! But then
again, we can as well use a regular program and create the socket ourselves.

### Why sleep in loop?

### Is dup needed?

Python file descriptors not inheritable.

### Is asyncio more reliable

Don't kill server if client request failed

### Can this mechanism be used for zero-downtime deploy

Well, yes, that is how I learned about it in the blog post.

### Can we use this technique to create a load balancer?
