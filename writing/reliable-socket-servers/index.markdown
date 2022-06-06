---
title: 'DRAFT: How to write reliable socket servers that survive crashes and restart?'
date: 2022-06-06
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

This past weekend I was researching how to do zero-downtime deployments and
found the wonderful blog post [Dream Deploys: Atomic, Zero-Downtime
Deployments](https://alangrow.com/blog/dream-deploys-atomic-zero-downtime-deployments).

In it, Alan describes how separating listening on a socket and accepting
connections on it into different processes can keep a socket "live" at all
times even during a restart.

In this blog post I want to document that trick and my understanding of it.

## TODO

* Make service double multiplication service
* Clean up client code

## The problem with a crashing server

To illustrate the problem with a crashing server, we use the example below.

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;accepting connections&quot;</span><span class="p">)</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">conn</span><span class="p">,</span> <span class="n">addr</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">accept</span><span class="p">()</span>
        <span class="k">with</span> <span class="n">conn</span><span class="p">:</span>
            <span class="n">data</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span>
            <span class="n">number</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">data</span><span class="p">)</span>
            <span class="n">conn</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;got number </span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
</pre></div>
</div></div>

This is a TCP server, listening on port 9000, reading numbers from clients, and
echoing them back. It assumes that the data received can be parsed as an
integer. If the parsing fails, the server crashes.

To test the behavior of the server, we use the following client:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>client.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>
<span class="kn">import</span> <span class="nn">time</span>

<span class="k">for</span> <span class="n">i</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="mi">21</span><span class="p">):</span>
    <span class="k">if</span> <span class="n">i</span> <span class="o">&gt;</span> <span class="mi">0</span><span class="p">:</span>
        <span class="k">try</span><span class="p">:</span>
            <span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
                <span class="n">s</span><span class="o">.</span><span class="n">connect</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
                <span class="k">if</span> <span class="n">i</span> <span class="o">==</span> <span class="mi">5</span><span class="p">:</span>
                    <span class="n">s</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">b</span><span class="s2">&quot;five</span><span class="se">\n</span><span class="s2">&quot;</span><span class="p">)</span>
                <span class="k">else</span><span class="p">:</span>
                    <span class="n">s</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">i</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
                <span class="n">message</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span><span class="o">.</span><span class="n">decode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">)</span><span class="o">.</span><span class="n">rstrip</span><span class="p">()</span>
                <span class="n">diff</span> <span class="o">=</span> <span class="nb">int</span><span class="p">((</span><span class="n">time</span><span class="o">.</span><span class="n">perf_counter</span><span class="p">()</span> <span class="o">-</span> <span class="n">prev</span><span class="p">)</span> <span class="o">*</span> <span class="mi">1000</span><span class="p">)</span>
                <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">diff</span><span class="si">}</span><span class="s2">ms </span><span class="si">{</span><span class="n">message</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="k">except</span><span class="p">:</span>
            <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">i</span><span class="si">}</span><span class="s2"> failed&quot;</span><span class="p">)</span>
        <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.01</span><span class="p">)</span>
    <span class="n">prev</span> <span class="o">=</span> <span class="n">time</span><span class="o">.</span><span class="n">perf_counter</span><span class="p">()</span>
</pre></div>
</div></div>

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
infinite loop using a script like the one below:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>loop.sh</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">while</span> true<span class="p">;</span> <span class="k">do</span>
    <span class="nb">echo</span> <span class="s2">&quot;</span><span class="nv">$@</span><span class="s2">&quot;</span>
    <span class="s2">&quot;</span><span class="nv">$@</span><span class="s2">&quot;</span> <span class="o">||</span> <span class="nb">true</span>
    <span class="nb">echo</span> <span class="s2">&quot;restarting&quot;</span>
<span class="k">done</span>
</pre></div>
</div></div>

This Bash script takes a command to run as argument and runs that command in a
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

How can we make sure to answer all connections?

## Solution: separate listening on a socket and accepting connections

The trick, as also demonstrated in the blog post, is to listen on the socket in
one process and accept connections and processing requests in another process.
That way, if processing fails, and that process dies, the socket still stays
open because it is managed by another process.

Here is a program that listens on a socket and then spawns another process in a
loop to accept connections:

Here is `server-listen-loop.py`:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen-loop.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="n">os</span><span class="o">.</span><span class="n">dup2</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">(),</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">close</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">())</span>
    <span class="n">os</span><span class="o">.</span><span class="n">execvp</span><span class="p">(</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="p">[</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="s2">&quot;loop.sh&quot;</span><span class="p">,</span> <span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;server-accept.py&quot;</span><span class="p">])</span>
</pre></div>
</div></div>

The first part of this program creates a socket and starts listening.

The last part starts executing the command `bash loop.sh python
server-accept.py`. At this point the process is listening on the socket and
starts the `server-accept.py` program in a loop.

The `server-accept.py` program is similar to `server-listen.py`, but instead of listening on port 9000, it just accepts connections on the socket which is passed to it as file descriptor 0 (stdin):

Here is `server-accept.py`:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-accept.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">(</span><span class="n">fileno</span><span class="o">=</span><span class="mi">0</span><span class="p">)</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;accepting connections&quot;</span><span class="p">)</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">conn</span><span class="p">,</span> <span class="n">addr</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">accept</span><span class="p">()</span>
        <span class="k">with</span> <span class="n">conn</span><span class="p">:</span>
            <span class="n">data</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span>
            <span class="n">number</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">data</span><span class="p">)</span>
            <span class="n">conn</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;got number </span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
</pre></div>
</div></div>

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

Now all requests that we send get a response. We see that request number six
takes longer to complete. That is because the server needs to start and
`accept` the socket. But it doesn't fail. The client will not get connection
errors.

And this is one way to write a reliable socket servers that survive crashes and
restarts.

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

### Unix domain socket vs. TCP socket
