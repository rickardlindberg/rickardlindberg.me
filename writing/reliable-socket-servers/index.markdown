---
title: 'DRAFT: How to write reliable socket servers that survive crashes and restarts?'
date: 2022-08-07
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

A few months ago, I was researching how to do zero-downtime deployments and
found the wonderful blog post [Dream Deploys: Atomic, Zero-Downtime
Deployments](https://alangrow.com/blog/dream-deploys-atomic-zero-downtime-deployments).

In it, Alan describes how separating listening on a socket and accepting
connections on it into different processes can keep a socket "live" at all
times even during a restart.

In this blog post I want to document that trick and my understanding of it.

## The problem with a crashing server

To illustrate the problem with a crashing server, we use the example below:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">conn</span><span class="p">,</span> <span class="n">addr</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">accept</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;accepting connection&quot;</span><span class="p">)</span>
        <span class="k">with</span> <span class="n">conn</span><span class="p">:</span>
            <span class="n">data</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span>
            <span class="n">number</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">data</span><span class="p">)</span>
            <span class="n">conn</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">*</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">=</span><span class="si">{</span><span class="n">number</span><span class="o">*</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
</pre></div>
</div></div>

This is a TCP server, listening on port 9000, reading numbers from clients, and
returning the product of the two numbers. It assumes that numbers can be parsed
as integers. If parsing fails, the server crashes.

To test the behavior of the server, we use the following client:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>client.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>
<span class="kn">import</span> <span class="nn">time</span>

<span class="k">def</span> <span class="nf">make_request</span><span class="p">(</span><span class="n">number</span><span class="p">):</span>
    <span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
        <span class="n">s</span><span class="o">.</span><span class="n">connect</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
        <span class="k">if</span> <span class="n">number</span> <span class="o">==</span> <span class="mi">5</span><span class="p">:</span>
            <span class="n">s</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">b</span><span class="s2">&quot;five</span><span class="se">\n</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="n">s</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
        <span class="k">return</span> <span class="n">s</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span><span class="o">.</span><span class="n">decode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">)</span><span class="o">.</span><span class="n">rstrip</span><span class="p">()</span>

<span class="k">for</span> <span class="n">i</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="mi">20</span><span class="p">):</span>
    <span class="k">try</span><span class="p">:</span>
        <span class="n">time_start</span> <span class="o">=</span> <span class="n">time</span><span class="o">.</span><span class="n">perf_counter</span><span class="p">()</span>
        <span class="n">message</span> <span class="o">=</span> <span class="n">make_request</span><span class="p">(</span><span class="n">i</span><span class="p">)</span>
        <span class="n">time_end</span> <span class="o">=</span> <span class="n">time</span><span class="o">.</span><span class="n">perf_counter</span><span class="p">()</span>
        <span class="n">diff</span> <span class="o">=</span> <span class="nb">int</span><span class="p">((</span><span class="n">time_end</span> <span class="o">-</span> <span class="n">time_start</span><span class="p">)</span> <span class="o">*</span> <span class="mi">1000</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">message</span><span class="p">:</span>
            <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">message</span><span class="si">}</span><span class="s2"> (request took </span><span class="si">{</span><span class="n">diff</span><span class="si">}</span><span class="s2">ms)&quot;</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;No response for </span><span class="si">{</span><span class="n">i</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">)</span>
    <span class="k">except</span><span class="p">:</span>
        <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;Connection failed for </span><span class="si">{</span><span class="n">i</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">)</span>
    <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.01</span><span class="p">)</span>
</pre></div>
</div></div>

It sends 20 requests to the server with a 10ms delay between them. However, for
request with number 5, instead of sending the number `5` it sends the string
`five` to cause the server to crash.

If we start the server, then the client, the output looks as follows:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">server output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ python server-listen.py 
listening on port 9000
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
Traceback (most recent call last):
  File &quot;/home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-listen.py&quot;, line 13, in &lt;module&gt;
    number = int(data)
ValueError: invalid literal for int() with base 10: b&#39;five\n&#39;
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">client output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ python client.py 
0*0=0 (request took 1ms)
1*1=1 (request took 0ms)
2*2=4 (request took 0ms)
3*3=9 (request took 0ms)
4*4=16 (request took 0ms)
No response for 5
Connection failed for 6
Connection failed for 7
Connection failed for 8
Connection failed for 9
Connection failed for 10
Connection failed for 11
Connection failed for 12
Connection failed for 13
Connection failed for 14
Connection failed for 15
Connection failed for 16
Connection failed for 17
Connection failed for 18
Connection failed for 19
</pre></div>
</div></div>
In the client output, we see that request with number 5 never receives a
response from the server and that subsequent requests fail because the server
has crashed, and there is no one listening on port 9000.

## Solution: restart the server in a loop

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

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">server output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ bash loop.sh python server-listen.py
python server-listen.py
listening on port 9000
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
Traceback (most recent call last):
  File &quot;/home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-listen.py&quot;, line 13, in &lt;module&gt;
    number = int(data)
ValueError: invalid literal for int() with base 10: b&#39;five\n&#39;
restarting
python server-listen.py
listening on port 9000
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">client output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ python client.py 
0*0=0 (request took 1ms)
1*1=1 (request took 0ms)
2*2=4 (request took 0ms)
3*3=9 (request took 1ms)
4*4=16 (request took 0ms)
No response for 5
Connection failed for 6
Connection failed for 7
Connection failed for 8
Connection failed for 9
Connection failed for 10
Connection failed for 11
Connection failed for 12
Connection failed for 13
14*14=196 (request took 0ms)
15*15=225 (request took 0ms)
16*16=256 (request took 0ms)
17*17=289 (request took 0ms)
18*18=324 (request took 0ms)
19*19=361 (request took 1ms)
</pre></div>
</div></div>
In the server output, we see that the server starts again after the crash and
starts listening on port 9000.

In the client output, we see that request with number 5 fails the same way, but
after a few more requests, it starts getting responses again at request with
number 14.

## The problem with a restarting server

Running the server in a loop is an improvement. Instead of dropping all
subsequent requests, we only drop a few.

But during the time between the server crash and a new server being up, there
is no one listening on port 9000 and we still drop connections.

How can we make sure to handle all requests?

## Solution: separate listening on a socket and accepting connections

The trick, as also demonstrated in Alan's blog post, is to listen on the socket
in one process and accept connections and processing requests in another
process.  That way, if processing fails, and that process dies, the socket
still stays open because it is managed by another process.

Here is a program that listens on a socket and then spawns server processes in
a loop to accept connections:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen-loop.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">dup2</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">(),</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">close</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">())</span>
    <span class="n">os</span><span class="o">.</span><span class="n">execvp</span><span class="p">(</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="p">[</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="s2">&quot;loop.sh&quot;</span><span class="p">,</span> <span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;server-accept.py&quot;</span><span class="p">])</span>
</pre></div>
</div></div>

The first part of this program creates a socket and starts listening. This is
what we had in the previous example.

The second part moves the file descriptor of the socket to file descriptor 0
(stdin) to make it available to child processes.

The third part replaces the current process with `bash loop.sh python
server-accept.py`. At this point the process is listening on the socket and
starts the `server-accept.py` program in a loop. As long as the `loop.sh`
script doesn't exit, there will be someone listening on port 9000.

The `server-accept.py` program is similar to `server-listen.py`, but instead of
listening on port 9000, it just accepts connections on the socket which is
passed to it as file descriptor 0 (stdin):

Here is `server-accept.py`:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-accept.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">(</span><span class="n">fileno</span><span class="o">=</span><span class="mi">0</span><span class="p">)</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">conn</span><span class="p">,</span> <span class="n">addr</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">accept</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;accepting connection&quot;</span><span class="p">)</span>
        <span class="k">with</span> <span class="n">conn</span><span class="p">:</span>
            <span class="n">data</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span>
            <span class="n">number</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">data</span><span class="p">)</span>
            <span class="n">conn</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">*</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">=</span><span class="si">{</span><span class="n">number</span><span class="o">*</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
</pre></div>
</div></div>

Invoking the server and client again, we get the following output:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">server output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ python server-listen-loop.py
listening on port 9000
python server-accept.py
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
Traceback (most recent call last):
  File &quot;/home/rick/rickardlindberg.me/writing/reliable-socket-servers/server-accept.py&quot;, line 9, in &lt;module&gt;
    number = int(data)
ValueError: invalid literal for int() with base 10: b&#39;five\n&#39;
restarting
python server-accept.py
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
accepting connection
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">client output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ python client.py 
0*0=0 (request took 0ms)
1*1=1 (request took 0ms)
2*2=4 (request took 1ms)
3*3=9 (request took 0ms)
4*4=16 (request took 0ms)
No response for 5
6*6=36 (request took 106ms)
7*7=49 (request took 0ms)
8*8=64 (request took 1ms)
9*9=81 (request took 0ms)
10*10=100 (request took 0ms)
11*11=121 (request took 1ms)
12*12=144 (request took 0ms)
13*13=169 (request took 0ms)
14*14=196 (request took 0ms)
15*15=225 (request took 0ms)
16*16=256 (request took 1ms)
17*17=289 (request took 0ms)
18*18=324 (request took 0ms)
19*19=361 (request took 1ms)
</pre></div>
</div></div>
Now all requests (except the one that causes a crash) get a response. We see
that request with number six takes longer to complete. That is because
`server-accept.py` needs time to start up (by the loop script) and call
`accept` on the socket. But the request doesn't fail. The client will not get a
connection error.

And this is one way to write reliable socket servers that survive crashes and
restarts.

## Questions & Answers

### How long will a socket wait before timing out?

I tried to modify the loop script to sleep for 60 seconds before restarting the
server:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>loop-sleep.sh</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">while</span> true<span class="p">;</span> <span class="k">do</span>
    <span class="nb">echo</span> <span class="s2">&quot;</span><span class="nv">$@</span><span class="s2">&quot;</span>
    <span class="s2">&quot;</span><span class="nv">$@</span><span class="s2">&quot;</span> <span class="o">||</span> <span class="nb">true</span>
    <span class="nb">echo</span> <span class="s2">&quot;restarting&quot;</span>
    sleep <span class="m">60</span>
<span class="k">done</span>
</pre></div>
</div></div>

The client output looked like this:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li><span class="cp">client output
</span></li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span>...
No response for 5
6*6=36 (request took 60123ms)
7*7=49 (request took 0ms)
...
</pre></div>
</div></div>
So it seems that the client got no errors even though the request took 60
seconds to be responded to.

I suppose you can put a timeout in the client code. But this question was about
how long the operating system on the server will keep the connection "alive"
even though no one calls `accept`.

I suppose the operating system has some kind of buffer. Say that there are
multiple clients making requests at the same time and the server never calls
`accept` during that time. Eventually some buffer must be exceeded and
connections get dropped.

(It seems that the client hangs on the `s.recv` call. That means that the
request was sent to the server and must have filled up some buffer.)

If anyone can point me to documentation where I can read about this behavior,
please drop me a line.

### Can we decrease the startup time?

When the loop script restarts the server, it takes ~100ms for it to come up and
process requests. How can we decrease that time?

One way would be to modify the loop script to spawn multiple server processes.
That way, if one crashes, the other can serve the next request.

This would also make the server code concurrent. That is, no "global" state can
reside in the server process, because we don't know which server process will
serve the next request.

Another solution might be to have a second process in standby mode. So the loop
script starts a second server process, but it stops it right before calling
accept. But then we would need a way to signal to the process to resume
operation. Perhaps by sending it a signal?

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-accept-standby.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">(</span><span class="n">fileno</span><span class="o">=</span><span class="mi">0</span><span class="p">)</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="c1"># wait for signal before proceeding</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">conn</span><span class="p">,</span> <span class="n">addr</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">accept</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;accepting connection&quot;</span><span class="p">)</span>
        <span class="k">with</span> <span class="n">conn</span><span class="p">:</span>
            <span class="n">data</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span>
            <span class="n">number</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">data</span><span class="p">)</span>
            <span class="n">conn</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">*</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">=</span><span class="si">{</span><span class="n">number</span><span class="o">*</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
</pre></div>
</div></div>

Both of these make the loop script more complicated. And if it gets more
complicated, it is more likely to crash. And if it crashes, the socket gets
closed, and subsequent requests will get connection errors.

### Can we use this technique to create a load balancer?

Well, yes.

If the loop script spawns multiple server processes, the operating system will
load balance between them.

No fancy load balancing software needed.

### Why do we need to move the socket file descriptor?

In the middle of `server-listen-loop.py` we move the file descriptor of the
socket, `s.fileno()`, to file descriptor 0 (stdin):

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen-loop.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">dup2</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">(),</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">close</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">())</span>
    <span class="n">os</span><span class="o">.</span><span class="n">execvp</span><span class="p">(</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="p">[</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="s2">&quot;loop.sh&quot;</span><span class="p">,</span> <span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;server-accept.py&quot;</span><span class="p">])</span>
</pre></div>
</div></div>

We do that to make the file descriptor available to child processes so that
they can create a socket using it and then call `accept`.

In Python, the file descriptor of the socket is [not inheritable by
default](https://docs.python.org/3/library/os.html#fd-inheritance). That is, a
child process will not be able to access the socket file descriptor.  That is
why we have to move it to file descriptor 0 (stdin) which is inherited.

Another option might be to make the file descriptor inheritable. Something like
this:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen-loop-inherit.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">set_inheritable</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">(),</span> <span class="kc">True</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">execvp</span><span class="p">(</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="p">[</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="s2">&quot;loop.sh&quot;</span><span class="p">,</span> <span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;server-accept-inherit.py&quot;</span><span class="p">,</span> <span class="nb">str</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">())])</span>
</pre></div>
</div></div>

Then the file descriptor must also be passed to the server processes and used
there instead of stdin:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-accept-inherit.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">socket</span>
<span class="kn">import</span> <span class="nn">sys</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">(</span><span class="n">fileno</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">]))</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">conn</span><span class="p">,</span> <span class="n">addr</span> <span class="o">=</span> <span class="n">s</span><span class="o">.</span><span class="n">accept</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;accepting connection&quot;</span><span class="p">)</span>
        <span class="k">with</span> <span class="n">conn</span><span class="p">:</span>
            <span class="n">data</span> <span class="o">=</span> <span class="n">conn</span><span class="o">.</span><span class="n">recv</span><span class="p">(</span><span class="mi">100</span><span class="p">)</span>
            <span class="n">number</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">data</span><span class="p">)</span>
            <span class="n">conn</span><span class="o">.</span><span class="n">sendall</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">*</span><span class="si">{</span><span class="n">number</span><span class="si">}</span><span class="s2">=</span><span class="si">{</span><span class="n">number</span><span class="o">*</span><span class="n">number</span><span class="si">}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">encode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">))</span>
</pre></div>
</div></div>

This seems to work as well.

I think I choose the first approach because that is how [Alan did
it](https://github.com/acg/dream-deploys/blob/master/tcplisten).

Not having to pass the file descriptor to the child processes might be
preferable in some situations. I don't know.

### Why is execvp needed?

At the end of `server-listen-loop.py` we call `execvp` to start executing the
loop script in the same process that started listening on the socket:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen-loop.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">socket</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">dup2</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">(),</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">close</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">())</span>
    <span class="n">os</span><span class="o">.</span><span class="n">execvp</span><span class="p">(</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="p">[</span><span class="s2">&quot;bash&quot;</span><span class="p">,</span> <span class="s2">&quot;loop.sh&quot;</span><span class="p">,</span> <span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;server-accept.py&quot;</span><span class="p">])</span>
</pre></div>
</div></div>

Why do we do that?

I did it because that is how [Alan did
it](https://github.com/acg/dream-deploys/blob/master/tcplisten). But now that I
think about it, I think we can just as well inline the loop script in
`server-listen-loop.py`. That, of course, requires the loop script to be
written in Python. Something like this:

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>server-listen-loop-python.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">socket</span>
<span class="kn">import</span> <span class="nn">subprocess</span>

<span class="k">with</span> <span class="n">socket</span><span class="o">.</span><span class="n">socket</span><span class="p">()</span> <span class="k">as</span> <span class="n">s</span><span class="p">:</span>
    <span class="n">s</span><span class="o">.</span><span class="n">setsockopt</span><span class="p">(</span><span class="n">socket</span><span class="o">.</span><span class="n">SOL_SOCKET</span><span class="p">,</span> <span class="n">socket</span><span class="o">.</span><span class="n">SO_REUSEADDR</span><span class="p">,</span> <span class="mi">1</span><span class="p">)</span>
    <span class="n">s</span><span class="o">.</span><span class="n">bind</span><span class="p">((</span><span class="s2">&quot;localhost&quot;</span><span class="p">,</span> <span class="mi">9000</span><span class="p">))</span>
    <span class="n">s</span><span class="o">.</span><span class="n">listen</span><span class="p">()</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;listening on port 9000&quot;</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">dup2</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">(),</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">close</span><span class="p">(</span><span class="n">s</span><span class="o">.</span><span class="n">fileno</span><span class="p">())</span>
    <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
        <span class="n">subprocess</span><span class="o">.</span><span class="n">Popen</span><span class="p">([</span><span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;server-accept.py&quot;</span><span class="p">])</span><span class="o">.</span><span class="n">wait</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;restarting&quot;</span><span class="p">)</span>
</pre></div>
</div></div>

It seems to work as well.

If the loop script is a simple loop like this, perhaps it makes sense to inline
it. But if the loop script is more complex, perhaps even a third party product
to manage server processes, it makes sense to do the `execvp`.

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

https://github.com/acg/dream-deploys/blob/master/loop-forever

### Is asyncio more reliable

Don't kill server if client request failed

### Can this mechanism be used for zero-downtime deployments

Well, yes, that is how I learned about it in Alan's blog post.

### Unix domain socket vs. TCP socket

Unix domain sockets are probably faster than TCP sockets when running on the
same machine.

https://eli.thegreenplace.net/2019/unix-domain-sockets-in-go/
