---
title: 'DRAFT: Output Tracking vs Mocks'
date: 2024-05-01
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this blog post we're going to explore how to write and test a Git client
using the [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
approach. Specifically we're going to focus on [Output
Tracking](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#output-tracking)
and explore how to apply it to this example.

## Example Git client

The example Git client is a CLI-application that provides a simplified
interface to Git. This represents a [real world scenario](https://gut-cli.dev/)
yet can be made small enough for an example.

The application implements two commands:

```
myscm save  -> git commit

myscm share -> git push
```

## Architecture

The application consists of the following classes:

```
App --+--> SaveCommand --+--> Process
      |                  |
      |                  +--> Filesystem
      |
      +--> ShareCommand ----> Process
      |
      +--> Args
      |
      +--> Terminal
```

* `Process`, `Filesystem`, `Args`, and `Terminal` are low-level [infrastructure
  wrappers](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#infrastructure-wrappers)
  that are made
  [nullable](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#nullables)
  using [embedded
  stubs](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#embedded-stub).

    * `Process` is for running external processes. (`git` in this example.)
    * `Filesystem` is for reading file contents from disk.
    * `Args` is for reading command line arguments.
    * `Terminal` is for writing text to the terminal.

* `SaveCommand` and `ShareCommand` are application code that performs a
  function in the domain of a Git client. They are made nullable using [fake it
  once you make
  it](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#fake-it).

* `App` is also application code that routes commands to the correct
  sub-command. It is also made nullable using "fake it once you make it".

## How to test `App`?

We want to write sociable, state-based test.

What does that mean in the context of testing `App`?

Sociable means that we should use its real dependencies. That is, we should
inject a real `SaveCommand`, `ShareCommand`, `Args`, and `Terminal`. We should
not inject test doubles like mocks or stubs.

So the test setup will look something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; app = App(</span>
<span class="sd">...     save_command=SaveCommand(...),</span>
<span class="sd">...     share_command=ShareCommand(...),</span>
<span class="sd">...     terminal=Terminal(...),</span>
<span class="sd">...     args=Args(...),</span>
<span class="sd">... )</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
However, if we were to invoke methods on `app` now, it would interact with the
outside world. It would read command line arguments, execute `git` commands,
and write to the terminal.

We don't want to do that. It takes a long time and is brittle. We therefore
inject null versions of dependencies like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; app = App(</span>
<span class="sd">...     save_command=SaveCommand.create_null(),</span>
<span class="sd">...     share_command=ShareCommand.create_null(),</span>
<span class="sd">...     terminal=Terminal.create_null(),</span>
<span class="sd">...     args=Args.create_null(),</span>
<span class="sd">... )</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
Creating a null version is exactly like creating a real version except that at
the very edge of the application boundary, the communication with the outside
world is turned off. We put this in a factory-method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">App</span><span class="p">:</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">save_command</span><span class="o">=</span><span class="n">SaveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
            <span class="n">share_command</span><span class="o">=</span><span class="n">ShareCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
            <span class="n">terminal</span><span class="o">=</span><span class="n">Terminal</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
            <span class="n">args</span><span class="o">=</span><span class="n">Args</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
        <span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
`App` has only one method, and that is `run`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
    <span class="o">...</span>
</pre></div>
</div></div>
So the only test we can write is this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; app = App.create_null()</span>
<span class="sd">&gt;&gt;&gt; app.run()</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
There is no way to control what the command line arguments are, and there is no
way to observe what the application is doing.

Here are two scenarios that would be useful to test:

* When the application is called with `["save", "message"]`, then `git commit
  -a -m message` is called.

* When the application is called with `["share"]`, then `git push` is called.

In order to write those test, we need a way to control the outside world to
simulate that a given set of command line arguments are present. We also need a
way to observe what commands would be run (if we were not using the
null-version).

We can solve the first part by passing command line arguments to simulate to
`create_null`. The test then becomes this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; app = App.create_null(args=[&quot;save&quot;, &quot;message&quot;])</span>
<span class="sd">&gt;&gt;&gt; app.run()</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
`App.create_null` is modified to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">App</span><span class="p">:</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">save_command</span><span class="o">=</span><span class="n">SaveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
            <span class="n">share_command</span><span class="o">=</span><span class="n">ShareCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
            <span class="n">terminal</span><span class="o">=</span><span class="n">Terminal</span><span class="o">.</span><span class="n">create_null</span><span class="p">(),</span>
            <span class="n">args</span><span class="o">=</span><span class="n">Args</span><span class="o">.</span><span class="n">create_null</span><span class="p">(</span><span class="n">args</span><span class="o">=</span><span class="n">args</span><span class="p">),</span>
        <span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
`Args` supports [configuring
responses](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#configurable-responses)
when creating the null version. In that case it would return the configured
command line arguments instead of the real ones. The communication with the
outside world has been turned off, and we simulate the part of the outside
world that reads command line arguments from the environment.

Now we can write our two scenarios like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; app = App.create_null(args=[&quot;save&quot;, &quot;message&quot;])</span>
<span class="sd">&gt;&gt;&gt; app.run()</span>
<span class="sd"># How to assert that &quot;git commit&quot; was called?</span>

<span class="sd">&gt;&gt;&gt; app = App.create_null(args=[&quot;share&quot;])</span>
<span class="sd">&gt;&gt;&gt; app.run()</span>
<span class="sd"># How to assert that &quot;git push&quot; was called?</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And now we come to the main topic of this blog post: output tracking.

`App` performs action by delegating to `SaveCommand` and `ShareCommand`. Both
of them take the rest of the command line arguments and performs an action
without returning anything. To observe that with output tracking, we introduce
state in the commands so that we can query them and see if they were run. A
slightly more elegant solution, instead of introducing state, is to fire
events. Here is how we implement it in `SaveCommand`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">SaveCommand</span><span class="p">(</span><span class="n">Trackable</span><span class="p">):</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;SAVE_COMMAND </span><span class="si">{</span><span class="n">args</span><span class="si">!r}</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="o">...</span>
</pre></div>
</div></div>
To track events, we can do this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; events = Events()</span>
<span class="sd">&gt;&gt;&gt; SaveCommand.create_null().track_events(events).run([&quot;message&quot;])</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">SAVE_COMMAND [&#39;message&#39;]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We use the event tracking pattern for both commands and the terminal like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">App</span><span class="p">:</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">,</span> <span class="n">events</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">save_command</span><span class="o">=</span><span class="n">SaveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">),</span>
            <span class="n">share_command</span><span class="o">=</span><span class="n">ShareCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">),</span>
            <span class="n">terminal</span><span class="o">=</span><span class="n">Terminal</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">),</span>
            <span class="n">args</span><span class="o">=</span><span class="n">Args</span><span class="o">.</span><span class="n">create_null</span><span class="p">(</span><span class="n">args</span><span class="o">=</span><span class="n">args</span><span class="p">),</span>
        <span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
And now we can write our tests like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; events = Events()</span>
<span class="sd">&gt;&gt;&gt; App.create_null(events, args=[&quot;save&quot;, &quot;message&quot;]).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">SAVE_COMMAND [&#39;message&#39;]</span>

<span class="sd">&gt;&gt;&gt; events = Events()</span>
<span class="sd">&gt;&gt;&gt; App.create_null(events, args=[&quot;share&quot;]).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">SHARE_COMMAND []</span>

<span class="sd">&gt;&gt;&gt; events = Events()</span>
<span class="sd">&gt;&gt;&gt; App.create_null(events, args=[&quot;unknown&quot;, &quot;sub&quot;, &quot;command&quot;]).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">TERMINAL_WRITE &#39;Unknown command.&#39;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The implementation looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
    <span class="n">args</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">args</span><span class="o">.</span><span class="n">get</span><span class="p">()</span>
    <span class="k">if</span> <span class="n">args</span><span class="p">[</span><span class="mi">0</span><span class="p">:</span><span class="mi">1</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;save&quot;</span><span class="p">]:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">save_command</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">args</span><span class="p">[</span><span class="mi">1</span><span class="p">:])</span>
    <span class="k">elif</span> <span class="n">args</span><span class="p">[</span><span class="mi">0</span><span class="p">:</span><span class="mi">1</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;share&quot;</span><span class="p">]:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">share_command</span><span class="o">.</span><span class="n">run</span><span class="p">([])</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">terminal</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="s2">&quot;Unknown command.&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
## Reflections

The tests for `App` are similar to end-to-end-test in that the whole stack is
executed. Except right at the application boundary. So if we supply incorrect
arguments to the save command for example, this test will blow up:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; App.create_null(Events(), args=[&quot;save&quot;]).run()</span>
<span class="sd">Traceback (most recent call last):</span>
<span class="sd">  ...</span>
<span class="sd">ValueError: Expected one argument as the message, but got [].</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This is overlapping, sociable testing. We we actually testing that `App` calls
`SaveCommand` correctly. However, the behavior of the save command is not
tested here. We only test that application parses command line arguments
correctly and calls the appropriate sub-command.

## The Mock version

Let's contrast how the first test case can be written using mocks and stubs
instead. Here it is again:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; events = Events()</span>
<span class="sd">&gt;&gt;&gt; App.create_null(events, args=[&quot;save&quot;, &quot;message&quot;]).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">SAVE_COMMAND [&#39;message&#39;]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And here is the mock/stub version:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; save_command_mock = Mock()</span>
<span class="sd">&gt;&gt;&gt; App(</span>
<span class="sd">...     save_command=save_command_mock,</span>
<span class="sd">...     share_command=None,</span>
<span class="sd">...     terminal=None,</span>
<span class="sd">...     args=Mock(**{&quot;get.return_value&quot;: [&quot;save&quot;, &quot;message&quot;]})</span>
<span class="sd">... ).run()</span>
<span class="sd">&gt;&gt;&gt; save_command_mock.run.call_args_list</span>
<span class="sd">[call([&#39;message&#39;])]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The share command and terminal are not exercised in this test, so we just
inject `None`. For `args` we inject a stub that is configured to return
`["save", "message"]` when its `get` method is called. For the `save_command`,
we inject a mock. After we call the `run` method on the application, we assert
that the `run` method was called on the mock with the `['message']` argument.

Let's contrast the two assertions:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">SAVE_COMMAND [&#39;message&#39;]</span>

<span class="sd">&gt;&gt;&gt; save_command_mock.run.call_args_list</span>
<span class="sd">[call([&#39;message&#39;])]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
They look very similar. Almost to the point that output tracking feels like
mocking.

But there is one crucial difference:

**The mock version creates isolated tests whereas the output tracking version
creates sociable tests.**

We have already seen what happens in the output tracking version when we call
the save command with incorrect arguments. What happens in the mock based
version? It happily passes:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="o">&gt;&gt;&gt;</span> <span class="n">save_command_mock</span> <span class="o">=</span> <span class="n">Mock</span><span class="p">()</span>
<span class="o">&gt;&gt;&gt;</span> <span class="n">App</span><span class="p">(</span>
<span class="o">...</span>     <span class="n">save_command</span><span class="o">=</span><span class="n">save_command_mock</span><span class="p">,</span>
<span class="o">...</span>     <span class="n">share_command</span><span class="o">=</span><span class="kc">None</span><span class="p">,</span>
<span class="o">...</span>     <span class="n">terminal</span><span class="o">=</span><span class="kc">None</span><span class="p">,</span>
<span class="o">...</span>     <span class="n">args</span><span class="o">=</span><span class="n">Mock</span><span class="p">(</span><span class="o">**</span><span class="p">{</span><span class="s2">&quot;get.return_value&quot;</span><span class="p">:</span> <span class="p">[</span><span class="s2">&quot;save&quot;</span><span class="p">]})</span>
<span class="o">...</span> <span class="p">)</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
<span class="o">&gt;&gt;&gt;</span> <span class="n">save_command_mock</span><span class="o">.</span><span class="n">run</span><span class="o">.</span><span class="n">call_args_list</span>
<span class="p">[</span><span class="n">call</span><span class="p">([])]</span>
</pre></div>
</div></div>
To make the mock based test suite "equivalently powerful" we need to augment it
with "contract tests". In this case we need a test saying something like when
the save command is called with no arguments, it does not blow up. And we have
to write such tests for every example in our test suite. When we assert that a
dependency is called in a certain way or returns a certain thing under certain
conditions, we also have to write a contract test that checks that the
dependency can actually except those arguments and return those things under
said conditions. That seems like a whole lot more work to me.

## Recording function calls vs actions

Another more subtle difference is..

## Notes

See also [How to test a router?](/writing/how-to-test-a-router/index.html)

See also [Favor real dependencies for unit
testing](https://stackoverflow.blog/2022/01/03/favor-real-dependencies-for-unit-testing/)

* Don't mock internal dependencies vs output tracking

* p.117

    * Event: the action that is performed.

    * Output tracking (invisible writes). Write to external system.

    * Track writes in terms of behaviors your callers care about.

        * Logger writes to stdout. (Write string.)

        * Callers care about data written. (Track data.)

* p.123

* Functional Core / Imperative Shell. Functional core returns decision that
  imperative shell executes.

## Appendix: myscm.py

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>myscm.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="ch">#!/usr/bin/env python</span>

<span class="kn">from</span> <span class="nn">unittest.mock</span> <span class="kn">import</span> <span class="n">Mock</span>
<span class="kn">import</span> <span class="nn">doctest</span>
<span class="kn">import</span> <span class="nn">subprocess</span>
<span class="kn">import</span> <span class="nn">sys</span>

<span class="k">class</span> <span class="nc">Trackable</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">events</span> <span class="o">=</span> <span class="p">[]</span>

    <span class="k">def</span> <span class="nf">track_events</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">events</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">events</span><span class="p">)</span>
        <span class="k">return</span> <span class="bp">self</span>

    <span class="k">def</span> <span class="nf">notify</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">events</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">events</span><span class="p">:</span>
            <span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Events</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">events</span> <span class="o">=</span> <span class="p">[]</span>

    <span class="k">def</span> <span class="nf">append</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>

    <span class="k">def</span> <span class="fm">__repr__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="s2">&quot;</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">join</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">events</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">App</span><span class="p">:</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; isinstance(App.create(), App)</span>
<span class="sd">        True</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">save_command</span><span class="o">=</span><span class="n">SaveCommand</span><span class="o">.</span><span class="n">create</span><span class="p">(),</span>
            <span class="n">share_command</span><span class="o">=</span><span class="n">ShareCommand</span><span class="o">.</span><span class="n">create</span><span class="p">(),</span>
            <span class="n">terminal</span><span class="o">=</span><span class="n">Terminal</span><span class="o">.</span><span class="n">create</span><span class="p">(),</span>
            <span class="n">args</span><span class="o">=</span><span class="n">Args</span><span class="o">.</span><span class="n">create</span><span class="p">(),</span>
        <span class="p">)</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">,</span> <span class="n">events</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">save_command</span><span class="o">=</span><span class="n">SaveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">),</span>
            <span class="n">share_command</span><span class="o">=</span><span class="n">ShareCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">),</span>
            <span class="n">terminal</span><span class="o">=</span><span class="n">Terminal</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">),</span>
            <span class="n">args</span><span class="o">=</span><span class="n">Args</span><span class="o">.</span><span class="n">create_null</span><span class="p">(</span><span class="n">args</span><span class="o">=</span><span class="n">args</span><span class="p">),</span>
        <span class="p">)</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">save_command</span><span class="p">,</span> <span class="n">share_command</span><span class="p">,</span> <span class="n">terminal</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">save_command</span> <span class="o">=</span> <span class="n">save_command</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">share_command</span>  <span class="o">=</span> <span class="n">share_command</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">terminal</span> <span class="o">=</span> <span class="n">terminal</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">args</span> <span class="o">=</span> <span class="n">args</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        I dispatch to the correct sub-command:</span>

<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; App.create_null(events, args=[&quot;save&quot;, &quot;message&quot;]).run()</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        SAVE_COMMAND [&#39;message&#39;]</span>

<span class="sd">        &gt;&gt;&gt; save_command_mock = Mock()</span>
<span class="sd">        &gt;&gt;&gt; App(</span>
<span class="sd">        ...     save_command=save_command_mock,</span>
<span class="sd">        ...     share_command=None,</span>
<span class="sd">        ...     terminal=None,</span>
<span class="sd">        ...     args=Mock(**{&quot;get.return_value&quot;: [&quot;save&quot;, &quot;message&quot;]})</span>
<span class="sd">        ... ).run()</span>
<span class="sd">        &gt;&gt;&gt; save_command_mock.run.call_args_list</span>
<span class="sd">        [call([&#39;message&#39;])]</span>

<span class="sd">        &gt;&gt;&gt; App.create_null(Events(), args=[&quot;save&quot;]).run()</span>
<span class="sd">        Traceback (most recent call last):</span>
<span class="sd">          ...</span>
<span class="sd">        ValueError: Expected one argument as the message, but got [].</span>

<span class="sd">        &gt;&gt;&gt; save_command_mock = Mock()</span>
<span class="sd">        &gt;&gt;&gt; App(</span>
<span class="sd">        ...     save_command=save_command_mock,</span>
<span class="sd">        ...     share_command=None,</span>
<span class="sd">        ...     terminal=None,</span>
<span class="sd">        ...     args=Mock(**{&quot;get.return_value&quot;: [&quot;save&quot;]})</span>
<span class="sd">        ... ).run()</span>
<span class="sd">        &gt;&gt;&gt; save_command_mock.run.call_args_list</span>
<span class="sd">        [call([])]</span>

<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; App.create_null(events, args=[&quot;share&quot;]).run()</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        SHARE_COMMAND []</span>

<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; App.create_null(events, args=[&quot;unknown&quot;, &quot;sub&quot;, &quot;command&quot;]).run()</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        TERMINAL_WRITE &#39;Unknown command.&#39;</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="n">args</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">args</span><span class="o">.</span><span class="n">get</span><span class="p">()</span>
        <span class="k">if</span> <span class="n">args</span><span class="p">[</span><span class="mi">0</span><span class="p">:</span><span class="mi">1</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;save&quot;</span><span class="p">]:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">save_command</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">args</span><span class="p">[</span><span class="mi">1</span><span class="p">:])</span>
        <span class="k">elif</span> <span class="n">args</span><span class="p">[</span><span class="mi">0</span><span class="p">:</span><span class="mi">1</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;share&quot;</span><span class="p">]:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">share_command</span><span class="o">.</span><span class="n">run</span><span class="p">([])</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">terminal</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="s2">&quot;Unknown command.&quot;</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">SaveCommand</span><span class="p">(</span><span class="n">Trackable</span><span class="p">):</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">process</span><span class="o">=</span><span class="n">Process</span><span class="o">.</span><span class="n">create</span><span class="p">()</span>
        <span class="p">)</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">,</span> <span class="n">events</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">process</span><span class="o">=</span><span class="n">Process</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">)</span>
        <span class="p">)</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">process</span><span class="p">):</span>
        <span class="n">Trackable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">process</span> <span class="o">=</span> <span class="n">process</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; SaveCommand.create_null().track_events(events).run([&quot;message&quot;])</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        SAVE_COMMAND [&#39;message&#39;]</span>

<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; SaveCommand.create_null(events=events).run([&#39;message&#39;])</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        PROCESS_RUN [&#39;git&#39;, &#39;commit&#39;, &#39;-a&#39;, &#39;-m&#39;, &#39;message&#39;]</span>

<span class="sd">        &gt;&gt;&gt; SaveCommand.create_null().run([&#39;message&#39;, &#39;--force&#39;])</span>
<span class="sd">        Traceback (most recent call last):</span>
<span class="sd">          ...</span>
<span class="sd">        ValueError: Expected one argument as the message, but got [&#39;message&#39;, &#39;--force&#39;].</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;SAVE_COMMAND </span><span class="si">{</span><span class="n">args</span><span class="si">!r}</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="k">if</span> <span class="nb">len</span><span class="p">(</span><span class="n">args</span><span class="p">)</span> <span class="o">!=</span> <span class="mi">1</span><span class="p">:</span>
            <span class="k">raise</span> <span class="ne">ValueError</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;Expected one argument as the message, but got </span><span class="si">{</span><span class="n">args</span><span class="si">!r}</span><span class="s2">.&quot;</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">process</span><span class="o">.</span><span class="n">run</span><span class="p">([</span><span class="s2">&quot;git&quot;</span><span class="p">,</span> <span class="s2">&quot;commit&quot;</span><span class="p">,</span> <span class="s2">&quot;-a&quot;</span><span class="p">,</span> <span class="s2">&quot;-m&quot;</span><span class="p">,</span> <span class="n">args</span><span class="p">[</span><span class="mi">0</span><span class="p">]])</span>

<span class="k">class</span> <span class="nc">ShareCommand</span><span class="p">(</span><span class="n">Trackable</span><span class="p">):</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">process</span><span class="o">=</span><span class="n">Process</span><span class="o">.</span><span class="n">create</span><span class="p">()</span>
        <span class="p">)</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">,</span> <span class="n">events</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span>
            <span class="n">process</span><span class="o">=</span><span class="n">Process</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">track_events</span><span class="p">(</span><span class="n">events</span><span class="p">)</span>
        <span class="p">)</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">process</span><span class="p">):</span>
        <span class="n">Trackable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">process</span> <span class="o">=</span> <span class="n">process</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; ShareCommand.create_null(events=events).run([])</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        PROCESS_RUN [&#39;git&#39;, &#39;push&#39;]</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;SHARE_COMMAND </span><span class="si">{</span><span class="n">args</span><span class="si">!r}</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">process</span><span class="o">.</span><span class="n">run</span><span class="p">([</span><span class="s2">&quot;git&quot;</span><span class="p">,</span> <span class="s2">&quot;push&quot;</span><span class="p">])</span>

<span class="k">class</span> <span class="nc">Terminal</span><span class="p">(</span><span class="n">Trackable</span><span class="p">):</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span><span class="n">sys</span><span class="o">=</span><span class="n">sys</span><span class="p">)</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">class</span> <span class="nc">NullStream</span><span class="p">:</span>
            <span class="k">def</span> <span class="nf">write</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">text</span><span class="p">):</span>
                <span class="k">pass</span>
            <span class="k">def</span> <span class="nf">flush</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
                <span class="k">pass</span>
        <span class="k">class</span> <span class="nc">NullSysModule</span><span class="p">:</span>
            <span class="n">stdout</span> <span class="o">=</span> <span class="n">NullStream</span><span class="p">()</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span><span class="n">sys</span><span class="o">=</span><span class="n">NullSysModule</span><span class="p">())</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">sys</span><span class="p">):</span>
        <span class="n">Trackable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">sys</span> <span class="o">=</span> <span class="n">sys</span>

    <span class="k">def</span> <span class="nf">write</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">text</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; Terminal.create().track_events(events).write(&quot;hello&quot;)</span>
<span class="sd">        hello</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        TERMINAL_WRITE &#39;hello&#39;</span>

<span class="sd">        &gt;&gt;&gt; Terminal.create_null().write(&quot;hello&quot;)</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;TERMINAL_WRITE </span><span class="si">{</span><span class="n">text</span><span class="si">!r}</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="nb">print</span><span class="p">(</span><span class="n">text</span><span class="p">,</span> <span class="n">file</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="p">,</span> <span class="n">flush</span><span class="o">=</span><span class="kc">True</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Args</span><span class="p">(</span><span class="n">Trackable</span><span class="p">):</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span><span class="n">sys</span><span class="o">=</span><span class="n">sys</span><span class="p">)</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">,</span> <span class="n">args</span><span class="p">):</span>
        <span class="k">class</span> <span class="nc">NullSysModule</span><span class="p">:</span>
            <span class="n">argv</span> <span class="o">=</span> <span class="p">[</span><span class="s2">&quot;null program&quot;</span><span class="p">]</span><span class="o">+</span><span class="n">args</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span><span class="n">sys</span><span class="o">=</span><span class="n">NullSysModule</span><span class="p">())</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">sys</span><span class="p">):</span>
        <span class="n">Trackable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">sys</span> <span class="o">=</span> <span class="n">sys</span>

    <span class="k">def</span> <span class="nf">get</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; Args.create().get()</span>
<span class="sd">        [&#39;--test&#39;]</span>

<span class="sd">        &gt;&gt;&gt; Args.create_null(args=[&quot;configured&quot;, &quot;args&quot;]).get()</span>
<span class="sd">        [&#39;configured&#39;, &#39;args&#39;]</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:]</span>

<span class="k">class</span> <span class="nc">Process</span><span class="p">(</span><span class="n">Trackable</span><span class="p">):</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span><span class="n">subprocess</span><span class="o">=</span><span class="n">subprocess</span><span class="p">)</span>

    <span class="nd">@classmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="bp">cls</span><span class="p">):</span>
        <span class="k">class</span> <span class="nc">NullSubprocessModule</span><span class="p">:</span>
            <span class="k">def</span> <span class="nf">call</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">command</span><span class="p">):</span>
                <span class="k">pass</span>
        <span class="k">return</span> <span class="bp">cls</span><span class="p">(</span><span class="n">subprocess</span><span class="o">=</span><span class="n">NullSubprocessModule</span><span class="p">())</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">subprocess</span><span class="p">):</span>
        <span class="n">Trackable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">subprocess</span> <span class="o">=</span> <span class="n">subprocess</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">command</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; events = Events()</span>
<span class="sd">        &gt;&gt;&gt; Process.create().track_events(events).run([&quot;echo&quot;, &quot;hello&quot;])</span>
<span class="sd">        &gt;&gt;&gt; events</span>
<span class="sd">        PROCESS_RUN [&#39;echo&#39;, &#39;hello&#39;]</span>

<span class="sd">        &gt;&gt;&gt; Process.create_null().run([&quot;echo&quot;, &quot;hello&quot;])</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;PROCESS_RUN </span><span class="si">{</span><span class="n">command</span><span class="si">!r}</span><span class="s2">&quot;</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">subprocess</span><span class="o">.</span><span class="n">call</span><span class="p">(</span><span class="n">command</span><span class="p">)</span>

<span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="k">if</span> <span class="n">Args</span><span class="o">.</span><span class="n">create</span><span class="p">()</span><span class="o">.</span><span class="n">get</span><span class="p">()</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;--test&quot;</span><span class="p">]:</span>
        <span class="n">doctest</span><span class="o">.</span><span class="n">testmod</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;OK&quot;</span><span class="p">)</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">App</span><span class="o">.</span><span class="n">create</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
