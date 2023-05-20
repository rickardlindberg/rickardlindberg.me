---
title: How to test a router?
date: 2023-05-20
---

I've been practicing [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
for a while now. It describes a way of doing overlapping, sociable testing,
which include infrastructure, without having side effects occur in your
tests.

Recently I've been wondering how to test a "router" using this pattern. By
router I mean an entry level function that looks at a url or command line
arguments or whatever and dispatches to the relevant "controller". Something
like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">MyWebApp</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">main</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">url</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">url</span><span class="o">.</span><span class="n">startswith</span><span class="p">(</span><span class="s2">&quot;/home&quot;</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">home_controller</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="o">...</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">url</span><span class="o">.</span><span class="n">startswith</span><span class="p">(</span><span class="s2">&quot;/about&quot;</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">about_controller</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="o">...</span><span class="p">)</span>
</pre></div>
</div></div>
I [asked](https://hachyderm.io/@rickardlindberg/110379826738876668) James if he
had any examples of this, and he
[had](https://github.com/jamesshore/testing-without-mocks-complex/tree/javascript/src/www).
Let's explore.

## James' example

Overly simplified, to only highlight the parts that I'm interested in, James'
example looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">WwwRouter</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">home_page_controller</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">home_page_controller</span> <span class="o">=</span> <span class="n">home_page_controller</span>

    <span class="k">def</span> <span class="nf">route</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">url</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">url</span> <span class="o">==</span> <span class="s2">&quot;/&quot;</span><span class="p">:</span>
            <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">home_page_controller</span><span class="o">.</span><span class="n">get</span><span class="p">()</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="k">return</span> <span class="s2">&quot;FAIL&quot;</span>

<span class="k">class</span> <span class="nc">HomePageController</span><span class="p">:</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">HomePageController</span><span class="p">(</span><span class="n">SomeInfrastructure</span><span class="o">.</span><span class="n">create</span><span class="p">())</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">HomePageController</span><span class="p">(</span><span class="n">SomeInfrastructure</span><span class="o">.</span><span class="n">create_null</span><span class="p">())</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">some_infrastructure</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">some_infrastructure</span> <span class="o">=</span> <span class="n">some_infrastructure</span>

    <span class="k">def</span> <span class="nf">get</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">return</span> <span class="s2">&quot;Home Page&quot;</span>
</pre></div>
</div></div>
The question I had was, how to test the `WwwRouter`? James does it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">test_routes_home_page</span><span class="p">():</span>
    <span class="n">router</span> <span class="o">=</span> <span class="n">WwwRouter</span><span class="p">(</span><span class="n">HomePageController</span><span class="o">.</span><span class="n">create_null</span><span class="p">())</span>
    <span class="n">response</span> <span class="o">=</span> <span class="n">router</span><span class="o">.</span><span class="n">route</span><span class="p">(</span><span class="s2">&quot;/&quot;</span><span class="p">)</span>
    <span class="k">assert</span> <span class="n">response</span> <span class="o">==</span> <span class="n">HomePageController</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span><span class="o">.</span><span class="n">get</span><span class="p">()</span>

<span class="k">def</span> <span class="nf">test_routes_errors</span><span class="p">():</span>
    <span class="n">router</span> <span class="o">=</span> <span class="n">WwwRouter</span><span class="p">(</span><span class="n">HomePageController</span><span class="o">.</span><span class="n">create_null</span><span class="p">())</span>
    <span class="n">response</span> <span class="o">=</span> <span class="n">router</span><span class="o">.</span><span class="n">route</span><span class="p">(</span><span class="s2">&quot;/no-such-url&quot;</span><span class="p">)</span>
    <span class="k">assert</span> <span class="n">response</span> <span class="o">==</span> <span class="s2">&quot;FAIL&quot;</span>
</pre></div>
</div></div>
Some characteristics of this test setup:

* The router takes all the controllers as dependencies.

* The null version of the controllers are used.

* The test uses [collaborator-based
  isolation](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#isolation).
  (The test doesn't care what `HomePageController` returns as long as it is the
  same as the router returns.)

## What if return value is missing?

How about a router or dispatcher where the controllers don't return anything.
How to we test that?

Example:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">CliDispatcher</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">add_command</span><span class="p">,</span> <span class="n">remove_command</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">add_command</span> <span class="o">=</span> <span class="n">add_command</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">remove_command</span> <span class="o">=</span> <span class="n">remove_command</span>

    <span class="k">def</span> <span class="nf">dispatch</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">arguments</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">arguments</span><span class="p">[:</span><span class="mi">1</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;add&quot;</span><span class="p">]:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">add_command</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">arguments</span><span class="p">[</span><span class="mi">1</span><span class="p">:])</span>
        <span class="k">elif</span> <span class="n">arguments</span><span class="p">[:</span><span class="mi">1</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;remove&quot;</span><span class="p">]:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">remove_command</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">arguments</span><span class="p">[</span><span class="mi">1</span><span class="p">:])</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">exit</span><span class="p">(</span><span class="s2">&quot;Unknown command.&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
We start out the same:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">test_dispatches_to_add</span><span class="p">():</span>
    <span class="n">add_command</span> <span class="o">=</span> <span class="n">AddCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">remove_command</span> <span class="o">=</span> <span class="n">RemoveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">cli</span> <span class="o">=</span> <span class="n">CliDispatcher</span><span class="p">(</span><span class="n">add_command</span><span class="p">,</span> <span class="n">remove_command</span><span class="p">)</span>
    <span class="n">cli</span><span class="o">.</span><span class="n">dispatch</span><span class="p">([</span><span class="s2">&quot;add&quot;</span><span class="p">,</span> <span class="s2">&quot;item name&quot;</span><span class="p">])</span>
</pre></div>
</div></div>
But `dispatch` does not return anything, so we can't check any return value.
What to do?

We want to test that the function of `AddCommand` was performed, and nothing
else. We could introduce queries on commands to see if they have been run. Then
we can write the asserts like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">assert</span> <span class="n">add_command</span><span class="o">.</span><span class="n">get_last_arguments</span><span class="p">()</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;item name&quot;</span><span class="p">]</span>
<span class="k">assert</span> <span class="n">remove_command</span><span class="o">.</span><span class="n">get_last_arguments</span><span class="p">()</span> <span class="o">==</span> <span class="kc">None</span>
</pre></div>
</div></div>
So we want to assert that the add command was run with the given arguments and
that all the other (only one in the example) commands were not run.

I think this is in the spirit of testing without mocks. Here is what it says
about state-based testing:

> Use state-based tests instead of interaction-based tests. A state-based test
> checks the output or state of the code under test, without any awareness of
> its implementation.

An it goes on to say

> For mutable objects, provide a way for changes in state to be observed,
> either with a getter method or an event.

## Events instead

I am not a fan of the `get_last_*` pattern. Our code could call
`command.run(...)` twice, and the test would not catch the error. I prefer an
events approach instead.

Assuming that commands are observable and that they emit events when run (we
can test that separately), the test can be written like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">test_dispatches_to_add</span><span class="p">():</span>
    <span class="n">events</span> <span class="o">=</span> <span class="p">[]</span>
    <span class="n">add_command</span> <span class="o">=</span> <span class="n">AddCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">add_command</span><span class="o">.</span><span class="n">on_event</span><span class="p">(</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">)</span>
    <span class="n">remove_command</span> <span class="o">=</span> <span class="n">RemoveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">remove_command</span><span class="o">.</span><span class="n">on_event</span><span class="p">(</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">)</span>
    <span class="n">cli</span> <span class="o">=</span> <span class="n">CliDispatcher</span><span class="p">(</span><span class="n">add_command</span><span class="p">,</span> <span class="n">remove_command</span><span class="p">)</span>
    <span class="n">cli</span><span class="o">.</span><span class="n">dispatch</span><span class="p">([</span><span class="s2">&quot;add&quot;</span><span class="p">,</span> <span class="s2">&quot;item name&quot;</span><span class="p">])</span>
    <span class="k">assert</span> <span class="n">events</span> <span class="o">==</span> <span class="p">[</span>
        <span class="p">{</span><span class="s2">&quot;name&quot;</span><span class="p">:</span> <span class="s2">&quot;AddCommand&quot;</span><span class="p">,</span> <span class="s2">&quot;arguments&quot;</span><span class="p">:</span> <span class="p">[</span><span class="s2">&quot;item name&quot;</span><span class="p">]},</span>
    <span class="p">]</span>
</pre></div>
</div></div>
This test ensures that exactly one command was run and that it was run
only once. Exactly what we wanted to test, but now expressed with a single
assert.

## Better test setup

This test setup becomes tedious to do for every command. We can extract it to a
factory method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">create_cli</span><span class="p">():</span>
    <span class="n">events</span> <span class="o">=</span> <span class="p">[]</span>
    <span class="n">add_command</span> <span class="o">=</span> <span class="n">AddCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">add_command</span><span class="o">.</span><span class="n">on_event</span><span class="p">(</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">)</span>
    <span class="n">remove_command</span> <span class="o">=</span> <span class="n">RemoveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">remove_command</span><span class="o">.</span><span class="n">on_event</span><span class="p">(</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">)</span>
    <span class="n">cli</span> <span class="o">=</span> <span class="n">CliDispatcher</span><span class="p">(</span><span class="n">add_command</span><span class="p">,</span> <span class="n">remove_command</span><span class="p">)</span>
    <span class="k">return</span> <span class="n">cli</span><span class="p">,</span> <span class="n">events</span>

<span class="k">def</span> <span class="nf">test_dispatches_to_add</span><span class="p">():</span>
    <span class="n">cli</span><span class="p">,</span> <span class="n">events</span> <span class="o">=</span> <span class="n">create_cli</span><span class="p">()</span>
    <span class="n">cli</span><span class="o">.</span><span class="n">dispatch</span><span class="p">([</span><span class="s2">&quot;add&quot;</span><span class="p">,</span> <span class="s2">&quot;item name&quot;</span><span class="p">])</span>
    <span class="k">assert</span> <span class="n">events</span> <span class="o">==</span> <span class="p">[</span>
        <span class="p">{</span><span class="s2">&quot;name&quot;</span><span class="p">:</span> <span class="s2">&quot;AddCommand&quot;</span><span class="p">,</span> <span class="s2">&quot;arguments&quot;</span><span class="p">:</span> <span class="p">[</span><span class="s2">&quot;item name&quot;</span><span class="p">]},</span>
    <span class="p">]</span>

<span class="k">def</span> <span class="nf">test_dispatches_to_remove</span><span class="p">():</span>
    <span class="n">cli</span><span class="p">,</span> <span class="n">events</span> <span class="o">=</span> <span class="n">create_cli</span><span class="p">()</span>
    <span class="n">cli</span><span class="o">.</span><span class="n">dispatch</span><span class="p">([</span><span class="s2">&quot;remove&quot;</span><span class="p">,</span> <span class="s2">&quot;item name&quot;</span><span class="p">])</span>
    <span class="k">assert</span> <span class="n">events</span> <span class="o">==</span> <span class="p">[</span>
        <span class="p">{</span><span class="s2">&quot;name&quot;</span><span class="p">:</span> <span class="s2">&quot;RemoveCommand&quot;</span><span class="p">,</span> <span class="s2">&quot;arguments&quot;</span><span class="p">:</span> <span class="p">[</span><span class="s2">&quot;item name&quot;</span><span class="p">]},</span>
    <span class="p">]</span>
</pre></div>
</div></div>
Or even better:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run_in_test_mode</span><span class="p">(</span><span class="n">arguments</span><span class="p">):</span>
    <span class="n">events</span> <span class="o">=</span> <span class="p">[]</span>
    <span class="n">add_command</span> <span class="o">=</span> <span class="n">AddCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">add_command</span><span class="o">.</span><span class="n">on_event</span><span class="p">(</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">)</span>
    <span class="n">remove_command</span> <span class="o">=</span> <span class="n">RemoveCommand</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
    <span class="n">remove_command</span><span class="o">.</span><span class="n">on_event</span><span class="p">(</span><span class="n">events</span><span class="o">.</span><span class="n">append</span><span class="p">)</span>
    <span class="n">cli</span> <span class="o">=</span> <span class="n">CliDispatcher</span><span class="p">(</span><span class="n">add_command</span><span class="p">,</span> <span class="n">remove_command</span><span class="p">)</span>
    <span class="n">cli</span><span class="o">.</span><span class="n">dispatch</span><span class="p">(</span><span class="n">arguments</span><span class="p">)</span>
    <span class="k">return</span> <span class="n">events</span>

<span class="k">def</span> <span class="nf">test_dispatches_to_add</span><span class="p">():</span>
    <span class="k">assert</span> <span class="n">run_in_test_mode</span><span class="p">([</span><span class="s2">&quot;add&quot;</span><span class="p">,</span> <span class="s2">&quot;item name&quot;</span><span class="p">])</span> <span class="o">==</span> <span class="p">[</span>
        <span class="p">{</span><span class="s2">&quot;name&quot;</span><span class="p">:</span> <span class="s2">&quot;AddCommand&quot;</span><span class="p">,</span> <span class="s2">&quot;arguments&quot;</span><span class="p">:</span> <span class="p">[</span><span class="s2">&quot;item name&quot;</span><span class="p">]},</span>
    <span class="p">]</span>

<span class="k">def</span> <span class="nf">test_dispatches_to_remove</span><span class="p">():</span>
    <span class="k">assert</span> <span class="n">run_in_test_mode</span><span class="p">([</span><span class="s2">&quot;remove&quot;</span><span class="p">,</span> <span class="s2">&quot;item name&quot;</span><span class="p">])</span> <span class="o">==</span> <span class="p">[</span>
        <span class="p">{</span><span class="s2">&quot;name&quot;</span><span class="p">:</span> <span class="s2">&quot;RemoveCommand&quot;</span><span class="p">,</span> <span class="s2">&quot;arguments&quot;</span><span class="p">:</span> <span class="p">[</span><span class="s2">&quot;item name&quot;</span><span class="p">]},</span>
    <span class="p">]</span>
</pre></div>
</div></div>
## Summary

In hindsight, this seems quite obvious to me. I'm not sure what I had a hard
time understanding. But the example from James helped. Thanks! And writing this
blog post helped me clarify my thinking on the subject.
