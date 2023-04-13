---
title: 'DRAFT: "TDD trick: fake it"'
date: 2023-04-13
tags: tdd,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

The first step in the TDD loop is to think about what test you want to write. I
find it easiest to do that from the outside-in. I might not yet know what
different parts my system will consist of (especially in the beginning), but I
do know some behavior of the system.

The problem with outside-in is that the test might be difficult to write
because you don't have enough infrastructure in place to make the appropriate
assertions.

In this blog post I want to show you a trick to overcome that problem: faking
it. (This is also similar to what James describes in [Grow Evolutionary
Seeds](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#grow-seeds).)

## The example

Animated game...

## First behavior

> I draw an animated circle until the user closes the window.

## Fake it

With an empty project, that test seems quite difficult to get in place. So
let's fake it. Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I draw an animated circle until the user closes the window.</span>

<span class="sd">    &gt;&gt;&gt; game = Game(GameLoop())</span>
<span class="sd">    &gt;&gt;&gt; game.run()</span>
<span class="sd">    DRAW_CIRCLE</span>
<span class="sd">    EXIT</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">)</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
Really? How is that useful?

First of all we have got a description of one behavior of the system. We also
have code that verifies that behavior, even though it is not yet complete.

From here it is usually easier to see what to continue with.

## Refactor

Are you kidding? Refactor already?

To make this game testable, I want to separate the logic of the game from the
infrastructure needed to initialize and use a graphics library.

I imagine a game loop class that is responsible for that and also for calling
our game in every loop.

I can evolve the design in that direction even with what we have now.

Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I draw an animated circle until the user closes the window.</span>

<span class="sd">    &gt;&gt;&gt; game = Game(GameLoop())</span>
<span class="sd">    &gt;&gt;&gt; game.run()</span>
<span class="sd">    DRAW_CIRCLE</span>
<span class="sd">    EXIT</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span> <span class="o">=</span> <span class="n">loop</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">()</span>

<span class="k">class</span> <span class="nc">GameLoop</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
The game now gets a loop as a dependency, and maybe you can see how this would
be possible to test now.

Notice that we could do this refactoring with the safety net of the test. We
have one teeny tiny test that asserts something fake, but it still helped us do
this refactoring safely.

## Infrastructure wrapper

I want to use some patterns from [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks).
To test this.

I want to turn the game loop into an infrastructure wrapper. One part of that
pattern is that it should emit events of what it's doing.

So next I want to replace the print statements, that we used to fake this, with
events. Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">,</span> <span class="p">{})</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{})</span>
</pre></div>
</div></div>
The test for the game then changes to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an animated circle until the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop()</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; Game(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">EXIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And this is starting to look more real now.

But the game loop is still just emitting events, it's not actually doing
anything.

## Make game loop more real

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;PYGAME_INIT&quot;</span><span class="p">,</span> <span class="p">{})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">init</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">screen</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">set_mode</span><span class="p">((</span><span class="mi">1280</span><span class="p">,</span> <span class="mi">720</span><span class="p">))</span>
        <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">flip</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">,</span> <span class="p">{})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span> <span class="s2">&quot;red&quot;</span><span class="p">,</span> <span class="p">(</span><span class="mi">50</span><span class="p">,</span> <span class="mi">50</span><span class="p">),</span> <span class="mi">40</span><span class="p">)</span>
</pre></div>
</div></div>