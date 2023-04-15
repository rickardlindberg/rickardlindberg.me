---
title: 'DRAFT: "TDD trick: fake it!"'
date: 2023-04-15
tags: tdd,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

The first step in the TDD loop is to think about what test to write. I find it
easiest to do that from the outside-in. I might not yet know what different
parts my system will consist of (especially in the beginning), but I do know
some behavior of the entire system.

The problem with outside-in is that the test might be difficult to write
because you don't have enough infrastructure in place to make the appropriate
assertions.

In this blog post I want to show you a trick to overcome that problem: fake
it! (This is also similar to what James describes in [Grow Evolutionary
Seeds](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#grow-seeds).)

## The example

The example I will be using comes from a [game](/projects/agdpp/index.html)
that I'm working on.

So far, it doesn't do much. It just animates a circle:

<center>
![Animated circle.](animation.png)
</center>

The behavior of this system can be described as this:

> I draw an animated circle until the user closes the window.

## Fake it!

With an empty project, it seems quite difficult to write a test that actually
checks for that. What to do? Let's fake it! Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I draw an animated circle until the user closes the window.</span>

<span class="sd">    &gt;&gt;&gt; game = Game()</span>
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

First of all we have got a description of one behavior of our system. We also
have code that verifies that behavior, even though it is not yet complete.

From here it is usually easier to see what to continue with.

Get [something on the
screen](https://www.artima.com/articles/the-simplest-thing-that-could-possibly-work),
and we can more easily criticize it and improve it.

## Refactor

Are you kidding? Refactor already?

To make this game testable, I want to separate the logic of the game from the
infrastructure needed to initialize and use a graphics library.

I imagine a game loop class that is responsible for that.

We can evolve the design in that direction even with what we have now.

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
be possible to test now. We could inject some kind of test double as the loop
and verify that it is called correctly.

Notice that we were able to do this refactoring with the safety net of the
test. We have one teeny, tiny test that asserts something fake, but it still
helped us do this refactoring.

## Infrastructure wrapper

For the test double version of the game loop, I want to use some patterns from
[Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks).

I want to turn the game loop into an infrastructure wrapper. One part of that
pattern is that it should emit events of what it's doing so that tests can
observe it.

Let's replace the print statements, that we used to fake this, with events.
Here it is:

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
(I won't show the code for `Observable`. If you are curious, to know the
details, you can read
[here](https://github.com/rickardlindberg/agdpp/blob/initial-game-loop/events.py).)

And this is starting to look more real now. There is no real faking going on in
the game and its test any more. It seems logical to assert that the game loop
emits those events.

But the game loop is still just emitting events, it's not actually doing
anything.

## Make game loop more real

Another pattern of an infrastructure wrapper is that it should be nullable.
That means that we can instantiate two version of our game loop:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">GameLoop</span><span class="o">.</span><span class="n">create</span><span class="p">()</span>
<span class="n">GameLoop</span><span class="o">.</span><span class="n">create_null</span><span class="p">()</span>
</pre></div>
</div></div>
The creation methods look like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">GameLoop</span><span class="p">(</span><span class="n">pygame</span><span class="p">)</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">():</span>
        <span class="k">class</span> <span class="nc">NullPygame</span><span class="p">:</span>
            <span class="k">def</span> <span class="nf">init</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
                <span class="k">pass</span> <span class="c1"># Do nothing</span>
            <span class="o">...</span>
        <span class="k">return</span> <span class="n">GameLoop</span><span class="p">(</span><span class="n">NullPygame</span><span class="p">())</span>

    <span class="o">...</span>
</pre></div>
</div></div>
The null version works exactly the same as the real version except it nulls out
all the calls that actually do anything with pygame. This is useful in tests so
that we don't open windows and actually draw graphics when we don't need to.

The first step of making the game loop real is to flesh out some calls to
pygame. Here they are:

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
Our game test can use the null version of the game loop and will continue to
work as they did before.

When actually running our game, we create the real version of the game loop
which will also include all the real calls to pygame.

We can also test the real game loop in isolation, passing it a test game, to
make sure that we call pygame correctly.

If we run this now, it will actually open a window and draw a circle.

## Summary

The game loop still doesn't implement a loop. Hopefully you can see how to
continue to flesh that part out.

Once we have finished removing all the fakes from the game loop we are done.

The game test makes sense and additional tests for the game loop also make
sense and pass.

So there it is.

Start with something fake. Do a bit of design. Remove one fake at a time until
you are done. I find that a good workflow when getting started.

(If you want more details about this example in particular, check out my
[article](/writing/agdpp-game-loop/index.html) about how I implemented this
part of the game.)
