---
title: "TDD trick: fake it!"
date: 2023-04-17
tags: tdd
---

The first step in the TDD loop is to think about what test to write. I find it
easiest to do that from the outside-in. I might not yet know what different
parts my system will consist of (especially in the beginning), but I do know
some behavior of the entire system.

The problem with outside-in is that the test might be difficult to write
because we don't have enough infrastructure in place to make the appropriate
assertions.

In this blog post I want to show you a trick to overcome that problem: fake
it! (This is also similar to what James describes in [Grow Evolutionary
Seeds](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#grow-seeds).)

## The example

I will illustrate this trick with an example from a
[game](/projects/agdpp/index.html) that I'm working on.

So far, the game doesn't do much. It just animates a circle:

<center>
![Animated circle.](animation.png)
</center>

The behavior of the game can be described as this:

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
Really? How is this useful?

First of all we have got a description of one behavior of our system. We also
have code that verifies that behavior, even though it is not yet complete. (The
example shown in the docstring of the game class is actually an executable test
written using Python's
[doctest](https://docs.python.org/3/library/doctest.html) module.)

From here it is usually easier to see what to continue with. When we have
[something on the
screen](https://www.artima.com/articles/the-simplest-thing-that-could-possibly-work),
we can more easily criticize it and improve it.

So, what next?

## Refactor

Are you kidding? Refactor already?

To make this game testable, I want to separate the logic of the game from the
infrastructure needed to initialize and use a graphics library.  I imagine a
game loop class that is responsible for that.

We can evolve the design in that direction even with what we have now. Here it
is:

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
The game now gets a loop as a dependency. Can you see how this would be
possible to test now? We could inject some kind of test double as the loop and
verify that it is called correctly.

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

Let's replace the print statements, that we used to fake actions, with events.
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
(I won't show the code for `Observable`. If you are curious to know the
details, you can look
[here](https://github.com/rickardlindberg/agdpp/blob/initial-game-loop/events.py).)

And this is starting to look more real now. There is no real faking going on in
the game or its test any more. It seems logical to assert that the game loop
emits those events.

But the game loop is still just emitting events, it's not actually doing
anything.

## Make game loop more real

The game loop should initialize a graphics library and provide to the game a
way to draw on the screen. We will use [pygame](https://www.pygame.org/news)
for that. So our game loop will be an infrastructure wrapper around pygame
providing a clean interface for our game to draw on the screen.

We need to make the `run` method and the `draw_circle` method do something
real. Here is a first version:

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
Notice that have an instance variable called `pygame`.

When we test our game class, we don't actually want to execute any pygame code
that creates windows and draws circles on the screen. Therefore we use another
pattern of an infrastructure wrapper which is that it can be nullable. That
means that we can instantiate two version of our game loop:

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

Our game test can use the null version of the game loop and will continue to
work as it did before.

When actually running our game, we create the real version of the game loop
which will include all the real calls to pygame.

We can also test the real game loop in isolation, passing it a test game, to
make sure that we call pygame correctly:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; GameLoop.create().run(TestGame())</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This will actually open a window and draw whatever the test game draws. We can
program the test game to exit immediately so that the test suite will not hang
waiting for user input. But we will still see a flashing window which is a bit
distracting.

To verify that our implementation of `draw_circle` works, we have to inspect
the output visually. A test like the one above only asserts that we call pygame
functions correctly, not that the output looks the way we want.

This is a general problem with infrastructure that it is difficult to test,
because it involves the real world.

Anyway, that's a little beside the point of this article. Where were we?

## Looping and animation

If we run our game now, it will actually show a window with a circle on it.
But the window will close immediately.

That is because the game loop still doesn't implement a loop.

Let's have a look at our game test again:

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
It talks about *animating* a circle, and about *waiting* for the user to close
the window. But there is nothing in the setup or assertions about this. We are
missing something.

Hopefully, at this point, it is a bit more clear where to continue.

Once we implement the loop and some event handling, I think the initial
behavior of our game will actually be fully realized.

Here is what the final test for the game looks like when I continued fleshing
out all the fakes and missing pieces:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an animated circle until the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [pygame.event.Event(pygame.QUIT)],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; Game(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">PYGAME_INIT =&gt;</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 50</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 51</span>
<span class="sd">PYGAME_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
## Summary

We started with something fake, then did a bit of design, then removed one fake
at a time until there were no fakes left.

I find this a useful way of working, especially when getting started. Once you
have some structure in place it is easier to see where you need to add
additional tests and functionality.

(If you want more details about this example in particular, check out my
[article](/writing/agdpp-game-loop/index.html) about how I implemented this
part of the game.)
