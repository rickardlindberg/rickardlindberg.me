---
title: 'DRAFT: Thinking about test setup'
date: 2023-04-23
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In the [previous](/writing/agdpp-shooting-arrow/index.html) episode we were not
quite happy with our test suite. Are we testing things at the right level? Do
we see any smells? Are we missing tests? We will take some time in this episode
to reflect on those questions so that things will go smooth, testing wise, when
working on the next story.

## A concrete problem

In the last episode I had a feeling that everything was not alright with the
tests. I poked around a bit and noticed that I could delete some vital
production code that would break the game without my tests noticing.

The event handling in the balloon shooter now looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
            <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
        <span class="o">...</span>
</pre></div>
</div></div>
If we skip the check for the space key and always shoot the arrow, like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
        <span class="o">...</span>
</pre></div>
</div></div>
Then the arrow goes off immediately when the game starts, which is obviously
not correct, but all tests still pass.

## Coverage and expectation

The promise of TDD and automated testing is that you can have very high test
coverage. What I aspire to is to create a test suite that within seconds tells
me if I broke the production code with my change. It might not be realistic,
but when I find a case where I could break my production code without my tests
noticing, I want to look more closely.

Testing can for sure be seen as a tradeoff. At some point it probably costs
more to test that what you gain.

One reason to not test is that you don't know how. That can be solved by
practicing. Another reason might be that you decide that the cost is not worth
it. However, the cost of testing goes down the better you get at it.

And in this series I'm trying to learn as well and document that process. So
let's analyze the problem with the gap in the test suite and see what we can
come up with.

## What test is missing?

The balloon shooter has two tests. Here are their descriptions:

1. I draw the initial scene of the game which consists of a balloon and an
   arrow and quit when the user closes the window.

2. The arrow moves when it is shot by pressing the space key:

The first test is checking that the initial frame is drawn correctly.

The second test is checking that the arrow moves when we press space.

But there is no test checking that the arrow stays still if we don't press
space.

I think we tried to remedy that in the previous episode by adding these two
"lower-level tests" to the arrow:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I stay still if I&#39;ve not been fired:</span>

<span class="sd">    &gt;&gt;&gt; arrow = Arrow()</span>
<span class="sd">    &gt;&gt;&gt; initial_y = arrow.y</span>
<span class="sd">    &gt;&gt;&gt; arrow.tick(1)</span>
<span class="sd">    &gt;&gt;&gt; arrow.tick(1)</span>
<span class="sd">    &gt;&gt;&gt; arrow.tick(1)</span>
<span class="sd">    &gt;&gt;&gt; initial_y == arrow.y</span>
<span class="sd">    True</span>

<span class="sd">    I move upwards when fired:</span>

<span class="sd">    &gt;&gt;&gt; arrow = Arrow()</span>
<span class="sd">    &gt;&gt;&gt; initial_y = arrow.y</span>
<span class="sd">    &gt;&gt;&gt; arrow.shoot()</span>
<span class="sd">    &gt;&gt;&gt; arrow.tick(1)</span>
<span class="sd">    &gt;&gt;&gt; arrow.tick(1)</span>
<span class="sd">    &gt;&gt;&gt; arrow.tick(1)</span>
<span class="sd">    &gt;&gt;&gt; arrow.y &lt; initial_y</span>
<span class="sd">    True</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="o">...</span>
</pre></div>
</div></div>
The problem is that the event checking logic is not in the `Arrow` class, but
in the `BalloonShooter` class. So what we want to test can't be tested at this
level.

Perhaps there is a story missing. Something that talks about the initial state
of the game. Which should be something like the balloon falls down and the
arrow stays still.

## Where to test?

I'm a fan of testing as much as possible. What I want is for my test suite to
tell me within a couple of seconds if I made a mistake. I want that to be true
for *any* change. That means I need high test coverage.

But writing all tests as "top-level tests" which includes all objects is not a
good idea. The test setup will get complicated. The asserts will get difficult
to write. The test will be slower.

So we want to test at as a low level as possible.

So if we want to test the initial state of the arrow in the `Arrow` class, we
need to move event handling logic into it, so that it's its responsibility and
does not need to be tested in `BalloonShooter`.

On the other hand, when you start writing tests for smaller subsystems, those
subsystems become harder to refactor. Suppose you are not happy with the
subsystems that you have created, and you want a different design. If you do
that refactoring, you also have to modify the tests to fit. That makes
refactoring and design harder. So you don't want to do it too early.

In our case, I think we did it too early. Our game class only has one test, and
we already started extracting subsystems and writing tests there.

Let's see if we can fix that.

## Testing initial state

Before we had this test for the initial state:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw the initial scene of the game which consists of a balloon and an</span>
<span class="sd">arrow and quit when the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; BalloonShooter.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 50</span>
<span class="sd">    y: 50</span>
<span class="sd">    radius: 40</span>
<span class="sd">    color: &#39;red&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 500</span>
<span class="sd">    radius: 10</span>
<span class="sd">    color: &#39;blue&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 520</span>
<span class="sd">    radius: 15</span>
<span class="sd">    color: &#39;blue&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 540</span>
<span class="sd">    radius: 20</span>
<span class="sd">    color: &#39;blue&#39;</span>
<span class="sd">GAMELOOP_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This only captures the initial frame. Let's see if we can rewrite this.

We start with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I am a balloon shooter game!</span>

<span class="sd">Initial state</span>
<span class="sd">=============</span>

<span class="sd">We run the game for a few frames, then quit:</span>

<span class="sd">&gt;&gt;&gt; events = BalloonShooter.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This is just the setup. We simulate that we start the game, run it for a couple
of frames, then quit.

What are some behaviors that we expect to see here?

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">The game loop is initialized and cleaned up:</span>

<span class="sd">&gt;&gt;&gt; events.filter(&quot;GAMELOOP_INIT&quot;, &quot;GAMELOOP_QUIT&quot;)</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">GAMELOOP_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">The balloon is drawn animated:</span>

<span class="sd">&gt;&gt;&gt; events.filter(&quot;DRAW_CIRCLE&quot;, radius=40).collect(&quot;x&quot;, &quot;y&quot;)</span>
<span class="sd">[(50, 50), (51, 50), (52, 50)]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">The arrow is drawn in a fixed position:</span>

<span class="sd">&gt;&gt;&gt; set(events.filter(&quot;DRAW_CIRCLE&quot;, radius=10).collect(&quot;x&quot;, &quot;y&quot;))</span>
<span class="sd">{(500, 500)}</span>
<span class="sd">&gt;&gt;&gt; set(events.filter(&quot;DRAW_CIRCLE&quot;, radius=15).collect(&quot;x&quot;, &quot;y&quot;))</span>
<span class="sd">{(500, 520)}</span>
<span class="sd">&gt;&gt;&gt; set(events.filter(&quot;DRAW_CIRCLE&quot;, radius=20).collect(&quot;x&quot;, &quot;y&quot;))</span>
<span class="sd">{(500, 540)}</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The new cases cover all the old ones and more. Let's delete the old one. It now
also makes the bug catch!

These cases now cover all the cases from the initial test, and 

commit: 51365a3e39b75462e58171cf564dd47874bab393

Let's also remove the old tests.

commit: 233e91b054d85cf25d95f0e7fcd18250660162c1

## Reflecting on new test setup

The test for the initial state is structured by simulating a run of the game,
collecting events of what happened, and then making specific assertions about
those events to check different behavior.

Let's structure the rests of the test that way too:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">User presses space key</span>
<span class="sd">======================</span>

<span class="sd">We run the game for a few frames, press the space key, let it run for a few</span>
<span class="sd">frames, then quit:</span>

<span class="sd">&gt;&gt;&gt; events = BalloonShooter.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_keydown_space()],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>

<span class="sd">The arrow moves:</span>

<span class="sd">&gt;&gt;&gt; arrow_head_positions = events.filter(&quot;DRAW_CIRCLE&quot;, radius=10).collect(&quot;x&quot;, &quot;y&quot;)</span>
<span class="sd">&gt;&gt;&gt; len(arrow_head_positions) &gt; 1</span>
<span class="sd">True</span>
<span class="sd">&gt;&gt;&gt; len(set(arrow_head_positions)) &gt; 1</span>
<span class="sd">True</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
If there are more things that should happen when we press the space key, we can
add asserts for it. But for now, I don't think there is any.

## TODO

* What tests are left to write after ATDD?

## Summary

...

See you in the next episode!
