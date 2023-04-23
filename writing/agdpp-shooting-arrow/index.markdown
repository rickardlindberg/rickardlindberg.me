---
title: 'DRAFT: Shooting arrow'
date: 2023-04-23
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we continue towards the first version of the balloon shooter.
It's time to shoot the arrow!

## Video version

The video version of this episode:

<center>
...
</center>

## Reacp

We are trying to create an absolute minimum version of a balloon shooter game
that we can show to our customer and ask if that was what he had in mind. Our
idea for minimal is this:

* 1 balloon falling down the screen
* 1 arrow pointing in a fixed direction
* 1 button to shoot that single arrow
* Then game over

In the
[previous](/writing/agdpp-demo-and-game-idea/index.html) episode, we took the
first step by drawing a balloon and an arrow. Here is what it looks like:

<center>
![Balloon shooter.](shooter.png)
</center>

And here is a list of possible stories to work on next:

* Balloon moves downwards
* **Arrow animates when shot**
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* Real graphics instead of circles

I sometimes find it hard to look too far into the future. Perhaps that is true
for our customer as well. I find it much easier to look at the current state of
the software and ask myself "What to work on next?"

When I run the game now, all I want to do is fire that arrow and see it flying
across the screen. So that is our next story!

## Acceptance

When working in an agile way, things might seem reversed from what you are used
to. For example, when doing TDD, we write the test before we write the code.
The same applies for stories. Before starting work on a story, we should figure
out the acceptance criteria. How do we know when we are done?

For the story with shooting arrow I think the acceptance criteria is that you
should see the arrow flying across the screen when you press the space key. Our
customer agrees on that.

The next step is to figure out how to write an automated test for that.

## The test we want to write

Just to recap, the test for the ballon shooter looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; BalloonShooter(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 50</span>
<span class="sd">    y: 50</span>
<span class="sd">    radius: 40</span>
<span class="sd">    color: &#39;red&#39;</span>
<span class="sd">...</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
That is, we run the ballonn shooter game, configure a set of events that should
be simulated, and then assert that certain things happens (game loop inits,
circles are drawn on screen, etc).

The test we want to write the for shoot behavior starts like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">The arrow moves when it is shot by pressing the space key:</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_keydown_space()],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; BalloonShooter(loop).run()</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We introduce a new event, keydown space, and simulate that it happens after one
frame, and then we simulate a couple of more frames.

This partial test fails because this new event does not yet exist, so let's fix
it.

## Adding a new event

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create_event_keydown_space</span><span class="p">():</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; GameLoop.create_event_keydown_space().is_keydown_space()</span>
<span class="sd">        True</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="n">Event</span><span class="p">(</span><span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">Event</span><span class="p">(</span><span class="n">pygame</span><span class="o">.</span><span class="n">KEYDOWN</span><span class="p">,</span> <span class="n">key</span><span class="o">=</span><span class="n">pygame</span><span class="o">.</span><span class="n">K_SPACE</span><span class="p">))</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Event</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">is_keydown_space</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame_event</span><span class="o">.</span><span class="n">type</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">KEYDOWN</span> <span class="ow">and</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame_event</span><span class="o">.</span><span class="n">key</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">K_SPACE</span>
</pre></div>
</div></div>
## TODO

* move on to filter out draw events, not possible

* implement more filtering in Events

* discuss how to get x, y coordinates

* implement collect

* finally a real failure

* implement quickly and now head of the arrow moves

* fix arrow animation

## Getting tangled up in tests

I'm not happy with the current tests. On the other hand, I'm not sure how to
improve them either.

I think I need to spend some time with my notebook. Maybe running. Hammock?

## Summary

The arrow now moves. So exciting! But it's a bummer that you have to restart
the game after each shot. Therefore I think the game over screen might be most
interesting to work on next. When the arrow misses (goes outside the screen)
the game should be over, and the arrow should be reset.

After that, I think collision check with balloon would be most interesting.

All those require tests of course. So we should probably work on getting
comfortable with the test setup as well.

See you in the next episode!
