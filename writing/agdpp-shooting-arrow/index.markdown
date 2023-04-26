---
title: 'DRAFT: Shooting the arrow'
date: 2023-04-26
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we continue towards the first version of the balloon shooter.
It's time to shoot the arrow!

## Video version

The video version of this episode:

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/CfhEcp9Qghc" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
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
That is, we run the balloon shooter game, configure a set of events that should
be simulated, and then assert that certain things happens (game loop inits,
circles are drawn on screen, etc).

The test we want to write the for the shoot behavior starts like this:

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
frame, and then we simulate a couple of more frames. The reason we do include a
couple of frames is that we want to observe that the arrows moves between
different frames.

This partial test fails because this new event does not yet exist, so let's fix
it.

## Adding a new event

We add the new event like this:

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
We figure out the Pygame event parameters to use by reading the documentation.

We verify that we got it correct by printing all events and when running the
game, press different keys to see if it correctly only captures the space key.

It seems to work.

## Filtering events

The initial test now runs, but if we print all the events that we get, there is
a lot of noise:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
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
What we want to observe in that list of events is that the arrow has been drawn
in different positions (indicating movement).

So first we want to filter out the `DRAW_CIRCLE` events that are for the head
of the arrow.

We want to write like this:

    >>> events.filter("DRAW_CIRCLE", radius=10)

Filtering on event fields is not yet possible, but we own the library, and the
fix goes smoothly.

Once that is done, we get this list of events:

    DRAW_CIRCLE =>
        x: 500
        y: 500
        radius: 10
        color: 'blue'
    DRAW_CIRCLE =>
        x: 500
        y: 500
        radius: 10
        color: 'blue'
    DRAW_CIRCLE =>
        x: 500
        y: 500
        radius: 10
        color: 'blue'
    DRAW_CIRCLE =>
        x: 500
        y: 500
        radius: 10
        color: 'blue'

This means that when we ran our game in test mode, four frames where drawn and
here are all the circles with radius 10. We use 10 here because we know that
the head of the arrow is the only circle that is drawn with radius 10. But it
is not bullet proof. It would be better if we could pass an id to the draw
method call that is included in the event as well so that we could more
accurately identify objects. But this will do for now.

## Extracting positions

In the output above, we can look at the x and y coordinates and see if they
change. But there are also other fields that we don't care about in this test.
Let's filter them out like this:

    >>> events.filter("DRAW_CIRCLE", radius=10).collect("x", "y")
    [(500, 500), (500, 500), (500, 500), (500, 500)]

Again, the `collect` method does not exist, but we can extend our library
with it.

Now we have a list of positions where the head of the arrow is drawn. It
doesn't seem to change, which we can see more clearly by making the collection
into a set and seeing that it has only one element:

    >>> set(events.filter("DRAW_CIRCLE", radius=10).collect("x", "y"))
    {(500, 500)}

## Real test failure

We want the arrow to move, so let's write an assert like this:

    >>> len(arrow_head_positions) > 1
    True
    >>> len(set(arrow_head_positions)) > 1
    True

That is, we should get more than one position, and the set of all those
positions should also be larger than one, indicating movement.

The first assertion passes, but the other one fails. That is expected. Finally
we have the assertion failure that we wanted. Took a bit of time, huh? That
might tell us something about the design of our system. We'll talk about it
later.

## Implementation

First, we modify the event handler to check for the space key and shoot the
arrow if so:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
            <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
        <span class="o">...</span>

    <span class="o">...</span>
</pre></div>
</div></div>
The shooting mechanism we implement like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">y</span> <span class="o">=</span> <span class="mi">500</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">shooting</span> <span class="o">=</span> <span class="kc">False</span>

    <span class="k">def</span> <span class="nf">shoot</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">shooting</span> <span class="o">=</span> <span class="kc">True</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">shooting</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">y</span> <span class="o">-=</span> <span class="n">dt</span>
</pre></div>
</div></div>
We also adjust the drawing code so that all three circles that are drawn for
the arrow are drawn relative to the now variable y position.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
    <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">10</span><span class="p">)</span>
    <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="o">+</span><span class="mi">20</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">15</span><span class="p">)</span>
    <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="o">+</span><span class="mi">40</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">)</span>
</pre></div>
</div></div>
## Getting tangled up in tests

* test objection
    * whole lotta work in events
    * not full coverage
    * testing internals?

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

Source code from this episode: https://github.com/rickardlindberg/agdpp/tree/shoot-arrow

See you in the next episode!
