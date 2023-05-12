---
title: 'DRAFT: Programming a Logitech controller'
date: 2023-05-12
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

I recently bought a pair of Logitech Xbox-style controllers that me and my son
has used to play Super Tux Kart.

I want to be able to use those controllers in the balloon shooter as well. My
suspicion is that the balloon shooter will feel many times more as a "real"
game if we can control it using a real game controller. Even though we are all
about having fun here and learning, we still want this to feel like a real
game, not some toy example. So let's get started.

## Learning about events

How do we capture events from a Logitech controller?

One way to find out is to print all the events that pygame generates. we can
for example do that here:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="nb">print</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>
            <span class="o">...</span>
</pre></div>
</div></div>
This makes the test suite fail since the print statement is outputting event
information that the test does not expect to find.

This might be a downside of doctest, that it captures stdout and asserts on it.
Normally a print statement should not affect the logic of the code, so it
should be fine.

On the other hand, if we use print statements for debugging, maybe it's a good
thing that out test suite fails so that we are remembered to keep the debug
session short and remove it once we are done.

Anyway, if we run the game now and press keys we can see things like this in
the output:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>&lt;Event(771-TextInput {&#39;text&#39;: &#39; &#39;, &#39;window&#39;: None})&gt;
&lt;Event(769-KeyUp {&#39;unicode&#39;: &#39; &#39;, &#39;key&#39;: 32, &#39;mod&#39;: 0, &#39;scancode&#39;: 44, &#39;window&#39;: None})&gt;
&lt;Event(768-KeyDown {&#39;unicode&#39;: &#39;&#39;, &#39;key&#39;: 1073742049, &#39;mod&#39;: 1, &#39;scancode&#39;: 225, &#39;window&#39;: None})&gt;
&lt;Event(768-KeyDown {&#39;unicode&#39;: &#39;&#39;, &#39;key&#39;: 1073742050, &#39;mod&#39;: 257, &#39;scancode&#39;: 226, &#39;window&#39;: None})&gt;
</pre></div>
</div></div>
So we can clearly see what pygame thinks is happening.

But when we press a key on the Logitech controller, nothing happens.

But we see something about a joystick further up:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>...
</pre></div>
</div></div>
## Initializing joysticks

We read about joysticks in the [pygame documentation](). It seems like they
must be initialized before events are generated for them.

...

## Summary

See you in the next episode!

## TODO

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>
<center>
![Bug with drawing circles on negative x values.](negative-x-draw-bug.png)
</center>

* Joystick

    * work towards a nice input handler (mocks vs state based)
    * Angle class

    commit 3b17692a64e48fc8712c0c085cdf94e063926251
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sat Apr 29 06:08:41 2023 +0200

        Event looping is done in loop.

    ...

    commit 1e6d7ba35e721c591a7246a71af633ccbe17e0df
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Tue May 2 06:41:55 2023 +0200

        InputHandler does not now arrow angle, just the turn angle.
