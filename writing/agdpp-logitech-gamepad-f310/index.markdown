---
title: 'DRAFT: Programming a Logitech Gamepad F310'
date: 2023-05-15
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

I recently bought a pair of Logitech gamepads that me and my son use when
playing [SuperTuxKart](https://supertuxkart.net/Main_Page):

<p>
<center>
![Logitech Gamepad F310.](logitech-gamepad-f310.png)
</center>
</p>

I want to be able to use those gamepads in the balloon shooter as well. My
suspicion is that the balloon shooter will feel many times more as a "real"
game if we can control it using "real" game controllers. Even though we are all
about having fun here and learning, we still want this to feel like a real
game, not some toy example. So let's get started.

## Learning about events

How do we capture events from a Logitech gamepad?

One way to find out is to print all the events that pygame generates. We can
for example do that in the `tick` method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="nb">print</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>
            <span class="o">...</span>
</pre></div>
</div></div>
This makes the test suite fail since the print statement is outputting event
information that the tests do not expect to find.

This might be a downside of doctest, that it captures stdout and asserts on it.
Normally a print statement should not affect the function of the code, so it
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
But when we press a key on the Logitech gamepad, nothing happens.

However, if we look at the beginning of the event log, we see this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>&lt;Event(1541-JoyDeviceAdded {&#39;device_index&#39;: 0, &#39;guid&#39;: &#39;030000006d0400001dc2000014400000&#39;})&gt;
</pre></div>
</div></div>
So it seems that pygame has detected our gamepad.

## Initializing joysticks

We read about joysticks in the [pygame
documentation](https://www.pygame.org/docs/ref/joystick.html). It seems like
they must be initialized before events are generated for them.

> Joysticks are initialised on creation and are shut down when deallocated.
> Once the device is initialized the pygame event queue will start receiving
> events about its input.

We try to mimic the example in the documentation to initialize a joystick:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">,</span> <span class="n">resolution</span><span class="o">=</span><span class="p">(</span><span class="mi">1280</span><span class="p">,</span> <span class="mi">720</span><span class="p">),</span> <span class="n">fps</span><span class="o">=</span><span class="mi">60</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="n">joysticks</span> <span class="o">=</span> <span class="p">{}</span>
        <span class="k">try</span><span class="p">:</span>
            <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
                <span class="n">pygame_events</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">get</span><span class="p">()</span>
                <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">pygame_events</span><span class="p">:</span>
                    <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">type</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">JOYDEVICEADDED</span><span class="p">:</span>
                        <span class="n">joy</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">joystick</span><span class="o">.</span><span class="n">Joystick</span><span class="p">(</span><span class="n">event</span><span class="o">.</span><span class="n">device_index</span><span class="p">)</span>
                        <span class="n">joysticks</span><span class="p">[</span><span class="n">joy</span><span class="o">.</span><span class="n">get_instance_id</span><span class="p">()]</span> <span class="o">=</span> <span class="n">joy</span>
                    <span class="k">else</span><span class="p">:</span>
                        <span class="n">game</span><span class="o">.</span><span class="n">event</span><span class="p">(</span><span class="n">Event</span><span class="p">(</span><span class="n">event</span><span class="p">))</span>
                <span class="o">...</span>
</pre></div>
</div></div>
We don't handle `JOYDEVICEREMOVED` yet. We probably should, but unless we
unplug the gamepad while running the game, we should be fine.

This change passes all the tests. However, we are never simulating the
`JOYDEVICEADDED` event, so the code is never executed.

I think we will get faster feedback by just testing this thing for real. We can
come back and describe the joystick handling code in the form of tests later on
if we feel the need. And maybe test the `JOYDEVICEREMOVED` as well.

Anyway, if we run the game now and press keys on the gamepad, we see events
like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>&lt;Event(1536-JoyAxisMotion {&#39;joy&#39;: 0, &#39;instance_id&#39;: 0, &#39;axis&#39;: 0, &#39;value&#39;: 0.003906369212927641})&gt;
&lt;Event(1539-JoyButtonDown {&#39;joy&#39;: 0, &#39;instance_id&#39;: 0, &#39;button&#39;: 0})&gt;
&lt;Event(1540-JoyButtonUp {&#39;joy&#39;: 0, &#39;instance_id&#39;: 0, &#39;button&#39;: 0})&gt;
</pre></div>
</div></div>
I feel a disproportional sense of excitement and joy over this. We can now get
input from the Logitech gamepad. Kudos to pygame for making this relatively
straight forward. We are real game developers now! Now it's just a matter of
mapping events to actions in our game.

## Isolating input handling

For now we want to be able to play our game with both the keyboard and the
Logitech gamepad. I will most likely use the gamepad 99% of the time, but if
you don't have it, we still want you to be able to play the game.

Input handling is therefore something that is starting to become a little
complicated. It's not just a matter of mapping one event to one action.

Now we have this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">clone_shooting</span><span class="p">())</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_left</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">angle_left</span><span class="p">()</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_right</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">angle_right</span><span class="p">()</span>
</pre></div>
</div></div>
That is a one to one mapping between events and actions.

We still want this code to look similar but allow multiple events to generate
the same actions.

Here is what we end up with:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">action</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">get_shoot</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">bow</span><span class="o">.</span><span class="n">clone_shooting</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">bow</span><span class="o">.</span><span class="n">turn</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">get_turn_angle</span><span class="p">())</span>
</pre></div>
</div></div>
So we pass along events to an input handler, then we query it in the `update`
method, asking it if a shot action as triggered (from either input device), and
if so, modify `flying_arrows` as before. We do something similar for turning
the arrow. But instead of asking the input handler if a left/right action was
triggered, we ask it for an angle that we should turn the arrow. Since the
arrow can be turned with variable speed with the Logitech gamepad, this
makes more sense.

Before we move on to the input handler, I want to discuss another new thing
here which is the bow.

## Bow

Instead of doing `arrow.angle_left/right()` we do `bow.turn(angle)`. We have
extracted a concept called bow.

Right now it is a wrapper around an arrow, but the idea is that you might want
to draw more graphics for the bow.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Bow</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Arrow</span><span class="p">())</span>

    <span class="k">def</span> <span class="nf">turn</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">angle</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">set_angle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">angle</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">angle</span><span class="p">))</span>

    <span class="k">def</span> <span class="nf">clone_shooting</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">clone_shooting</span><span class="p">()</span>

    <span class="o">...</span>
</pre></div>
</div></div>
I'm not sure bow is the right name. Do we shoot arrows with a bow in our game?
Or is it some kind of cannon? I think we need to ask our product owner.

At the moment we are not doing any drawing except the arrow, so the bow just
acts as a placeholder to attract new functionality. And the concept of a bow
makes sense. You need to shoot the arrow with something. And when you shoot,
the arrow leaves the bow and goes into the list of flying arrows.

## Input handler

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">InputHandler</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span> <span class="o">=</span> <span class="n">ResettableValue</span><span class="p">(</span><span class="mi">0</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">shoot_down</span> <span class="o">=</span> <span class="n">ResettableValue</span><span class="p">(</span><span class="kc">False</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">get_shoot</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">shoot</span>

    <span class="k">def</span> <span class="nf">get_turn_angle</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">turn_angle</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">shoot</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">shoot_down</span><span class="o">.</span><span class="n">get_and_reset</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">turn_angle</span> <span class="o">=</span> <span class="n">Angle</span><span class="o">.</span><span class="n">fraction_of_whole</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">get</span><span class="p">()</span><span class="o">*</span><span class="n">dt</span><span class="o">*</span><span class="mi">1</span><span class="o">/</span><span class="mi">2000</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">action</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown</span><span class="p">(</span><span class="n">KEY_SPACE</span><span class="p">)</span> <span class="ow">or</span> <span class="n">event</span><span class="o">.</span><span class="n">is_joystick_down</span><span class="p">(</span><span class="n">XBOX_A</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">shoot_down</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="kc">True</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown</span><span class="p">(</span><span class="n">KEY_LEFT</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="o">-</span><span class="mi">1</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keyup</span><span class="p">(</span><span class="n">KEY_LEFT</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">reset</span><span class="p">()</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown</span><span class="p">(</span><span class="n">KEY_RIGHT</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="mi">1</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keyup</span><span class="p">(</span><span class="n">KEY_RIGHT</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">reset</span><span class="p">()</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_joystick_motion</span><span class="p">()</span> <span class="ow">and</span> <span class="n">event</span><span class="o">.</span><span class="n">get_axis</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
            <span class="k">if</span> <span class="nb">abs</span><span class="p">(</span><span class="n">event</span><span class="o">.</span><span class="n">get_value</span><span class="p">())</span> <span class="o">&gt;</span> <span class="mf">0.01</span><span class="p">:</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="n">event</span><span class="o">.</span><span class="n">get_value</span><span class="p">())</span>
            <span class="k">else</span><span class="p">:</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">arrow_turn_factor</span><span class="o">.</span><span class="n">reset</span><span class="p">()</span>
</pre></div>
</div></div>
## Angle class

## Many refactorings

* work towards a nice input handler (mocks vs state based)

## Summary

See you in the next episode!

## TODO

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>
    commit 3b17692a64e48fc8712c0c085cdf94e063926251
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sat Apr 29 06:08:41 2023 +0200

        Event looping is done in loop.

    ...

    commit 1e6d7ba35e721c591a7246a71af633ccbe17e0df
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Tue May 2 06:41:55 2023 +0200

        InputHandler does not now arrow angle, just the turn angle.

* device_index vs instance id
