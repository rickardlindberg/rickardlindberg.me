---
title: 'DevLog 009: Improve timeline scrubbing'
date: 2023-08-03
tags: devlog,rlvideo
devlog: true
---

As a try to edit some footage with my [video
editor](/projects/rlvideo/index.html), I get annoyed by a timeline scrubbing
issue.

Scrubbing the timeline means clicking and dragging the playhead and then the
frame at that position will play. This works fine today if you click and drag,
but if you only click, nothing happens:

<p>
<center>
![Scrub problem.](scrub-problem.png)
</center>
</p>

Sometimes I just want to place the playhead at a certain position. And then I
just want to click.

That's what we'll work on fixing today.

## Reviewing the scrub action

Here is the scrub action:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">ScrubAction</span><span class="p">(</span><span class="n">Action</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">player</span><span class="p">,</span> <span class="n">scrollbar</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">player</span> <span class="o">=</span> <span class="n">player</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span> <span class="o">=</span> <span class="n">scrollbar</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">mouse_up</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">left_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="n">x</span>

    <span class="k">def</span> <span class="nf">mouse_up</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="kc">None</span>

    <span class="k">def</span> <span class="nf">mouse_move</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="ow">is</span> <span class="ow">not</span> <span class="kc">None</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">player</span><span class="o">.</span><span class="n">scrub</span><span class="p">(</span>
                <span class="nb">int</span><span class="p">(</span><span class="nb">round</span><span class="p">(</span>
                    <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">content_start</span>
                    <span class="o">+</span>
                    <span class="n">x</span><span class="o">/</span><span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">one_length_in_pixels</span>
                <span class="p">))</span>
            <span class="p">)</span>
</pre></div>
</div></div>
We can see that the scrubbing is happening only when we move the mouse, not
if we just left click.

The solution seems obvious: make sure to scrub on the click as well.

Let's see how we can move slowly and carefully and pay attention to design as
we go along. Let's start with a test.

## Testing new functionality

This is the test that I come up with:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I scrub the player when clicked:</span>

<span class="sd">&gt;&gt;&gt; class MockPlayer:</span>
<span class="sd">...     def scrub(self, position):</span>
<span class="sd">...         print(f&quot;scrub {position}&quot;)</span>
<span class="sd">&gt;&gt;&gt; class MockScrollbar:</span>
<span class="sd">...     content_start = 0</span>
<span class="sd">...     one_length_in_pixels = 1</span>
<span class="sd">&gt;&gt;&gt; action = ScrubAction(player=MockPlayer(), scrollbar=MockScrollbar())</span>
<span class="sd">&gt;&gt;&gt; action.simulate_click(x=10)</span>
<span class="sd">scrub 10</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The `left_mouse_down` currently takes both the x and y coordinates. In this
test, we only care about the x coordinate. That's why I introduced
`Action.simulate_click`. The idea is that it should simulate the calls that
GTK does when a left click happens. My idea is to extend this further with
something like `Action.simulate_drag` which will fire `left_mouse_down`,
`mouse_move`, and `mouse_up` in the same way that GTK would do it.

I implement it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">simulate_click</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="o">=</span><span class="mi">0</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">0</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">left_mouse_down</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">)</span>
</pre></div>
</div></div>
To make the test pass, I call `self.player.scrub` in the `left_mouse_down`
event as well. I extract it to a common method to remove the duplication.

This passes the tests, and when I try it in the application, it works as
intended.

Are we done?

## A concern

Let's take a moment to think about some design issues.

One thing that worry me is that `Action.simulate_click` does not actually
simulate clicks in the right way. That is, when we hook this up with GTK, the
same kinds of events will not be generated.

Let's have a look at how it works today.

Here is how `*_mouse_down` is handled:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">timeline</span> <span class="o">=</span> <span class="n">Gtk</span><span class="o">.</span><span class="n">DrawingArea</span><span class="p">()</span>
<span class="n">timeline</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;button-press-event&quot;</span><span class="p">,</span> <span class="n">timeline_button</span><span class="p">)</span>
<span class="n">timeline</span><span class="o">.</span><span class="n">add_events</span><span class="p">(</span>
    <span class="n">timeline</span><span class="o">.</span><span class="n">get_events</span><span class="p">()</span> <span class="o">|</span>
    <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">SCROLL_MASK</span> <span class="o">|</span>
    <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">BUTTON_PRESS_MASK</span> <span class="o">|</span>
    <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">BUTTON_RELEASE_MASK</span> <span class="o">|</span>
    <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">POINTER_MOTION_MASK</span>
<span class="p">)</span>
<span class="k">def</span> <span class="nf">timeline_button</span><span class="p">(</span><span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
    <span class="c1"># TODO: clarify what translate_coordinates do</span>
    <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">1</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">left_mouse_down</span><span class="p">(</span><span class="o">*</span><span class="n">timeline</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
            <span class="n">main_window</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">y</span>
        <span class="p">))</span>
    <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">3</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">right_mouse_down</span><span class="p">(</span><span class="o">*</span><span class="n">timeline</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
            <span class="n">main_window</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">y</span>
        <span class="p">),</span> <span class="n">GtkGui</span><span class="p">(</span><span class="n">event</span><span class="p">))</span>
</pre></div>
</div></div>
This code exists in a method which has a bunch of other GTK setup code and is
quite long.

Let's see if we can extract a GTK widget that has all the mechanisms for custom
drawing and event handling.

I slowly start to extract pieces, and eventually end up with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">CustomDrawWidget</span><span class="p">(</span><span class="n">Gtk</span><span class="o">.</span><span class="n">DrawingArea</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">main_window</span><span class="p">,</span> <span class="n">custom_draw_handler</span><span class="p">):</span>
        <span class="n">Gtk</span><span class="o">.</span><span class="n">DrawingArea</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">add_events</span><span class="p">(</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">get_events</span><span class="p">()</span> <span class="o">|</span>
            <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">SCROLL_MASK</span> <span class="o">|</span>
            <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">BUTTON_PRESS_MASK</span> <span class="o">|</span>
            <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">BUTTON_RELEASE_MASK</span> <span class="o">|</span>
            <span class="n">Gdk</span><span class="o">.</span><span class="n">EventMask</span><span class="o">.</span><span class="n">POINTER_MOTION_MASK</span>
        <span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;draw&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">on_draw</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;button-press-event&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">on_button_press_event</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;button-release-event&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">on_button_release_event</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;motion-notify-event&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">on_motion_notify_event</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span> <span class="o">=</span> <span class="n">RectangleMap</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">custom_draw_handler</span> <span class="o">=</span> <span class="n">custom_draw_handler</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="kc">None</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">main_window</span> <span class="o">=</span> <span class="n">main_window</span>

    <span class="k">def</span> <span class="nf">on_draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">widget</span><span class="p">,</span> <span class="n">context</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">clear</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">custom_draw_handler</span><span class="p">(</span><span class="n">context</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">on_button_press_event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="n">x</span><span class="p">,</span> <span class="n">y</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_coordinates_relative_self</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">1</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">Action</span><span class="p">())</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">left_mouse_down</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">3</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">Action</span><span class="p">())</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">right_mouse_down</span><span class="p">(</span><span class="n">GtkGui</span><span class="p">(</span><span class="n">event</span><span class="p">))</span>

    <span class="k">def</span> <span class="nf">on_motion_notify_event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="n">x</span><span class="p">,</span> <span class="n">y</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_coordinates_relative_self</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">mouse_move</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">Action</span><span class="p">())</span><span class="o">.</span><span class="n">mouse_move</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">on_button_release_event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">mouse_up</span><span class="p">()</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="kc">None</span>

    <span class="k">def</span> <span class="nf">get_coordinates_relative_self</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">main_window</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">y</span>
        <span class="p">)</span>
</pre></div>
</div></div>
The timeline is then created like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">timeline</span> <span class="o">=</span> <span class="n">CustomDrawWidget</span><span class="p">(</span>
    <span class="n">main_window</span><span class="o">=</span><span class="n">main_window</span><span class="p">,</span>
    <span class="n">custom_draw_handler</span><span class="o">=</span><span class="n">timeline_draw</span><span class="p">,</span>
<span class="p">)</span>
</pre></div>
</div></div>
This part of the code base does not have many tests. I therefore moved slowly
and tested my changes manually after each small step.

Let's discuss some aspects of this and what we have done:

* The `CustomDrawWidget` now owns the rectangle map. (The timeline gets a
  reference to it, but there it only one instance, and it is created by
  `CustomDrawWidget`.)

* The `CustomDrawWidget` can handle clearing of the rectangle map on redraw,
  something that the timeline previously did.

* The `CustomDrawWidget` can handle mouse events and take the appropriate
  action by using the rectangle map.

* The timeline widget no longer knows about mouse events. It just has a
  rectangle map that it can fill with actions to be performed.

When I look at this, I feel like there are so many more things to improve.
However, I will practice stopping here and think that I made a bit of
improvement.

We can now see a bit more clearly the connection between GTK events, the
rectangle map, and what methods are called on the action. And, if we need a
second component that does custom drawing and handles events with a rectangle
map, we can re-use `CustomDrawWidget` and do not need to duplicate as much.

## Summary

We improved the application a tiny bit by allowing click on the timeline to
position the playhead. We also cleaned up the code base in the area we touched.
It now reflects a little better the ideas that we have about the code. I'm
happy with this progress.
