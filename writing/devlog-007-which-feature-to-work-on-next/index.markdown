---
title: 'DevLog 007: Which feature to work on next?'
date: 2023-08-01
tags: devlog,rlvideo,refactoring
devlog: true
---

In this session I will select what to work on next in my [video
editor](/projects/rlvideo/index.html) by trying to use it to edit some footage
and see where I get stuck.

I've previously managed to create a project which has some footage imported and
proxy clips generated. I can open that project like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ rlvideo my-project.rlvideo
</pre></div>
</div></div>
When I do that, two things happen that annoy me.

First of all, there are lots of exceptions printed to the console:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Traceback (most recent call last):
  File &quot;/home/rick/rlvideo/rlvideolib/gui/gtk.py&quot;, line 80, in timeline_draw
    self.timeline.draw_cairo(
  File &quot;/home/rick/rlvideo/rlvideolib/gui/generic.py&quot;, line 200, in draw_cairo
    self.draw_scrollbar(context, area, playhead_position)
  File &quot;/home/rick/rlvideo/rlvideolib/gui/generic.py&quot;, line 287, in draw_scrollbar
    self.rectangle_map.add(Rectangle(
  File &quot;/home/rick/rlvideo/rlvideolib/graphics/rectangle.py&quot;, line 19, in __init__
    raise ValueError(&quot;Width must be &gt; 0.&quot;)
ValueError: Width must be &gt; 0.
</pre></div>
</div></div>
And second of all, it seems like it's loading proxy clips again even though
they are already generated:

<p>
<center>
![Loading.](loading.png)
</center>
</p>

Which one should I work on? Should I work on something else? What is most
important?

## Analysis

Let's do an analysis of why the two problems occur.

The exception when drawing the scrollbar happens because there are too many
clips in a too small window, so the width of the scrollbar handle gets smaller
than 1 pixel. It can be worked around by zooming out a bit so that a larger
portion of the timeline is visible.

This is obviously not good, but not the end of the world.

The fix probably involves setting a minimum width on the handle.

What about proxies?

Actually, proxies are not created again, but in order to find the correct proxy
for a clip, the clip's md5 sum has to be calculated. This is much faster than
generating the proxy, but still takes some time, delaying me when I want to
edit clips.

The fix probably involves storing the path of the proxy clip in the project
file.

It is also not the end of the world. I can open the editor, go make some
coffee, and maybe when I'm back, it's done.

## Strategy

So which should I work on?

If you work in an agile fashion, doing evolutionary design, what should happen
is that it should get easier and easier to work with the code base and add new
features. I learned that from [James
Shore](https://www.jamesshore.com/v2/books/aoad2/design).

Say I start working on the scrollbar exception now. When I'm done with that,
it should be easier to fix the proxy loading issue than it was before, assuming
that the areas that need change overlap.

With that kind of thinking, it doesn't matter that much what we choose to work
on as long as we think it is somewhat important. Just pick one and the next
thing will be easier.

It almost sounds too good to be true, but I believe in it. For this to work
though, we need to practice evolutionary design. We'll do that today.

Let's pick the scrollbar issue.

## Review

The error happens in `draw_scrollbar` which looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">draw_scrollbar</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">context</span><span class="p">,</span> <span class="n">area</span><span class="p">,</span> <span class="n">playhead_position</span><span class="p">):</span>
    <span class="n">x_start</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">region_shown</span><span class="o">.</span><span class="n">start</span> <span class="o">/</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">whole_region</span><span class="o">.</span><span class="n">length</span> <span class="o">*</span> <span class="n">area</span><span class="o">.</span><span class="n">width</span>
    <span class="n">x_end</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">region_shown</span><span class="o">.</span><span class="n">end</span> <span class="o">/</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">whole_region</span><span class="o">.</span><span class="n">length</span> <span class="o">*</span> <span class="n">area</span><span class="o">.</span><span class="n">width</span>
    <span class="n">playhead_x</span> <span class="o">=</span> <span class="n">playhead_position</span> <span class="o">/</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">whole_region</span><span class="o">.</span><span class="n">length</span> <span class="o">*</span> <span class="n">area</span><span class="o">.</span><span class="n">width</span>

    <span class="c1"># TODO: add callback mechanism in rectangle map</span>
    <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">w</span><span class="p">,</span> <span class="n">h</span> <span class="o">=</span> <span class="p">(</span>
        <span class="n">area</span><span class="o">.</span><span class="n">x</span><span class="o">+</span><span class="n">x_start</span><span class="p">,</span>
        <span class="n">area</span><span class="o">.</span><span class="n">y</span><span class="p">,</span>
        <span class="n">x_end</span><span class="o">-</span><span class="n">x_start</span><span class="p">,</span>
        <span class="n">area</span><span class="o">.</span><span class="n">height</span>
    <span class="p">)</span>
    <span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
        <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
        <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
        <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
        <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
    <span class="p">),</span> <span class="s2">&quot;position&quot;</span><span class="p">)</span>

    <span class="n">context</span><span class="o">.</span><span class="n">rectangle</span><span class="p">(</span><span class="n">area</span><span class="o">.</span><span class="n">x</span><span class="p">,</span> <span class="n">area</span><span class="o">.</span><span class="n">y</span><span class="p">,</span> <span class="n">area</span><span class="o">.</span><span class="n">width</span><span class="p">,</span> <span class="n">area</span><span class="o">.</span><span class="n">height</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">set_source_rgba</span><span class="p">(</span><span class="mf">0.4</span><span class="p">,</span> <span class="mf">0.9</span><span class="p">,</span> <span class="mf">0.4</span><span class="p">,</span> <span class="mf">0.5</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">fill</span><span class="p">()</span>

    <span class="n">scroll_box</span> <span class="o">=</span> <span class="n">Rectangle</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">rectangle</span><span class="p">(</span><span class="n">scroll_box</span><span class="o">.</span><span class="n">x</span><span class="p">,</span> <span class="n">scroll_box</span><span class="o">.</span><span class="n">y</span><span class="p">,</span> <span class="n">scroll_box</span><span class="o">.</span><span class="n">width</span><span class="p">,</span> <span class="n">scroll_box</span><span class="o">.</span><span class="n">height</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">set_source_rgba</span><span class="p">(</span><span class="mf">0.4</span><span class="p">,</span> <span class="mf">0.9</span><span class="p">,</span> <span class="mf">0.4</span><span class="p">,</span> <span class="mf">0.5</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">fill</span><span class="p">()</span>

    <span class="c1"># Playhead</span>
    <span class="n">context</span><span class="o">.</span><span class="n">set_source_rgb</span><span class="p">(</span><span class="mf">0.1</span><span class="p">,</span> <span class="mf">0.1</span><span class="p">,</span> <span class="mf">0.1</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">move_to</span><span class="p">(</span><span class="n">playhead_x</span><span class="p">,</span> <span class="n">area</span><span class="o">.</span><span class="n">top</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">line_to</span><span class="p">(</span><span class="n">playhead_x</span><span class="p">,</span> <span class="n">area</span><span class="o">.</span><span class="n">bottom</span><span class="p">)</span>
    <span class="n">context</span><span class="o">.</span><span class="n">stroke</span><span class="p">()</span>

    <span class="n">context</span><span class="o">.</span><span class="n">set_source_rgb</span><span class="p">(</span><span class="mf">0.1</span><span class="p">,</span> <span class="mf">0.1</span><span class="p">,</span> <span class="mf">0.1</span><span class="p">)</span>
    <span class="n">scroll_box</span><span class="o">.</span><span class="n">draw_pixel_perfect_border</span><span class="p">(</span><span class="n">context</span><span class="p">,</span> <span class="mi">2</span><span class="p">)</span>
</pre></div>
</div></div>
When I look at this, it's difficult for me to see what is going on. It is just
too long and does too much. It doesn't clearly represent what I had in mind
when I wrote it.

If we are going to do evolutionary design, we have to pay more attention to
design. All the time.

It's fine that I didn't pay too much attention last time I modified this
method, but now that we are here again, let's give it some extra love so that
it is easier to work with next time.

## Further review

The error happens when creating the rectangle in the following piece of code:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># TODO: add callback mechanism in rectangle map</span>
<span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">w</span><span class="p">,</span> <span class="n">h</span> <span class="o">=</span> <span class="p">(</span>
    <span class="n">area</span><span class="o">.</span><span class="n">x</span><span class="o">+</span><span class="n">x_start</span><span class="p">,</span>
    <span class="n">area</span><span class="o">.</span><span class="n">y</span><span class="p">,</span>
    <span class="n">x_end</span><span class="o">-</span><span class="n">x_start</span><span class="p">,</span>
    <span class="n">area</span><span class="o">.</span><span class="n">height</span>
<span class="p">)</span>
<span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
<span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
<span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
    <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
    <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
    <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
    <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
<span class="p">),</span> <span class="s2">&quot;position&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
Look, there is even a TODO comment there. Now that we are touching this piece
of code again, perhaps it's time to deal with it.

## The rectangle map

The rectangle map is used to store areas of the screen that the user can
interact with. You can put objects at a given rectangle and retrieve them by
position. Here is an example:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; r = RectangleMap()</span>
<span class="sd">&gt;&gt;&gt; r.add(Rectangle(x=0, y=0, width=10, height=10), &quot;item&quot;)</span>
<span class="sd">&gt;&gt;&gt; r.get(5, 5)</span>
<span class="sd">&#39;item&#39;</span>
<span class="sd">&gt;&gt;&gt; r.get(100, 100) is None</span>
<span class="sd">True</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
In the timeline area, each cut puts itself in a rectangle, allowing a context
menu to be shown when it is right clicked like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
    <span class="n">cut</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">cut</span><span class="p">,</span> <span class="n">Cut</span><span class="p">):</span>
        <span class="c1"># show context menu</span>
</pre></div>
</div></div>
The TODO comment that I wrote suggests that we should instead store objects
that can handle `right_mouse_down` events for example so that we don't need to
check instances at the outermost event handler.

Let's see if we can do it.

## Action test

I sketch this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Action</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">left_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">pass</span>

    <span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
        <span class="k">pass</span>

    <span class="k">def</span> <span class="nf">mouse_move</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">pass</span>

    <span class="k">def</span> <span class="nf">mouse_up</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">pass</span>

<span class="k">class</span> <span class="nc">ScrollbarDragAction</span><span class="p">(</span><span class="n">Action</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">timeline</span><span class="p">,</span> <span class="n">scrollbar</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span> <span class="o">=</span> <span class="n">timeline</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span> <span class="o">=</span> <span class="n">scrollbar</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">mouse_up</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">left_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="n">x</span>

    <span class="k">def</span> <span class="nf">mouse_up</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="kc">None</span>

    <span class="k">def</span> <span class="nf">mouse_move</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="ow">is</span> <span class="ow">not</span> <span class="kc">None</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">set_scrollbar</span><span class="p">(</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">move_scrollbar</span><span class="p">(</span>
                    <span class="n">x</span> <span class="o">-</span> <span class="bp">self</span><span class="o">.</span><span class="n">x</span>
                <span class="p">)</span>
            <span class="p">)</span>
</pre></div>
</div></div>
Let's see if we can use it.

I modify `right_mouse_down` and all the other event handlers to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
    <span class="n">item</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">item</span><span class="p">,</span> <span class="n">Action</span><span class="p">):</span>
        <span class="n">item</span><span class="o">.</span><span class="n">right_mouse_down</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">down_item</span> <span class="o">=</span> <span class="n">item</span>
        <span class="k">return</span>
    <span class="o">...</span>
</pre></div>
</div></div>
This is special handling for the case where the entry in the rectangle map is
an `Action`. Eventually, we want there to be only actions in there, and then
the instance check can be removed.

Next I change what we put into the rectangle map for the scrollbar to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
    <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
    <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
    <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
    <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
<span class="p">),</span> <span class="n">ScrollbarDragAction</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="p">))</span>
</pre></div>
</div></div>
Boom! Test failure:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Failed example:
    timeline.rectangle_map # doctest: +ELLIPSIS
Differences (ndiff with -expected +actual):
    ...
      Rectangle(x=0, y=0, width=300, height=20):
        scrub
      Rectangle(x=0, y=77, width=300, height=23):
    -   position
    +   &lt;rlvideolib.gui.generic.ScrollbarDragAction object at 0x7fd1f8891d00&gt;
</pre></div>
</div></div>
There is now another object in the rectangle map. Let's modify the test to
assert that instead.

The question now is, will it work in the application?

This behavior I think lacks tests, so let's try.

Nothing happens.

I review the code and find that I had forgotten the `mouse_move` event:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">mouse_move</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
    <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">down_item</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">down_item</span><span class="o">.</span><span class="n">mouse_move</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
        <span class="k">return</span>
    <span class="o">...</span>
</pre></div>
</div></div>
And that actually works!

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Add a ScrollbarDragAction instead of position string.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.405s

OK
[main 87c9b07] Add a ScrollbarDragAction instead of position string.
 1 file changed, 59 insertions(+), 2 deletions(-)
</pre></div>
</div></div>
I make the same change for the remaining actions.

Here is the one for scrubbing the timeline:

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
And here is the one for moving a cut and opening the context menu for a cut:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">CutAction</span><span class="p">(</span><span class="n">Action</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">project</span><span class="p">,</span> <span class="n">cut</span><span class="p">,</span> <span class="n">scrollbar</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">project</span> <span class="o">=</span> <span class="n">project</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">cut</span> <span class="o">=</span> <span class="n">cut</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span> <span class="o">=</span> <span class="n">scrollbar</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">mouse_up</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">left_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">transaction</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">new_transaction</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="n">x</span>

    <span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
        <span class="k">def</span> <span class="nf">mix_strategy_updater</span><span class="p">(</span><span class="n">value</span><span class="p">):</span>
            <span class="k">def</span> <span class="nf">update</span><span class="p">():</span>
                <span class="k">with</span> <span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">new_transaction</span><span class="p">()</span> <span class="k">as</span> <span class="n">transaction</span><span class="p">:</span>
                    <span class="n">transaction</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">cut</span><span class="o">.</span><span class="n">id</span><span class="p">,</span> <span class="k">lambda</span> <span class="n">cut</span><span class="p">:</span>
                        <span class="n">cut</span><span class="o">.</span><span class="n">with_mix_strategy</span><span class="p">(</span><span class="n">value</span><span class="p">))</span>
            <span class="k">return</span> <span class="n">update</span>
        <span class="n">gui</span><span class="o">.</span><span class="n">show_context_menu</span><span class="p">([</span>
            <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;over&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="n">mix_strategy_updater</span><span class="p">(</span><span class="s2">&quot;over&quot;</span><span class="p">)),</span>
            <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;under&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="n">mix_strategy_updater</span><span class="p">(</span><span class="s2">&quot;under&quot;</span><span class="p">)),</span>
        <span class="p">])</span>

    <span class="k">def</span> <span class="nf">mouse_up</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">transaction</span> <span class="o">=</span> <span class="kc">None</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="kc">None</span>

    <span class="k">def</span> <span class="nf">mouse_move</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">transaction</span> <span class="ow">is</span> <span class="ow">not</span> <span class="kc">None</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">transaction</span><span class="o">.</span><span class="n">rollback</span><span class="p">()</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">transaction</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">cut</span><span class="o">.</span><span class="n">id</span><span class="p">,</span> <span class="k">lambda</span> <span class="n">cut</span><span class="p">:</span>
                <span class="n">cut</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="nb">int</span><span class="p">((</span><span class="n">x</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span><span class="o">/</span><span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="o">.</span><span class="n">one_length_in_pixels</span><span class="p">)))</span>
</pre></div>
</div></div>
## Clean up

At this point, we only put actions into the rectangle map, and we can simplify
the event handlers to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">left_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">Action</span><span class="p">())</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">left_mouse_down</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">Action</span><span class="p">())</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">right_mouse_down</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">mouse_move</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
    <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">mouse_move</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">Action</span><span class="p">())</span><span class="o">.</span><span class="n">mouse_move</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">mouse_up</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
    <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span><span class="o">.</span><span class="n">mouse_up</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">down_action</span> <span class="o">=</span> <span class="kc">None</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Timeline assumes there are Actions in rectangle map.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.381s

OK
[main 3c8e9b9] Timeline assumes there are Actions in rectangle map.
 Date: Mon Jul 31 14:32:06 2023 +0200
 2 files changed, 14 insertions(+), 64 deletions(-)
</pre></div>
</div></div>
## Transaction problem

Everything seems to work fine. However, I notice that the committing of the
transaction has disappeared.

This is not tested anywhere, missed my manual tests, and is pretty severe.

Let's see if we can make the code a little more reliable. I write this test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; project = Project.new()</span>
<span class="sd">&gt;&gt;&gt; transaction = project.new_transaction()</span>
<span class="sd">&gt;&gt;&gt; transaction = project.new_transaction()</span>
<span class="sd">Traceback (most recent call last):</span>
<span class="sd">  ...</span>
<span class="sd">ValueError: transaction already in progress</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
I make it pass, and I am now more confident that this error will show up when
testing in the application.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Ensure there can be only one transaction active at a time.&#39;
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.402s

OK
[main 2b36bdb] Ensure there can be only one transaction active at a time.
 2 files changed, 34 insertions(+), 7 deletions(-)
</pre></div>
</div></div>
And sure enough, it does. The second time I try to drag a cut, I get the
"transaction already in progress" error.

Nice!

The fix:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>     def mouse_up(self):
<span class="gi">+        if self.transaction:</span>
<span class="gi">+            self.transaction.commit()</span>
         self.transaction = None
         self.x = None
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Ensure CutAction transaction is commited at mouse_up.&#39;
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.406s

OK
[main 5fd460d] Ensure CutAction transaction is commited at mouse_up.
 1 file changed, 4 insertions(+), 1 deletion(-)
</pre></div>
</div></div>
Normally you use a transaction like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">with</span> <span class="n">project</span><span class="o">.</span><span class="n">new_transaction</span><span class="p">()</span> <span class="k">as</span> <span class="n">transaction</span><span class="p">:</span>
    <span class="n">_</span> <span class="o">=</span> <span class="n">transaction</span><span class="o">.</span><span class="n">add_text_clip</span><span class="p">(</span><span class="s2">&quot;hello&quot;</span><span class="p">,</span> <span class="n">length</span><span class="o">=</span><span class="mi">30</span><span class="p">)</span>
    <span class="n">x</span> <span class="o">=</span> <span class="n">transaction</span><span class="o">.</span><span class="n">add_text_clip</span><span class="p">(</span><span class="s2">&quot;world&quot;</span><span class="p">,</span> <span class="n">length</span><span class="o">=</span><span class="mi">35</span><span class="p">)</span>
    <span class="n">_</span> <span class="o">=</span> <span class="n">transaction</span><span class="o">.</span><span class="n">add_text_clip</span><span class="p">(</span><span class="s2">&quot;end&quot;</span><span class="p">,</span> <span class="n">length</span><span class="o">=</span><span class="mi">20</span><span class="p">)</span>
    <span class="n">_</span> <span class="o">=</span> <span class="n">transaction</span><span class="o">.</span><span class="n">add_text_clip</span><span class="p">(</span><span class="s2">&quot;end&quot;</span><span class="p">,</span> <span class="n">length</span><span class="o">=</span><span class="mi">20</span><span class="p">)</span>
    <span class="n">transaction</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="k">lambda</span> <span class="n">cut</span><span class="p">:</span> <span class="n">cut</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="o">-</span><span class="mi">10</span><span class="p">))</span>
</pre></div>
</div></div>
In that case a commit/rollback is guaranteed.

However, when dealing with mouse events, we can not use the context manager and
instead have to deal with mouse events.

The new check that prevents multiple transactions ensures that everything stops
working if we forget to close a transaction.

But I would like to come up with a nicer pattern for ensuring that transactions
close.

I'll add a TODO for it and maybe we can come up with a nicer solution later.

## Further cleanup

In order to satisfy Python's import mechanism, I put `Action` and `MenuItem` in
the `rlvideolib.domain.cut` module.

They obviously don't belong there.

Here is what the gui package looks like now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>rlvideolib/gui/
├── generic.py
├── gtk.py
├── __init__.py
└── testing.py
</pre></div>
</div></div>
Previously `Action` and `MenuItem` were defined in `generic`. That makes sense.
But now we have a dependency on them from `rlvideolib.domain.cut`. Should a
domain object depend on GUI? Maybe that is ok.

I think what I'll do is create another module inside the gui package called
`framework`. It will contain generic GUI elements that do not depend on GTK or
our application.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Move generic framework GUI code to new rlvideolib.gui.framework.&#39;
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.393s

OK
[main b1a8f5d] Move generic framework GUI code to new rlvideolib.gui.framework.
 5 files changed, 23 insertions(+), 19 deletions(-)
 create mode 100644 rlvideolib/gui/framework.py
</pre></div>
</div></div>
## Progress?

Back to this code where we started:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># TODO: add callback mechanism in rectangle map</span>
<span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">w</span><span class="p">,</span> <span class="n">h</span> <span class="o">=</span> <span class="p">(</span>
    <span class="n">area</span><span class="o">.</span><span class="n">x</span><span class="o">+</span><span class="n">x_start</span><span class="p">,</span>
    <span class="n">area</span><span class="o">.</span><span class="n">y</span><span class="p">,</span>
    <span class="n">x_end</span><span class="o">-</span><span class="n">x_start</span><span class="p">,</span>
    <span class="n">area</span><span class="o">.</span><span class="n">height</span>
<span class="p">)</span>
<span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
<span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
<span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
    <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
    <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
    <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
    <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
<span class="p">),</span> <span class="n">ScrollbarDragAction</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">scrollbar</span><span class="p">))</span>
</pre></div>
</div></div>
Ah, the TODO is actually done now.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Remove completed TODO about callback mechanism for rectangle map.&#39;
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.392s

OK
[main b757e3a] Remove completed TODO about callback mechanism for rectangle map.
 1 file changed, 1 deletion(-)
</pre></div>
</div></div>
We still haven't made any progress on the exception problem though. But we have
fixed design issues in related areas.

Let's focus again on the exception.

## A common pattern

I think the following pattern exists in all places where we add actions to the
rectangle map:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
<span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
<span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
    <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
    <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
    <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
    <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
<span class="p">),</span> <span class="o">...</span><span class="p">)</span>
</pre></div>
</div></div>
What about if we add a method to `RectangleMap` like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">add_from_context</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">,</span> <span class="n">context</span><span class="p">,</span> <span class="n">item</span><span class="p">):</span>
    <span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
        <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
        <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
        <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
        <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
    <span class="p">),</span> <span class="n">item</span><span class="p">)</span>
</pre></div>
</div></div>
We can use that method to add both the scroll action and the scrub action.

However, the cut action looks slightly different:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">rectangle</span><span class="o">.</span><span class="n">x</span><span class="p">,</span> <span class="n">rectangle</span><span class="o">.</span><span class="n">y</span><span class="p">)</span>
<span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">rectangle</span><span class="o">.</span><span class="n">width</span><span class="p">,</span> <span class="n">rectangle</span><span class="o">.</span><span class="n">height</span><span class="p">)</span>
<span class="k">if</span> <span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">)</span> <span class="o">&gt;</span> <span class="mi">0</span> <span class="ow">and</span> <span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span> <span class="o">&gt;</span> <span class="mi">0</span><span class="p">:</span>
    <span class="n">rectangle_map</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Rectangle</span><span class="p">(</span>
        <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
        <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
        <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
        <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
    <span class="p">),</span> <span class="n">CutAction</span><span class="p">(</span><span class="n">project</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_source_cut</span><span class="p">(),</span> <span class="n">scrollbar</span><span class="p">))</span>
</pre></div>
</div></div>
It actually has the check that we also need for the scrollbar. That is, we only
add the rectangle to the map if it has a width and height.

Let's add those checks to `add_from_context`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">add_from_context</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">,</span> <span class="n">context</span><span class="p">,</span> <span class="n">item</span><span class="p">):</span>
    <span class="n">rect_x</span><span class="p">,</span> <span class="n">rect_y</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="n">rect_w</span><span class="p">,</span> <span class="n">rect_h</span> <span class="o">=</span> <span class="n">context</span><span class="o">.</span><span class="n">user_to_device_distance</span><span class="p">(</span><span class="n">w</span><span class="p">,</span> <span class="n">h</span><span class="p">)</span>
    <span class="k">if</span> <span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">)</span> <span class="o">&gt;</span> <span class="mi">0</span> <span class="ow">and</span> <span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span> <span class="o">&gt;</span> <span class="mi">0</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span>
            <span class="n">Rectangle</span><span class="p">(</span>
                <span class="n">x</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_x</span><span class="p">),</span>
                <span class="n">y</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_y</span><span class="p">),</span>
                <span class="n">width</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_w</span><span class="p">),</span>
                <span class="n">height</span><span class="o">=</span><span class="nb">int</span><span class="p">(</span><span class="n">rect_h</span><span class="p">)</span>
            <span class="p">),</span>
            <span class="n">item</span>
        <span class="p">)</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Extract RectangleMap.add_from_context which does width/height checks.&#39;
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.498s

OK
[main cd38e3e] Extract RectangleMap.add_from_context which does width/height checks.
 3 files changed, 17 insertions(+), 25 deletions(-)
</pre></div>
</div></div>
And this actually resolves the exception problem when I open my project.

# Summary

I don't have much experience doing evolutionary design. My feeling right now is
that I need to spend much more time designing than what I am currently doing. I
feel like I need to do at least 60% designing and only 40% adding new
features. If you are reading this and have any experience with evolutionary
design, feel free to share it with me. I should probably also re-read the
chapters in James' book to refresh my memory.
