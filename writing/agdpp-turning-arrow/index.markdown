---
title: Turning arrow
date: 2023-05-12
tags: agdpp
agdpp: true
---

We have a basic version of a balloon shooter in place. We have a balloon moving
across the screen and an arrow that can be shot and hit the balloon to score a
point.

<center>
![Arrow shooting straight.](points.png)
</center>

When I play the game now, I want to turn the arrow. I think that will make the
game a little more fun. Then you have to control both angle and timing instead
of just timing to hit a balloon. (If we implement that balloons fall downwards
instead, turning will also be necessary to hit balloons that are not straight
above the arrow.)

## How does arrow movement work?

Arrows move first when we press the space key to shoot. Then we create a new
arrow and set its shooting attribute to true:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Arrow</span><span class="p">(</span><span class="n">shooting</span><span class="o">=</span><span class="kc">True</span><span class="p">))</span>
</pre></div>
</div></div>
An arrow that has the shooting attribute set to true moves like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">shooting</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="n">dy</span><span class="o">=-</span><span class="n">dt</span><span class="p">)</span>
</pre></div>
</div></div>
That is, it moves straight up in increments of `dt`. There is no concept of
direction yet.

What we would like to have is some kind of direction attribute on the arrow
that we can change. When we shoot, the arrow that we create should get that
direction attribute so that it flies in the same direction that we aimed.

Let's see if we can refactor towards that.

## Clone shooting

We start by creating a method `clone_shooting` on the arrow that should return
a copy of itself (including all attributes) and have the shooting attribute set
to true:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">clone_shooting</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">Arrow</span><span class="p">(</span><span class="n">shooting</span><span class="o">=</span><span class="kc">True</span><span class="p">,</span> <span class="n">position</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">)</span>
</pre></div>
</div></div>
We modify how a flying arrow is added like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">- self.flying_arrows.add(Arrow(shooting=True))</span>
<span class="gi">+ self.flying_arrows.add(self.arrow.clone_shooting())</span>
</pre></div>
</div></div>
One change here is that we also clone the arrow's position attribute. The
position of the arrow is always the same. Only when we shoot it, it changes.
But should we choose to move the arrow to a different start position, the code
now takes that into account and places shooting arrows at the right start
positions.

I think this is still a pure refactoring.  There is no change in visible
behavior, but the code is more robust because we can now change the start
position of the arrow, and it will shoot from the right position without we
having to modify any other piece of code. The design is better.

## Work towards arrow velocity

Next we take a small step towards having an arrow velocity. We change the
update method to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">shooting</span><span class="p">:</span>
            <span class="n">velocity</span> <span class="o">=</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">0</span><span class="p">,</span> <span class="n">y</span><span class="o">=-</span><span class="n">dt</span><span class="p">)</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">velocity</span><span class="p">)</span>
</pre></div>
</div></div>
Our relatively new `Point` class attracts more and more behavior. Here we added
the `add` method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Point</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">add</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">point</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; Point(0, 5).add(Point(1, 1))</span>
<span class="sd">        Point(1, 6)</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="n">dx</span><span class="o">=</span><span class="n">point</span><span class="o">.</span><span class="n">x</span><span class="p">,</span> <span class="n">dy</span><span class="o">=</span><span class="n">point</span><span class="o">.</span><span class="n">y</span><span class="p">)</span>
</pre></div>
</div></div>
## Derive velocity from angle

Now we could modify the velocity of the arrow and the update method would move
it in the right direction.

However, I think it is better if we have a concept of an arrow angle that we
can adjust left and right. That would fit our use case better.

We add and angle attribute to the arrow and derive the velocity vector from it:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">shooting</span><span class="o">=</span><span class="kc">False</span><span class="p">,</span> <span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">500</span><span class="p">)):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">angle</span> <span class="o">=</span> <span class="o">-</span><span class="mi">90</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">shooting</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Point</span><span class="o">.</span><span class="n">from_angle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">angle</span><span class="p">)</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="n">dt</span><span class="p">))</span>

    <span class="o">...</span>
</pre></div>
</div></div>
The `Point` class again attracts functionality. This time for converting angles
to unit vectors (vectors of length one) and for magnifying vectors:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Point</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">from_angle</span><span class="p">(</span><span class="n">degrees</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; p = Point.from_angle(-90)</span>
<span class="sd">        &gt;&gt;&gt; int(p.x)</span>
<span class="sd">        0</span>
<span class="sd">        &gt;&gt;&gt; int(p.y)</span>
<span class="sd">        -1</span>

<span class="sd">        &gt;&gt;&gt; p = Point.from_angle(0)</span>
<span class="sd">        &gt;&gt;&gt; int(p.x)</span>
<span class="sd">        1</span>
<span class="sd">        &gt;&gt;&gt; int(p.y)</span>
<span class="sd">        0</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="n">Point</span><span class="p">(</span>
            <span class="n">x</span><span class="o">=</span><span class="n">math</span><span class="o">.</span><span class="n">cos</span><span class="p">(</span><span class="n">math</span><span class="o">.</span><span class="n">radians</span><span class="p">(</span><span class="n">degrees</span><span class="p">)),</span>
            <span class="n">y</span><span class="o">=</span><span class="n">math</span><span class="o">.</span><span class="n">sin</span><span class="p">(</span><span class="n">math</span><span class="o">.</span><span class="n">radians</span><span class="p">(</span><span class="n">degrees</span><span class="p">))</span>
        <span class="p">)</span>

    <span class="k">def</span> <span class="nf">times</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">magnification</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; Point(1, 5).times(2)</span>
<span class="sd">        Point(2, 10)</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="o">*</span><span class="n">magnification</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="o">*</span><span class="n">magnification</span><span class="p">)</span>
</pre></div>
</div></div>
## Base drawing on angle

The arrow now moves correctly based on the angle, but it doesn't draw its three
circles correctly. It looks like this now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">10</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="n">dy</span><span class="o">=</span><span class="mi">20</span><span class="p">),</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">15</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="n">dy</span><span class="o">=</span><span class="mi">40</span><span class="p">),</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">)</span>
</pre></div>
</div></div>
That is, it draws the second two circles by moving them downwards, assuming
that the arrow is pointing up.

Let's instead draw them offset by the opposite direction of what the arrow
points:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">v</span> <span class="o">=</span> <span class="n">Point</span><span class="o">.</span><span class="n">from_angle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">angle</span> <span class="o">+</span> <span class="mi">180</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">10</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">v</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="mi">20</span><span class="p">)),</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">15</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">v</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="mi">40</span><span class="p">)),</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">)</span>
</pre></div>
</div></div>
We get the reverse angle by turning it 180 degrees.

Perhaps angle is another case of primitive obsession. If we had an angle class,
we could have a `reverse` method that did this and we would no longer be
required to know about degrees (the angle could be implemented with radians
instead for example). We make a note about that.

Anyway, we can change the angle attribute of the arrow and it will fly in the
right direction and draw correctly. Now there is only one thing left: control
the angle with arrow keys.

## Changing angle with left/right

Here is the test we write for changing arrow angle:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(space)</span>
<span class="sd">&gt;&gt;&gt; game.get_arrow_angle()</span>
<span class="sd">-90</span>
<span class="sd">&gt;&gt;&gt; game.event(GameLoop.create_event_keydown_left())</span>
<span class="sd">&gt;&gt;&gt; game.get_arrow_angle()</span>
<span class="sd">-95</span>
<span class="sd">&gt;&gt;&gt; game.event(GameLoop.create_event_keydown_right())</span>
<span class="sd">&gt;&gt;&gt; game.get_arrow_angle()</span>
<span class="sd">-90</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
For this to work we need to create new event wrappers for keydown left/right
and add a getter to expose the arrow angle. We have done similar things before.
Same procedure this time.

We make it pass by handling the events and changing the angle:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_left</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">angle_left</span><span class="p">()</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_right</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">angle_right</span><span class="p">()</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">angle_left</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">angle</span> <span class="o">-=</span> <span class="mi">5</span>

    <span class="k">def</span> <span class="nf">angle_right</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">angle</span> <span class="o">+=</span> <span class="mi">5</span>
</pre></div>
</div></div>
This almost works, but when we turn and arrow and shoot it, it still goes
straight up. We need to fix the `clone_shooting` method to also clone the
angle.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">clone_shooting</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        It preserves position and angle and set it to shooting:</span>

<span class="sd">        &gt;&gt;&gt; arrow = Arrow(position=Point(x=5, y=5), angle=-45)</span>
<span class="sd">        &gt;&gt;&gt; new_arrow = arrow.clone_shooting()</span>
<span class="sd">        &gt;&gt;&gt; new_arrow.get_position()</span>
<span class="sd">        (5, 5)</span>
<span class="sd">        &gt;&gt;&gt; new_arrow.angle</span>
<span class="sd">        -45</span>
<span class="sd">        &gt;&gt;&gt; new_arrow.shooting</span>
<span class="sd">        True</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="n">Arrow</span><span class="p">(</span><span class="n">shooting</span><span class="o">=</span><span class="kc">True</span><span class="p">,</span> <span class="n">position</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">,</span> <span class="n">angle</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">angle</span><span class="p">)</span>
</pre></div>
</div></div>
## Result

Now we can turn the arrow with left/right keys and shoot it in different
directions. It looks like this:

<center>
![Turning arrow.](turning-arrow.png)
</center>

If you want to try it out, the full source code from this episode on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/turning-arrow).

## Summary

Testing continues to go smooth with state based testing and getters to expose
internal state.

What I like to do after implementing a feature is to take a break and then come
back later to review to code for possible improvements. Often times it is small
things like renaming a variable to make it more clear. In this episode we also
noted that angle might benefit being wrapped in an abstraction. Not sure they
are too interesting to write about. Let me know if you think otherwise.

See you in the next episode!
