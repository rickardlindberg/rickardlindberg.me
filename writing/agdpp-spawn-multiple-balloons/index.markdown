---
title: 'DRAFT: Spawn multiple balloons'
date: 2023-06-15
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

We [previously](/writing/agdpp-game-over/index.html) had a story about balloons
moving downwards. We scratched that because other stories were more important
for the first version of the balloon shooter. With those stories done, I think
a more realistic balloon spawning and movement pattern is the most valuable
thing we can work on.

## Video version

The video version of this episode:

    TODO

## Code review

Let's review our code and look at how balloons are managed.

Our game scene has a sprite group for balloons which by default contains only
one balloon:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">balloons</span><span class="o">=</span><span class="p">[(</span><span class="mi">50</span><span class="p">,</span> <span class="mi">50</span><span class="p">)],</span> <span class="o">...</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">([</span>
            <span class="n">Balloon</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">))</span> <span class="k">for</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span> <span class="ow">in</span> <span class="n">balloons</span>
        <span class="p">]))</span>
</pre></div>
</div></div>
This sprite group is modified in the `update` method if an arrows hits a
balloon:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">for</span> <span class="n">arrow</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
            <span class="o">...</span>
            <span class="k">for</span> <span class="n">balloon</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
                <span class="k">if</span> <span class="n">arrow</span><span class="o">.</span><span class="n">hits_baloon</span><span class="p">(</span><span class="n">balloon</span><span class="p">):</span>
                    <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">balloon</span><span class="p">)</span>
                    <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Balloon</span><span class="p">(</span><span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">50</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">50</span><span class="p">)))</span>
                    <span class="o">...</span>
</pre></div>
</div></div>
So the hit balloon is removed, and a new one is added.

How do balloons move?

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">position</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">40</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="n">position</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">radius</span> <span class="o">=</span> <span class="n">radius</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">x</span> <span class="o">&gt;</span> <span class="mi">1200</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">50</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="n">dx</span><span class="o">=</span><span class="n">dt</span><span class="o">*</span><span class="mf">0.3</span><span class="p">)</span>
</pre></div>
</div></div>
They move from left to right and wrap around at x=1200.

## Strategy

To be able to write more isolated tests for balloon behavior, I want to start
with a few refactorings. I want to extract a `Balloons` class which contains
most logic related to balloons. Then I want to write tests for new behavior.
This is also known as make the change easy, then make the easy change.

We begin by creating the class and using it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-        self.balloons = self.add(SpriteGroup([</span>
<span class="gd">-            Balloon(Point(x=x, y=y)) for (x, y) in balloons</span>
<span class="gd">-        ]))</span>
<span class="gi">+        self.balloons = self.add(Balloons(balloons))</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">positions</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="p">[</span>
            <span class="n">Balloon</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">))</span> <span class="k">for</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span> <span class="ow">in</span> <span class="n">positions</span>
        <span class="p">])</span>
</pre></div>
</div></div>
We continue to move some behavior into this new class:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_balloon_hit_by_arrow</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">arrow</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">balloon</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
            <span class="k">if</span> <span class="n">arrow</span><span class="o">.</span><span class="n">hits_baloon</span><span class="p">(</span><span class="n">balloon</span><span class="p">):</span>
                <span class="k">return</span> <span class="n">balloon</span>

    <span class="k">def</span> <span class="nf">spawn_new</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Balloon</span><span class="p">(</span><span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">50</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">50</span><span class="p">)))</span>
</pre></div>
</div></div>
With that in place, we can simplify the update code to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-            for balloon in self.balloons.get_sprites():</span>
<span class="gd">-                if arrow.hits_baloon(balloon):</span>
<span class="gd">-                    self.balloons.remove(balloon)</span>
<span class="gd">-                    self.balloons.add(Balloon(position=Point(x=50, y=50)))</span>
<span class="gd">-                    self.score.add(1)</span>
<span class="gi">+            hit_balloon = self.balloons.get_balloon_hit_by_arrow(arrow)</span>
<span class="gi">+            if hit_balloon:</span>
<span class="gi">+                self.balloons.remove(hit_balloon)</span>
<span class="gi">+                self.balloons.spawn_new()</span>
<span class="gi">+                self.score.add(1)</span>
</pre></div>
</div></div>
There is probably some more functionality that we can move into the new
balloons class, but let's stop here for now and focus on the new behavior.

(If you want to see this refactoring happening in smaller steps in real time,
check out the video version.)

## Stories

Here is some new behavior that we would like to have:

* balloons move downwards
* balloons appear at different x positions
* multiple balloons are in the air at the same time

If we move them into a test, it looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    Balloons move downwards (move to Balloon?):</span>

<span class="sd">    ...</span>

<span class="sd">    It spawns up to 3 balloons:</span>

<span class="sd">    ...</span>

<span class="sd">    Balloons outside the screen is removed:</span>

<span class="sd">    ...</span>
<span class="sd">    &quot;&quot;&quot;</span>
</pre></div>
</div></div>
<p>
<center>
![Multiple balloons.](multiple-balloons.png)
</center>
</p>

## Break and cleanup

    * then hammock + refactoring cleanup
        * cleaning up feels so good
        * get feature out fast, the cover up the imperfections

    commit 782cda7032896b15d89058b0fe3bc4ccbb54da8c
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 10:12:49 2023 +0200

        Replace (x, y) with position in Arrow and Balloon.

    ...

    commit 18a9a5af49966f9b2c7e8841495687181e7fedfb
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 14:19:21 2023 +0200

        Clean up shooting arrow tests.

## Summary

See you in the next episode!
