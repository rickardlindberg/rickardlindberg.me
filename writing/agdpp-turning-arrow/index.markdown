---
title: 'DRAFT: Turning arrow'
date: 2023-05-09
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

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
One change here is that we also clone the arrows position attribute. The
position of the arrow is always the same. Only when we shoot it it changes. But
should we choose to move the arrow to a different start position, the code now takes
that into account and places shooting arrows at the right start positions.

I think this is still a pure refactoring.  There is no change in visible
behavior, but the code is more robust because we can now change the start
position of the arrow, and it will shoot from the right position without we
having to modify any other piece of code. The design is better.

## Work towards arrow velocity

## Derive velocity from angle

## Base drawing on angle

## Changing angle with left/right

## Result

<center>
![Turning arrow.](turning-arrow.png)
</center>

## Summary

Full source code from this episode on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/turning-arrow).

See you in the next episode!

# TODO

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>
    commit b08065975c2233f27916a049c75529d1be3295c5
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Thu Apr 27 07:03:44 2023 +0200

        Add Arrow.clone_shooting.

    ...

    commit e2f869113c37b91086888d262c22dd4376d47395
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Fri Apr 28 06:51:48 2023 +0200

        Arrow angle can be changed with keys.
