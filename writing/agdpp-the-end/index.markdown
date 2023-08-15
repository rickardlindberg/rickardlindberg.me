---
title: 'DRAFT: The end?'
date: 2023-08-15
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

When I started this series, my intention was to document my journey of creating
a game using agile methods.

I think I have mostly succeeded in this regard, but at the moment I've done
some development that I have not documented. Furthermore, I did that
development many months ago, so documenting it gets harder and harder because I
forget what I was thinking when I did the development.

Recently though, I've experimented with a new format which I call
[DevLog](/tags/devlog/index.html). It is basically the same thing but a little
less polished. I write a DevLog while doing the development, so there is no
risk of falling behind.

In this post I will briefly mention the development that I've done but not
documented and then talk a little about future plans for this project.

## Particle effects

I polish the game a little by adding a particle effect system that I use to
render a splashing animation when a balloon is shot.

It looks a little something like this (although it is hard to show in a single
image):

<p>
<center>
![Particles when shooting a balloon.](particles.png)
</center>
</p>

The most interesting piece of code is this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_hit_particles</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">number_of_particles</span> <span class="o">=</span> <span class="n">random</span><span class="o">.</span><span class="n">randint</span><span class="p">(</span><span class="mi">4</span><span class="p">,</span> <span class="mi">8</span><span class="p">)</span>
        <span class="k">return</span> <span class="p">[</span>
            <span class="n">BalloonParticle</span><span class="p">(</span>
                <span class="n">position</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">move</span><span class="p">(</span>
                    <span class="n">dx</span><span class="o">=</span><span class="n">random</span><span class="o">.</span><span class="n">randint</span><span class="p">(</span><span class="mi">0</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">radius</span><span class="p">),</span>
                    <span class="n">dy</span><span class="o">=</span><span class="n">random</span><span class="o">.</span><span class="n">randint</span><span class="p">(</span><span class="mi">0</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">radius</span><span class="p">)</span>
                <span class="p">),</span>
                <span class="n">radius</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">radius</span><span class="o">*</span><span class="p">(</span><span class="n">random</span><span class="o">.</span><span class="n">randint</span><span class="p">(</span><span class="mi">30</span><span class="p">,</span> <span class="mi">70</span><span class="p">)</span><span class="o">/</span><span class="mi">100</span><span class="p">),</span>
                <span class="n">velocity</span><span class="o">=</span><span class="n">Angle</span><span class="o">.</span><span class="n">fraction_of_whole</span><span class="p">(</span><span class="n">random</span><span class="o">.</span><span class="n">random</span><span class="p">())</span><span class="o">.</span><span class="n">to_unit_point</span><span class="p">()</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">speed</span><span class="o">*</span><span class="mi">2</span><span class="p">)</span>
            <span class="p">)</span>
            <span class="k">for</span> <span class="n">x</span>
            <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="n">number_of_particles</span><span class="p">)</span>
        <span class="p">]</span>
</pre></div>
</div></div>
It generates a list of particles when a balloon is hit. The particles have a
little randomized position, radius, and velocity.

The radius keeps decreasing as time passes, and when it reaches a low enough
value, the particle is removed.

The complete diff for this change can be seen on
[GitHub](https://github.com/rickardlindberg/agdpp/compare/b5261a939505c203cd1ffb21462a6772f0381faf...7533ec079dbdeba713526469535a1cc0fc915449).

## Sound effects

Me and my son record sound effects that are played when a balloon is hit.  We
go to the store, buy some balloons, rig the mic, and pop them.  It is much
fun.

The code for integrating the sound can be seen on
[GitHub](https://github.com/rickardlindberg/agdpp/compare/7533ec079dbdeba713526469535a1cc0fc915449...fcb1757f9b219be55d65d8588c259b96b9dc26ce).

This change include adding the `load_sound` method to `GameLoop`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">load_sound</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">Sound</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">mixer</span><span class="o">.</span><span class="n">Sound</span><span class="p">(</span><span class="n">path</span><span class="p">))</span>
</pre></div>
</div></div>
Does it really make sense that you load a sound from the game loop? I'm not
sure. The game loop is the only abstraction that we have for accessing pygame.
That's why it ended up there. But the design here feels a little off to me.
Something to keep an eye on in the future.

## Medals

When I ask my son what he wants to work on next, he said that he wants to
get a medal for every 100 balloon that you shoot down.

I add a fun little particle effect again for the animation when you get a
medal:

<p>
<center>
![Medal animation.](medal1.png)
</center>
</p>

The medals stack up in the upper left corner like this:

<p>
<center>
![Medals stacking up.](medal2.png)
</center>
</p>

[GitHub](https://github.com/rickardlindberg/agdpp/compare/fcb1757f9b219be55d65d8588c259b96b9dc26ce...0c8e713a6d938898ddb92164cc86dcb1db19aa0c).

## Test scene

Testing the medal particle effect is tedious. You have to shoot down 100
balloons, then you can see the effect for a split second, then you have to
shoot down 100 more.

When I have done that enough times, I come up with a better idea. And that is
to allow the game to be started in "test mode" where we can trigger the
animation with a press of a button.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py rundev test-scene-score
</pre></div>
</div></div>
<p>
<center>
![Test scene.](test-scene.png)
</center>
</p>

## Next: double dispatch, decentralized?

## Summary

This post probably marks the end of this series as we know it.  When I continue
this project, it will be in the form of a [DevLog](/tags/devlog/index.html).
See you there!
