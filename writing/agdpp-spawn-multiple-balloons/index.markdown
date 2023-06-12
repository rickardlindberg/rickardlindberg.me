---
title: 'DRAFT: Spawn multiple balloons'
date: 2023-06-12
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

We [previously](/writing/agdpp-game-over/index.html) had a story about balloons
moving downwards. We scratched that because other stories were more important
for the first version of the balloon shooter. With those stories done, I think
a more realistic balloon spawning and movement pattern is the most valuable
think we can work on.

## Video version

The video version of this episode:

    TODO

## Code review

Let's review our code and look at how balloons are managed.

Our game scene has a sprite group for balloons which by default only contains
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

## Stories

...

## Summary

See you in the next episode!

## TODO

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>
<p>
<center>
![Timeline halt GUI.](timeline-halt-gui.png)
</center>
</p>

    * refactor to make the change easy

    commit b0a34cb7e542aeff0a49a146576986390f1d7d1f
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 08:41:34 2023 +0200

        Extract Balloons.

    ...

    commit c6f3debba566e6cf1f69d300077d8a16483efc82
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 09:24:55 2023 +0200

        Spawn multiple balloons and remove the ones outside the screen.

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

