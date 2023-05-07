---
title: 'DRAFT: Arrow can hit balloon'
date: 2023-05-07
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

We have two stories left before we think we have a first version of a balloon
shooter game:

* Arrow can hit balloon
* Point is given for hit

In this episode we will work on making an arrow hit a balloon.

## Acceptance criteria

What do we mean by arrow can hit balloon?

* balloon disappears after hit?

## The test

Our game scene object currently inits like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">space</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Balloon</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Arrow</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">space</span> <span class="o">=</span> <span class="n">space</span>

    <span class="o">...</span>
</pre></div>
</div></div>
We would like to write a test where we shoot an arrow, make it collide with the
balloon, and then assert that the balloon disappears.

With the current design, this is really difficult to do. It can only be done
something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(...)</span>
<span class="sd">&gt;&gt;&gt; game.event(GameLoop.create_event_keydown_space())</span>
<span class="sd">&gt;&gt;&gt; game.update(??)</span>
<span class="sd">&gt;&gt;&gt; game.get_balloon() is None</span>
<span class="sd">True</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
But this is really flaky and hard to understand. In order for this to work, we
have to time the shooting and the updating so that the arrow actually hits the
balloon. Even if we get it to work, it will start failing if we for example
change the speed of the arrow. And this test should really not care about arrow
speed.

Let's see if we can do better.

We change the init method to this instead:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">space</span><span class="p">,</span> <span class="n">balloons</span><span class="o">=</span><span class="p">[(</span><span class="mi">50</span><span class="p">,</span> <span class="mi">50</span><span class="p">)],</span> <span class="n">arrows</span><span class="o">=</span><span class="p">[]):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">([</span>
            <span class="n">Balloon</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">)</span> <span class="k">for</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span> <span class="ow">in</span> <span class="n">balloons</span>
        <span class="p">]))</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Arrow</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">([</span>
            <span class="n">Arrow</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">)</span> <span class="k">for</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span> <span class="ow">in</span> <span class="n">arrows</span>
        <span class="p">]))</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">space</span> <span class="o">=</span> <span class="n">space</span>

    <span class="o">...</span>
</pre></div>
</div></div>
That is, we make it possible the create a game scene object where we specify
where all the balloons should be and where all the flying arrows should be. We
also change the balloon from a single object to a sprite group. This is not
strictly necessary, but it will make removing hit balloons easier. The default
values for the balloons and arrows mimics the current default. We have one
balloon that starts at (50, 50) and zero flying arrows.

Here is the test that checks initial state:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(space, balloons=[(100, 100)], arrows=[(500, 500)])</span>
<span class="sd">&gt;&gt;&gt; len(game.get_balloons())</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; len(game.get_flying_arrows())</span>
<span class="sd">1</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
In order for it to work, we expose another getter for the balloon sprites:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">get_balloons</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">()</span>
</pre></div>
</div></div>
The test continues to check that we still have one balloon and one flying arrow
after an update:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game.update(0)</span>
<span class="sd">&gt;&gt;&gt; len(game.get_balloons())</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; len(game.get_flying_arrows())</span>
<span class="sd">1</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We update with 0 to ensure that nothing moves. We need to call update to make
the collision detection code run, but to ensure exact positions, we pass 0 as
the delta time. All movements should take the delta time into account, so 0
should result in no movement.

We continue and write the test for hitting a balloon like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(space, balloons=[(500, 500)], arrows=[(500, 500)])</span>
<span class="sd">&gt;&gt;&gt; len(game.get_balloons())</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; game.update(0)</span>
<span class="sd">&gt;&gt;&gt; game.get_balloons()</span>
<span class="sd">[]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We place the arrow at the center of the balloon, invoke the collision detection
code with update, and assert that there are no longer any balloons.

## Implementing arrow hit

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">for</span> <span class="n">arrow</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
            <span class="o">...</span>
            <span class="k">for</span> <span class="n">balloon</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
                <span class="k">if</span> <span class="n">arrow</span><span class="o">.</span><span class="n">hits_baloon</span><span class="p">(</span><span class="n">balloon</span><span class="p">):</span>
                    <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">balloon</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">hits_baloon</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">balloon</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">balloon</span><span class="o">.</span><span class="n">inside</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">inside</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">return</span> <span class="p">(</span><span class="n">x</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span><span class="o">**</span><span class="mi">2</span><span class="o">+</span><span class="p">(</span><span class="n">y</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="p">)</span><span class="o">**</span><span class="mi">2</span> <span class="o">&lt;=</span> <span class="bp">self</span><span class="o">.</span><span class="n">radius</span><span class="o">**</span><span class="mi">2</span>

    <span class="o">...</span>
</pre></div>
</div></div>
## Demo trick

The game works and if we manage to hit a balloon, it disappears. Again, bummer.
We can shoot infinitely many arrows, but if there is no more balloons to hit,
the game is not that interesting.

We had a situation like this [before](/writing/agdpp-shooting-arrow/index.html)
where you shot the arrow and you could only get a new one by restarting the
game.

One trick I used when I demoed this for this customer was to run the game in a
loop like this:

    $ while true; do ./zero.py rundev; done

So when you have no more arrow to shoot or no more balloons to hit, you close
the game window and a new one will immediately pop up.

That way, it is a little smoother to gather feedback on the current game
functionality.

We fixed so that you get more arrows to shoot, let's also fix so that a new
balloon is spawned after one is hit so we don't need to restart the game in a
loop anymore.

## Primitive obsession refactoring

Before we start adding new functionality, let's have a look at the code and see
if there is anything that we can improve to make it more clear and make the
future a little smoother.

One thing I notice is that we are not passing around (x, y) coordinates in a
lot of places, and objects keep track of the x and y coordinates. Here is the
balloon class for example:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">40</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="n">x</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">y</span> <span class="o">=</span> <span class="n">y</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">radius</span> <span class="o">=</span> <span class="n">radius</span>

    <span class="k">def</span> <span class="nf">inside</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="k">return</span> <span class="p">(</span><span class="n">x</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span><span class="o">**</span><span class="mi">2</span><span class="o">+</span><span class="p">(</span><span class="n">y</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="p">)</span><span class="o">**</span><span class="mi">2</span> <span class="o">&lt;=</span> <span class="bp">self</span><span class="o">.</span><span class="n">radius</span><span class="o">**</span><span class="mi">2</span>

    <span class="o">...</span>
</pre></div>
</div></div>
This smell is called primitive obsession. It is when you pass around primitive
objects (integers, strings encoding information, etc) instead of an
abstraction. That leads to lots of duplicated logic. Say for example that we
want to move an object, we might do something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">+=</span> <span class="mi">1</span>
<span class="bp">self</span><span class="o">.</span><span class="n">y</span> <span class="o">+=</span> <span class="mi">2</span>
</pre></div>
</div></div>
And we probably need to move multiple objects, so this kind of code will be
duplicated in many places.

The solution is to create and abstraction for the concept. In this case, I
choose to call it point:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Point</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="n">x</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">y</span> <span class="o">=</span> <span class="n">y</span>
</pre></div>
</div></div>
We refactor in small tiny steps to make use of this point.

Eventually, the inside check in the balloon looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">inside</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">position</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">distance_to</span><span class="p">(</span><span class="n">position</span><span class="p">)</span> <span class="o">&lt;=</span> <span class="bp">self</span><span class="o">.</span><span class="n">radius</span>

    <span class="o">...</span>
</pre></div>
</div></div>
We are no longer dealing with separate x and y coordinates. We are dealing with
positions.

A big chunk of the hit test has also move into the new point class:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Point</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">distance_to</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">point</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">math</span><span class="o">.</span><span class="n">sqrt</span><span class="p">((</span><span class="n">point</span><span class="o">.</span><span class="n">x</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span><span class="o">**</span><span class="mi">2</span><span class="o">+</span><span class="p">(</span><span class="n">point</span><span class="o">.</span><span class="n">y</span><span class="o">-</span><span class="bp">self</span><span class="o">.</span><span class="n">y</span><span class="p">)</span><span class="o">**</span><span class="mi">2</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
What usually happens when you extract a concept like the point is that it
starts attracting new functionality. Suddenly, there is a logical place to
implement something instead of spreading it across the code base.

Another benefit of this abstraction is that we can now more easily test the
behavior of `distance_to` in isolation. No need to involve balloons and arrows.

## Spawning new balloons

So it's no fun the play the game after you hit the balloon, because then there
are no more balloons to hit. We want to spawn a new balloon.

We need to modify our test. It looks like this now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(space, balloons=[(500, 500)], arrows=[(500, 500)])</span>
<span class="sd">&gt;&gt;&gt; len(game.get_balloons())</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; game.update(0)</span>
<span class="sd">&gt;&gt;&gt; game.get_balloons()</span>
<span class="sd">[]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We don't want the balloon list to be empty. We still want it to contain a
balloon. But not the balloon that we just shot down, another one.

I think we can do it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(space, balloons=[(500, 500)], arrows=[(500, 500)])</span>
<span class="sd">&gt;&gt;&gt; balloons = game.get_balloons()</span>
<span class="sd">&gt;&gt;&gt; len(balloons)</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; game.update(0)</span>
<span class="sd">&gt;&gt;&gt; new_balloons = game.get_balloons()</span>
<span class="sd">&gt;&gt;&gt; len(new_balloons)</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; new_balloons == balloons</span>
<span class="sd">False</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The fix is easy, just add another balloon after the one that has been shot down
has been removed:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

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
This now works, but it is a little hard to actually notice that we hit a
balloon. It should be more clear if we include a score.

## TODO: add score?

## Summary

The state based testing approach continues to work well. Tests are easy to
write, and I don't think the getters that we add to expose internal state are
too problematic.

In the next episode we will look at how to add a score, after which I think we
have the first version of a balloon shooter ready. See you then!
