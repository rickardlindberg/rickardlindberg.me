---
title: 'DRAFT: Score as text'
date: 2023-05-23
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

Currently, the game keeps track of the score by drawing yellow point markers.
One for each point:

<p>
<center>
![Point markers.](points.png)
</center>
</p>

I find it tedious to count them when my son wants to know the score, and I
think he also expressed that he wanted to have the score as a number in the
upper right corner.

That's what we will work on in this episode. The end result will look like
this:

<p>
<center>
![Score text.](score-text.png)
</center>
</p>

## Drawing text

The reason that we implemented score display with point markers was that it was
quicker. There was no way to draw text and it was therefore quicker to use
point markers. But that has to change now. Let's see if we can draw some text.

We start by adding a test case to the top-level, initial state test that
asserts that the text '0' is drawn on the screen:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I am a balloon shooter game!</span>

<span class="sd">    Initial state</span>
<span class="sd">    =============</span>

<span class="sd">    We run the game for a few frames, then quit:</span>

<span class="sd">    &gt;&gt;&gt; events = BalloonShooter.run_in_test_mode(</span>
<span class="sd">    ...     events=[</span>
<span class="sd">    ...         [],</span>
<span class="sd">    ...         [],</span>
<span class="sd">    ...         [],</span>
<span class="sd">    ...         [],</span>
<span class="sd">    ...         [],</span>
<span class="sd">    ...         [],</span>
<span class="sd">    ...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">    ...     ]</span>
<span class="sd">    ... )</span>

<span class="sd">    ...</span>

<span class="sd">    The score is drawn:</span>

<span class="sd">    &gt;&gt;&gt; set(events.filter(&quot;DRAW_TEXT&quot;).collect(&quot;text&quot;))</span>
<span class="sd">    {(&#39;0&#39;,)}</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="o">...</span>
</pre></div>
</div></div>
This fails because there is no event `DRAW_TEXT` yet.

We add a method to draw text and have it emit a `DRAW_TEXT` event like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw_text</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">position</span><span class="p">,</span> <span class="n">text</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_TEXT&quot;</span><span class="p">,</span> <span class="p">{</span>
            <span class="s2">&quot;x&quot;</span><span class="p">:</span> <span class="n">position</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
            <span class="s2">&quot;y&quot;</span><span class="p">:</span> <span class="n">position</span><span class="o">.</span><span class="n">y</span><span class="p">,</span>
            <span class="s2">&quot;text&quot;</span><span class="p">:</span> <span class="n">text</span><span class="p">,</span>
        <span class="p">})</span>
        <span class="n">f</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">font</span><span class="o">.</span><span class="n">Font</span><span class="p">(</span><span class="n">size</span><span class="o">=</span><span class="mi">100</span><span class="p">)</span>
        <span class="n">surface</span> <span class="o">=</span> <span class="n">f</span><span class="o">.</span><span class="n">render</span><span class="p">(</span><span class="n">text</span><span class="p">,</span> <span class="kc">True</span><span class="p">,</span> <span class="s2">&quot;black&quot;</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="o">.</span><span class="n">blit</span><span class="p">(</span><span class="n">surface</span><span class="p">,</span> <span class="p">(</span><span class="n">position</span><span class="o">.</span><span class="n">x</span><span class="p">,</span> <span class="n">position</span><span class="o">.</span><span class="n">y</span><span class="p">))</span>
</pre></div>
</div></div>
We test the pygame code manually to ensure that we use the text drawing api
correctly and that text appears on the screen.

## Keeping track of score to draw

Before we kept track of the score by adding point markers to a sprite group. We
replace this sprite group with a new `Score` object:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-        self.points = self.add(SpriteGroup())</span>
<span class="gi">+        self.score = self.add(Score())</span>
</pre></div>
</div></div>
Instead of adding point markers, we just add a number to increase the score:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-                    self.points.add(PointMarker(position=Point(</span>
<span class="gd">-                        x=20+len(self.points.get_sprites())*12,</span>
<span class="gd">-                        y=700</span>
<span class="gd">-                    )))</span>
<span class="gi">+                    self.score.add(1)</span>
</pre></div>
</div></div>
The `Score` class looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Score</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">score</span> <span class="o">=</span> <span class="mi">0</span>

    <span class="k">def</span> <span class="nf">add</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">points</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">score</span> <span class="o">+=</span> <span class="n">points</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">pass</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">1100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">20</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="nb">str</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">score</span><span class="p">))</span>
</pre></div>
</div></div>
## Adapting tests

We already have tests that make sure that we count the score correctly. We have
to modify them slightly to look at the score number instead of counting point
markers in the sprite group. We change them like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-    &gt;&gt;&gt; game.get_points()</span>
<span class="gd">-    []</span>
<span class="gi">+    &gt;&gt;&gt; game.get_score()</span>
<span class="gi">+    0</span>
</pre></div>
</div></div>
Now all tests pass and the score is displayed with text instead of point
markers. Success!

## Summary

This change went rather smoothly. Adding functionality to draw text was not
that much work. Perhaps we could just as well have done that from the start.
But point markers felt easier at the time.

Our customer is happy with the new score display. When we play the game, we
sometimes decide to go for 100 points, then stop. Perhaps that could become a
competitive aspect? How fast can you get to 100 points? And the time will be
the "final" score? We'll see.

See you in the next episode!
