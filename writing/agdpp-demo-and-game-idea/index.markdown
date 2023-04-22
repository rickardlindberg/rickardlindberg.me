---
title: 'DRAFT: Demo and game idea'
date: 2023-04-22
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we demo the "game" for our customer. We get some feedback on it
and start evolving the game to meet our customer's vision.

## Video version

The video version of this episode:

<center>
...
</center>

## Demo

I mentioned to my son that I had started working on a game. He wanted to see
it. I told him that there wasn't much to play yet, but I could show him what it
looks like.

I started the "game" and he watched the circle going back and forth.

For reference, this is what the "game" looks like now:

<center>
![Animated circle.](animation.png)
</center>

Then he tried to interact with the circle. He grabbed the mouse and clicked
away and was disappointed that nothing happened. I told him that you can't do
anything yet, that it's just an animation.

He looked at me sad and said "Dad, this game is boring."

Initially I wasn't planning on showing him the game because in this early stage
there is not much to play, and I knew he was not going to like it. Did I ruin
it?

Then I asked him, "What would you like to do with the circle?"

The sadness in his face faded and he started talking about balloons. He said
that he wanted the circle to be a balloon and that he wanted to shoot down
balloons with arrows.

I think we have an idea for a game.

## On early feedback

My initial idea for the game was some kind of tetris variant. I was thinking
that you have two boards and that you could help each other out by clearing
blocks on each other's boards. Cooperation seems more fun that competition.

Turns out, the customer wants a balloon shooter. If we hadn't shown the demo,
we might have never found that out.

## Simplest possible version

In the spirit of
[Ward](https://www.artima.com/articles/the-simplest-thing-that-could-possibly-work#part3)
we ask ourselves what the simplest possible balloon shooter could look like.
What is the absolute minimum version that I can give to my son and he can
somewhat enjoy playing or at least recognize as a balloon shooter? (The goal is
to create a game that *we* can enjoy playing together. That means some kind of
multiplayer mode. But that is another for another story.)

Here is what I'm thinking:

* 1 balloon falling down the screen
* 1 arrow pointing in a fixed direction
* 1 button to shoot that single arrow
* Then game over

So the only challenge will be to fire the arrow at the right time for it to hit
the balloon. You will either hit and get a point or miss and it'll be game
over.

Oh, and circles for the graphics is probably fine. The balloon can be drawn
with one big circle, and the arrow maybe with three smaller circles in a row.
If I tell my son to imagine they are balloons and arrows, I think he'll accept
that.

## First story

To make progress towards a balloon shooter, I want to work on a story that is
about drawing a balloon and an arrow. Create the initial scene of the game sort
of.

Our game already animates a circle. Let's put all behavior related to that
circle into its own class called `Balloon`. Here is how our game `tick` method
looks like now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">&gt;</span> <span class="mi">500</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="mi">50</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">+=</span> <span class="n">dt</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span>
</pre></div>
</div></div>
After we extract the balloon class, it looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>
</pre></div>
</div></div>
And here is the balloon class:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="mi">50</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">&gt;</span> <span class="mi">500</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="mi">50</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">+=</span> <span class="n">dt</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span>
</pre></div>
</div></div>
Any behavior that the balloon should have, we can now test at this lower level.
We can instantiate a balloon, call its tick method, and observe that the right
thing happens. There is no need to involve the game or the game loop. (At least
if accept exposing somewhat internal state like the x variable.)

With the balloon object in place, it is natural to create a new object called
`Arrow` for our other piece in the balloon shooter game. We create a version
that just draws a circle:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">pass</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="mi">10</span><span class="p">)</span>
</pre></div>
</div></div>
We make sure it is included in the game by modifying the tick method of the
game to also tick and draw the arrow:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">tick</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>
</pre></div>
</div></div>
We notice a pattern here. It seems like the responsibility of the game is to
call tick and draw on a set of objects. In games (or in Pygame) those objects
are referred to as sprites. My understanding is that a sprite is any visual
element that shows up in the game.

We refactor our game to reflect this new understanding:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span> <span class="o">=</span> <span class="n">Balloon</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="n">Arrow</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span> <span class="o">=</span> <span class="p">[</span><span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="p">]</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">for</span> <span class="n">sprite</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span><span class="p">:</span>
            <span class="n">sprite</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
        <span class="k">for</span> <span class="n">sprite</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span><span class="p">:</span>
            <span class="n">sprite</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
If we run the game now, this is what we see:

<center>
![First scene of balloon shooter.](scene1.png)
</center>

Not very pretty. What is that?

Let's see if we can improve the drawing of the arrow. But remember, not
perfection, but improvement. If I look at the game now, I don't get an idea
what the game is about. We just want to make the arrow slightly more realistic
to convey the meaning of the object. We are still restricted to drawing
circles. The current `draw_circle` looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{</span><span class="s2">&quot;x&quot;</span><span class="p">:</span> <span class="n">x</span><span class="p">})</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span> <span class="s2">&quot;red&quot;</span><span class="p">,</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="mi">50</span><span class="p">),</span> <span class="mi">40</span><span class="p">)</span>
</pre></div>
</div></div>
That's stupid! Why is there no ability to specify anything but the
x-coordinate? Well, until now, we haven't needed that. Now that we do need it,
let's add it. No biggie:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">50</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">40</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;red&quot;</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{</span><span class="s2">&quot;x&quot;</span><span class="p">:</span> <span class="n">x</span><span class="p">,</span> <span class="s2">&quot;y&quot;</span><span class="p">:</span> <span class="n">y</span><span class="p">,</span> <span class="s2">&quot;radius&quot;</span><span class="p">:</span> <span class="n">radius</span><span class="p">,</span> <span class="s2">&quot;color&quot;</span><span class="p">:</span> <span class="n">color</span><span class="p">})</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span> <span class="n">color</span><span class="p">,</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">),</span> <span class="n">radius</span><span class="p">)</span>
</pre></div>
</div></div>
We experiment with three circles for the arrow and tweak the numbers until we
think it looks good. Here is the result:

<center>
![Improved drawing of arrow.](scene2.png)
</center>

I don't know about you, but when I see this, I want to press a button to fire
that arrow so it hits the balloon. I'm convinced this is a balloon shooter now.
I hope our customer is as well.

Right, balloon shooter. Before we didn't know what game we should write, so our
game class was just called `Game`. Let's fix that:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span> from gameloop import GameLoop
 
<span class="gd">-class Game:</span>
<span class="gi">+class BalloonShooter:</span>
</pre></div>
</div></div>
There, now the code more accurately represent the ideas that we have in our
minds about this game.

I forget to mention anything about testing. (For the complete workflow, check
out the video.) But here is what the test for the balloon shooter looks like
now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw the initial scene of the game which consists of a balloon and an</span>
<span class="sd">arrow and quit when the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; BalloonShooter(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 50</span>
<span class="sd">    y: 50</span>
<span class="sd">    radius: 40</span>
<span class="sd">    color: &#39;red&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 500</span>
<span class="sd">    radius: 10</span>
<span class="sd">    color: &#39;blue&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 520</span>
<span class="sd">    radius: 15</span>
<span class="sd">    color: &#39;blue&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 540</span>
<span class="sd">    radius: 20</span>
<span class="sd">    color: &#39;blue&#39;</span>
<span class="sd">GAMELOOP_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We both changed the description, becasue we have a balloon shooter now, not a
generic game, and added checks for it drawing both the balloon and the arrow.

It might be too specific to assert on exact coordinates here. We have to
visually inspect the output and see if we think it looks good. That's what I
did here. And when I was happy, I copied the coordinates into the test.

## Summary

We have completed a first vertical slice of our balloon shooter. There is now a
balloon and an arrow on the screen. This is new behavior that we could show to
our customer and get feedback on. Perhaps he doesn't like the position of the
arrow. If so, we can adjust. Perhaps he can't imagine that the circle is
actually a balloon. If so, we can work on improving the graphics.

What else do we think we need before we have the first version of the balloon
shooter?

* Balloon moves downwards
* Arrow animates when shot
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* Real graphics instead of circles

Which one is the most important one to work on next? We'll tackle that one in
the next episode!

You can browse the [complete source
code](https://github.com/rickardlindberg/agdpp/tree/initial-balloon-shooter-story) from
this episode.

See you!
