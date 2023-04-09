---
title: 'DRAFT: Agile Game Development with Python and Pygame: The Game Loop'
date: 2023-04-09
tags: agdpp,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we will look at how to set up the game loop, draw something on
the screen, and setup tests for it. We begin with a spike to learn Pygame
fundamentals and then we look at how to set it up properly with tests.

## Hello World

We start with this example straight from the [Pygame
docs](https://www.pygame.org/docs/):

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># Example file showing a basic pygame &quot;game loop&quot;</span>
<span class="kn">import</span> <span class="nn">pygame</span>

<span class="c1"># pygame setup</span>
<span class="n">pygame</span><span class="o">.</span><span class="n">init</span><span class="p">()</span>
<span class="n">screen</span> <span class="o">=</span> <span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">set_mode</span><span class="p">((</span><span class="mi">1280</span><span class="p">,</span> <span class="mi">720</span><span class="p">))</span>
<span class="n">clock</span> <span class="o">=</span> <span class="n">pygame</span><span class="o">.</span><span class="n">time</span><span class="o">.</span><span class="n">Clock</span><span class="p">()</span>
<span class="n">running</span> <span class="o">=</span> <span class="kc">True</span>

<span class="k">while</span> <span class="n">running</span><span class="p">:</span>
    <span class="c1"># poll for events</span>
    <span class="c1"># pygame.QUIT event means the user clicked X to close your window</span>
    <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">get</span><span class="p">():</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">type</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">QUIT</span><span class="p">:</span>
            <span class="n">running</span> <span class="o">=</span> <span class="kc">False</span>

    <span class="c1"># fill the screen with a color to wipe away anything from last frame</span>
    <span class="n">screen</span><span class="o">.</span><span class="n">fill</span><span class="p">(</span><span class="s2">&quot;purple&quot;</span><span class="p">)</span>

    <span class="c1"># RENDER YOUR GAME HERE</span>

    <span class="c1"># flip() the display to put your work on screen</span>
    <span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">flip</span><span class="p">()</span>

    <span class="n">clock</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="mi">60</span><span class="p">)</span>  <span class="c1"># limits FPS to 60</span>

<span class="n">pygame</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
</pre></div>
</div></div>
When run, it shows an empty screen:

<center>
![Tutorial output.](tutorial.png)
</center>

## Add drawing of something

An empty screen is not that interesting, so let's see if we can get an
animation going.

We add a call to draw a circle and some logic to move where that circle is
drawn:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="o">...</span>

<span class="n">pos_x</span> <span class="o">=</span> <span class="mi">50</span>
<span class="n">dt</span> <span class="o">=</span> <span class="mi">0</span>

<span class="k">while</span> <span class="n">running</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">if</span> <span class="n">pos_x</span> <span class="o">&gt;</span> <span class="mi">500</span><span class="p">:</span>
        <span class="n">pos_x</span> <span class="o">=</span> <span class="mi">50</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">pos_x</span> <span class="o">+=</span> <span class="n">dt</span><span class="o">*</span><span class="mf">0.3</span>

    <span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="n">screen</span><span class="p">,</span> <span class="s2">&quot;red&quot;</span><span class="p">,</span> <span class="p">(</span><span class="n">pos_x</span><span class="p">,</span> <span class="mi">50</span><span class="p">),</span> <span class="mi">40</span><span class="p">)</span>

    <span class="o">...</span>

    <span class="n">dt</span> <span class="o">=</span> <span class="n">clock</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="mi">60</span><span class="p">)</span>  <span class="c1"># limits FPS to 60</span>

<span class="o">...</span>
</pre></div>
</div></div>
This seems to work. We get an animated circle:

<center>
![Animated circle.](animation.png)
</center>

## Refactor to clarify game loop concept

Next we want to split the logic of the game loop from the logic of our game. We
refactor in small steps, testing manually that everything works, and end up
with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span> <span class="o">=</span> <span class="n">loop</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pos_x</span> <span class="o">=</span> <span class="mi">50</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">screen</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">get</span><span class="p">():</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">type</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">QUIT</span><span class="p">:</span>
                <span class="k">return</span> <span class="kc">True</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">pos_x</span> <span class="o">&gt;</span> <span class="mi">500</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">pos_x</span> <span class="o">=</span> <span class="mi">50</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">pos_x</span> <span class="o">+=</span> <span class="n">dt</span><span class="o">*</span><span class="mf">0.3</span>
        <span class="n">screen</span><span class="o">.</span><span class="n">fill</span><span class="p">(</span><span class="s2">&quot;purple&quot;</span><span class="p">)</span>
        <span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="n">screen</span><span class="p">,</span> <span class="s2">&quot;red&quot;</span><span class="p">,</span> <span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">pos_x</span><span class="p">,</span> <span class="mi">50</span><span class="p">),</span> <span class="mi">40</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">GameLoop</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="n">pygame</span><span class="o">.</span><span class="n">init</span><span class="p">()</span>
        <span class="n">screen</span> <span class="o">=</span> <span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">set_mode</span><span class="p">((</span><span class="mi">1280</span><span class="p">,</span> <span class="mi">720</span><span class="p">))</span>
        <span class="n">clock</span> <span class="o">=</span> <span class="n">pygame</span><span class="o">.</span><span class="n">time</span><span class="o">.</span><span class="n">Clock</span><span class="p">()</span>
        <span class="n">running</span> <span class="o">=</span> <span class="kc">True</span>
        <span class="n">dt</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="k">while</span> <span class="n">running</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">,</span> <span class="n">screen</span><span class="p">):</span>
                <span class="n">running</span> <span class="o">=</span> <span class="kc">False</span>
            <span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">flip</span><span class="p">()</span>
            <span class="n">dt</span> <span class="o">=</span> <span class="n">clock</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="mi">60</span><span class="p">)</span>
        <span class="n">pygame</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>

<span class="n">Game</span><span class="p">(</span><span class="n">GameLoop</span><span class="p">())</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
Remember, we are only doing a spike here. We are trying to learn Pygame and how
we could split the different responsibilities into different classes and how to
possible test it.

The game is now responsible for handling events and drawing the animated circle
and the game loop is responsible for setting up Pygame and calling the game in
a loop.

## How to test this?

I find it easiest to start from the outside when writing tests. What should the
system do? What should our game do?

* I draw a circle on the screen until the user exits.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I draw an animated circle until the user closes the window.</span>

<span class="sd">    &gt;&gt;&gt; game = Game(GameLoop())</span>
<span class="sd">    &gt;&gt;&gt; game.run()</span>
<span class="sd">    DRAW_CIRCLE</span>
<span class="sd">    EXIT</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span> <span class="o">=</span> <span class="n">loop</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">GameLoop</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">()</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">)</span>

<span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="n">Game</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>