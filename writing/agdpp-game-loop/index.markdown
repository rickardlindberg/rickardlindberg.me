---
title: 'DRAFT: Agile Game Development with Python and Pygame: The Game Loop'
date: 2023-04-05
tags: agdpp,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we will look at how to set up the game loop, draw something on
the screen, and setup tests for it. We begin with a spike to learn Pygame
fundamentals and then we look at how to set it up properly with tests.

## Hello World

We start with this example straight from the [Pygame
docs](https://www.pygame.org/docs/):

<div class="rliterate-code"><div class="rliterate-code-header"><ol class="rliterate-code-path"><li>doc-tutorial.py</li></ol></div><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># Example file showing a basic pygame &quot;game loop&quot;</span>
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

## Add drawing of something

## Refactor to clarify game loop concept

## How to test this?