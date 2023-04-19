---
title: Test driving the game loop
date: 2023-04-19
tags: agdpp
---

In this episode we look at how to set up the game loop, draw something on the
screen, and test it. We begin with a spike to learn Pygame fundamentals and
then we look at how to set up tests for it.

## Video version

The video version of this episode:

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/Q0347KVq7oU" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
</center>

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
When we run it, it shows an empty screen:

<center>
![Tutorial output.](tutorial.png)
</center>

## Draw something

An empty screen is not that interesting, so let's see if we can get an
animation going.

We add a call to draw a circle and some logic to animate it:

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

## Refactor to clarify

Next we separate the logic of the game loop from the logic of our game. We
refactor in small steps, testing manually that everything works, and end up
with this for our game:

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
</pre></div>
</div></div>
And this for our game loop:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">:</span>

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
</pre></div>
</div></div>
And it is all used like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">Game</span><span class="p">(</span><span class="n">GameLoop</span><span class="p">())</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
Remember, we are only doing a spike here. We are trying to learn Pygame and how
we could split the different responsibilities into different classes and how to
possibly test it.

With this refactoring, the game is now responsible for handling events and
drawing the animated circle and the game loop is responsible for setting up
Pygame and calling the game in a loop.

I think we have learned enough about this setup and I think I know how we could
test it. Let's see.

## How to test this?

So now we start completely from scratch, test driving our game. We know roughly
what we want to do from the spike.

Where to start?

I find it easiest to start from the outside when writing tests. What should the
system do? What should our game do?

Well, our game draws a circle on the screen until the user closes the window.
Let's start there.

Here is how we get some basic structure in place:

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
</pre></div>
</div></div>
Instead of actually drawing and exiting, we just print the action. We fake it.
The point of this is to get some basic structure in place with tests. From the
spike we know in which direction to go. Let's continue.

## Remove fakes

Eventually we want to turn `GameLoop` into an infrastructure wrapper. This will
give us the ability to conveniently use it in tests. This pattern is explained
in depth in [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks).

One part of that pattern is that we should be able to observe what `GameLoop`
is doing.

Here is how we rewrite the test for our game to assert on events fired from
`GameLoop` instead of print statements:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an animated circle until the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop()</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; Game(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">EXIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The `GameLoop` now looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">,</span> <span class="p">{})</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{})</span>
</pre></div>
</div></div>
Instead of printing actions, it notifies about its actions via an observable
pattern.

We also moved the circle drawing code to the game loop and have the game call
that method instead. The game loop will be responsible for drawing things on
the current frame.

But we are still not doing anything real, we are just firing events and
asserting on them. Time to fix that.

## Flesh out pygame calls

Continuing the pattern of an infrastructure wrapper, we add an argument to
`GameLoop` which is the Pygame module. We provide an embedded stub for the null
version that does nothing:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">GameLoop</span><span class="p">(</span><span class="n">pygame</span><span class="p">)</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">():</span>
        <span class="k">class</span> <span class="nc">NullPygame</span><span class="p">:</span>
            <span class="o">...</span>
        <span class="k">return</span> <span class="n">GameLoop</span><span class="p">(</span><span class="n">NullPygame</span><span class="p">())</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">pygame</span><span class="p">):</span>
        <span class="n">Observable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span> <span class="o">=</span> <span class="n">pygame</span>

    <span class="o">...</span>
</pre></div>
</div></div>
We write a test for the game loop that checks that the proper events are fired:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I init and clean up pygame:</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null()</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; loop.run(NullGame())</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">PYGAME_INIT =&gt;</span>
<span class="sd">EXIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And we also create a test that uses the real Pygame module:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; GameLoop.create().run(NullGame())</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This test will actually cause a window to pop up on the screen, so it is a bit
distracting, but it makes sure we are calling Pygame correctly.

The game loop now looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;PYGAME_INIT&quot;</span><span class="p">,</span> <span class="p">{})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">init</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">screen</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">set_mode</span><span class="p">((</span><span class="mi">1280</span><span class="p">,</span> <span class="mi">720</span><span class="p">))</span>
        <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">display</span><span class="o">.</span><span class="n">flip</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;EXIT&quot;</span><span class="p">,</span> <span class="p">{})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span> <span class="s2">&quot;red&quot;</span><span class="p">,</span> <span class="p">(</span><span class="mi">50</span><span class="p">,</span> <span class="mi">50</span><span class="p">),</span> <span class="mi">40</span><span class="p">)</span>
</pre></div>
</div></div>
We are getting closer. If we run the game now and look carefully, we can see
that a circle is drawn on the screen for a split second before it closes.  That
is because there is no loop yet. We just render one frame and then exit.  Time
to fix that.

## Loop and events

Let's start with our game. The test for it should simulate an event that the
user closes the window, and first then exit the application:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an animated circle until the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [pygame.event.Event(pygame.QUIT)],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; Game(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">PYGAME_INIT =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">PYGAME_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The `tick` method is modified to look like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
    <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">type</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">QUIT</span><span class="p">:</span>
            <span class="k">return</span> <span class="kc">True</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">()</span>
</pre></div>
</div></div>
So we made the decision that the game loop should exit if the tick method
returns true.

We add the ability for the null version of the game loop to simulate events:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="nd">@staticmethod</span>
<span class="k">def</span> <span class="nf">create_null</span><span class="p">(</span><span class="n">events</span><span class="o">=</span><span class="p">[]):</span>
    <span class="k">class</span> <span class="nc">NullPygame</span><span class="p">:</span>
        <span class="k">def</span> <span class="nf">init</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">event</span> <span class="o">=</span> <span class="n">NullEvent</span><span class="p">()</span>
            <span class="o">...</span>
    <span class="k">class</span> <span class="nc">NullEvent</span><span class="p">:</span>
        <span class="k">def</span> <span class="nf">get</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
            <span class="k">if</span> <span class="n">events</span><span class="p">:</span>
                <span class="k">return</span> <span class="n">events</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)</span>
            <span class="k">return</span> <span class="p">[]</span>
    <span class="k">return</span> <span class="n">GameLoop</span><span class="p">(</span><span class="n">NullPygame</span><span class="p">())</span>
</pre></div>
</div></div>
And we modify the run method to actually do a loop and pass events to the
`tick` method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="n">running</span> <span class="o">=</span> <span class="kc">True</span>
    <span class="k">while</span> <span class="n">running</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">get</span><span class="p">()):</span>
            <span class="n">running</span> <span class="o">=</span> <span class="kc">False</span>
    <span class="o">...</span>
</pre></div>
</div></div>
If we do not configure tests for our game to simulate the quit event, the test
will hang in an infinite loop.

If we run the game now, the circle stays on the screen until we close the
window. But it doesn't move. Let's work on that.

## Test animation

We modify the test for our game by simulating one more event so that one more
frame is rendered. That gives us two calls to draw circle:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an animated circle until the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [pygame.event.Event(pygame.QUIT)],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; Game(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">PYGAME_INIT =&gt;</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 50</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 51</span>
<span class="sd">PYGAME_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We had to add the `x` argument to the `DRAW_CIRCLE` event so that we could
observe that it changed:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;DRAW_CIRCLE&quot;</span><span class="p">,</span> <span class="p">{</span><span class="s2">&quot;x&quot;</span><span class="p">:</span> <span class="n">x</span><span class="p">})</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span> <span class="s2">&quot;red&quot;</span><span class="p">,</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="mi">50</span><span class="p">),</span> <span class="mi">40</span><span class="p">)</span>
</pre></div>
</div></div>
We also had to make a new call to clear the screen. If we don't clear the
screen we end up with circles drawn on top of each other. Clearing the screen
works like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">clear_screen</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;CLEAR_SCREEN&quot;</span><span class="p">,</span> <span class="p">{})</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="o">.</span><span class="n">fill</span><span class="p">(</span><span class="s2">&quot;purple&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
And the implementation for the animation looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="mi">50</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">&gt;</span> <span class="mi">500</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">=</span> <span class="mi">50</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">x</span> <span class="o">+=</span> <span class="n">dt</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">x</span><span class="p">)</span>
</pre></div>
</div></div>
To make the animation frame rate independent, we also had to include the delta
time. This is implemented in the game loop similar to how we did it in the
spike:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="n">dt</span> <span class="o">=</span> <span class="mi">0</span>
    <span class="k">while</span> <span class="n">running</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">get</span><span class="p">()):</span>
            <span class="n">running</span> <span class="o">=</span> <span class="kc">False</span>
        <span class="o">...</span>
        <span class="n">dt</span> <span class="o">=</span> <span class="n">clock</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="mi">60</span><span class="p">)</span>
    <span class="o">...</span>
</pre></div>
</div></div>
At this point our test-driven implementation does the same thing that our spike
does. We are now in a good place to move forward.

## Refactor exit

Before closing this episode, let's take advantage of our test suite and explore
an alternative way to exit the application. Instead of having a boolean return
from `tick` indicating if we should exit or not, which I think is a bit
unclear, let's try an exception. Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">ExitGameLoop</span><span class="p">(</span><span class="ne">Exception</span><span class="p">):</span>
    <span class="k">pass</span>
</pre></div>
</div></div>
The tick method then turns into this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
    <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">type</span> <span class="o">==</span> <span class="n">pygame</span><span class="o">.</span><span class="n">QUIT</span><span class="p">:</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>
    <span class="o">...</span>
</pre></div>
</div></div>
And the game loop into this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">game</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="k">try</span><span class="p">:</span>
        <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
            <span class="n">game</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">get</span><span class="p">())</span>
            <span class="o">...</span>
    <span class="k">except</span> <span class="n">ExitGameLoop</span><span class="p">:</span>
        <span class="k">pass</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;PYGAME_QUIT&quot;</span><span class="p">,</span> <span class="p">{})</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
</pre></div>
</div></div>
I like this better. And all the tests still pass, unchanged.

## Summary

We have now recreated the functionality that we had in the spike, added the
ability to test it, and improved the design with the safety net of our tests.
Great success!

You can browse the [complete source
code](https://github.com/rickardlindberg/agdpp/tree/initial-game-loop) from
this episode.

See you in the next episode!
