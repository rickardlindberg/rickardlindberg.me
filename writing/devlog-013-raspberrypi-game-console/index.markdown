---
title: 'DevLog 013: Raspberry Pi game console'
date: 2023-09-10
tags: devlog,agdpp
devlog: true
---

It is time to revisit the [balloon shooter](/projects/agdpp/index.html). I'm
interested in building a "game console PC" so that my son can more easily play
the balloon shooter and other games. Until now we have played all games on my
laptop.

This will involve two main steps I think. The first is to get a Raspberry Pi
and install all games on it. The second involves auto starting a custom
application that can be used to select which game to play by using the gamepad.
Ideally, you should not need to use a mouse or a keyboard. My plan for this
custom application is to build it using the framework that we have in the
balloon shooter.

Let's get started.

## The Raspberry Pi

At first, I'm not sure what hardware to get for this game console PC. I look
around a bit, and then eventually settle on a Raspberry Pi starter kit.

<p>
<center>
![Raspberry Pi starter kit.](pibox.png)
</center>
</p>

I am bit concerned that it will not be powerful enough to play games. But it is
relatively cheap, and if it can't play all games, perhaps my son (or me) can
have some fun with it in another way.

## Assembly

The starter kit comes with everything you need to get started. That's also one
reason that I went with it. I'm not that interested in selecting hardware. I'm
more interested in quickly prototyping this game console PC. If it turns out
the Pi is not powerful enough, but the game console PC concept is a hit, we can
look for better hardware. However, if the game console PC is not a hit, we have
not wasted that much time or money.

And look. Apparently Raspberry Pis need heat sinks and fans nowadays. When I
last played with a Pi, many, many years ago, I don't remember that being the
case. Let's hope that means that they are more powerful now.

<p>
<center>
![Assembling the starter kit.](assembly.png)
</center>
</p>

I assemble the kit in about 15 minutes. Then I boot it up and install the
operating system that comes preconfigured. I let it do its thing, and come
back once it is installed.

## Setup

I want to install [SuperTux](https://www.supertux.org/) and the balloon
shooter on the Pi.

It seems like the version of SuperTux is older than what I have on my laptop.
And my laptop is old.  Furthermore, Python 2 seems to be the default Python. I
learn that when trying to install all requirements for the balloon shooter. I
also have to install a newer version of Pygame and for that I need to install
some SDL build dependencies.  Perhaps getting a newer operating system would be
nice.

Eventually, I get everything working:

<p>
<center>
![Setting up games.](setup.png)
</center>
</p>

The versions might be a little old. The performance might be so so. But we have
something setup that we can experiment with.

## A note on performance

Me and my son try to play SuperTux on the setup. It feels a little different.
Part of it might be that it is slightly different version of the game. Part of
it might be that the Pi has worse performance. We try to run the game at a
lower resolution, and it seems to help a bit. We can probably try different
things to get better performance, but this is absolutely fine for now. My son
is still having fun playing.

## Autostart

To start SuperTux on the Pi you first have to start the Pi and then you have to
select SuperTux from the menu with the mouse. The balloon shooter is even more
complicated to start. First you need to open a terminal and then run a
command.

I don't think that is good enough for a game console PC. I want to be able to
operate it using the gamepad only.

The first tiny step in that direction is to configure SuperTux as the startup
application. If we can do that, then SuperTux can be started and played without
using the keyboard or mouse.

Once we have that working, we can work on our own startup application that
let us select the game, and then we can start that one instead.

I search the internet for how to configure a startup application for the Pi.

I find an article that says that you can put a file in the autostart
directory. I try this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ cat /etc/xdg/autostart/game_console_start.desktop
[Desktop Entry]
Name=Game console start
Exec=supertux2
</pre></div>
</div></div>
I restart the Pi, and SuperTux actually starts automatically and you can start
playing it using the gamepad. Fantastic!

## Startup application idea

Let's move on to our custom startup application. Here is the idea that I have
for it:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
    <span class="n">subprocess</span><span class="o">.</span><span class="n">call</span><span class="p">(</span><span class="n">StartupApplication</span><span class="o">.</span><span class="n">create</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">())</span>
</pre></div>
</div></div>
This code runs the startup application in a loop. Its `run` method should
return the command to run. (The game to play or shutdown command.)

I think we can test drive the `StartupApplication` and then we can hook it up
in the loop above.

Perhaps we should even test drive the loop.

We'll see.

## Test driving the application

I start with this in a new file:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I draw an application select screen:</span>

<span class="sd">    &gt;&gt;&gt; events = StartupApplication.run_in_test_mode(</span>
<span class="sd">    ...     events=[</span>
<span class="sd">    ...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">    ...     ]</span>
<span class="sd">    ... )</span>
<span class="sd">    &quot;&quot;&quot;</span>
</pre></div>
</div></div>
I create the bare minimum that the test complains about and get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">run_in_test_mode</span><span class="p">(</span><span class="n">events</span><span class="o">=</span><span class="p">[]):</span>
        <span class="n">loop</span> <span class="o">=</span> <span class="n">GameLoop</span><span class="o">.</span><span class="n">create_null</span><span class="p">(</span>
            <span class="n">events</span><span class="o">=</span><span class="n">events</span><span class="o">+</span><span class="p">[</span>
                <span class="p">[</span><span class="n">GameLoop</span><span class="o">.</span><span class="n">create_event_user_closed_window</span><span class="p">()],</span>
            <span class="p">]</span>
        <span class="p">)</span>
        <span class="n">events</span> <span class="o">=</span> <span class="n">loop</span><span class="o">.</span><span class="n">track_events</span><span class="p">()</span>
        <span class="n">StartupApplication</span><span class="p">(</span><span class="n">loop</span><span class="p">)</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
        <span class="k">return</span> <span class="n">events</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span> <span class="o">=</span> <span class="n">loop</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">pass</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">pass</span>
</pre></div>
</div></div>
Now it doesn't complain, but it seems to hang in an infinite loop.

I modify `event` to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>
</pre></div>
</div></div>
And we're green. Let's commit.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ git commit -a -m &#39;Emryo to new startup application.&#39;
[main a55d17e] Emryo to new startup application.
 2 files changed, 39 insertions(+)
 create mode 100644 startup.py
</pre></div>
</div></div>
The test is not yet fleshed out. It doesn't test what it says it tests. But it
drove out the skeleton of the application.

## Reflecting on the design

It's been a while since I worked on the balloon shooter. What do I think when I
work in this design again?

I got stuck in an infinite loop. That happens because we have a `while True:`
in our game loop somewhere. I've always found testing infinite loops difficult.
That's one reason why I hesitated testing the loop for the startup application.
But now I get another idea. What if we create the loop like this instead?

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">while</span> <span class="bp">self</span><span class="o">.</span><span class="n">loop_condition</span><span class="o">.</span><span class="n">active</span><span class="p">():</span>
    <span class="o">...</span>
</pre></div>
</div></div>
Then we can create different versions of the loop condition maybe something
like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">InfiniteLoopCondition</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">active</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="kc">True</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">TestLoopCondition</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">iterations</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">counter</span> <span class="o">=</span> <span class="mi">0</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">iterations</span> <span class="o">=</span> <span class="n">iterations</span>

    <span class="k">def</span> <span class="nf">active</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">flag</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">counter</span> <span class="o">&gt;=</span> <span class="bp">self</span><span class="o">.</span><span class="n">iterations</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">iterations</span> <span class="o">+=</span> <span class="mi">1</span>
        <span class="k">return</span> <span class="n">flag</span>
</pre></div>
</div></div>
Let's see if we can try this out in the startup application. If it works out
well, perhaps we can port it to the game loop as well?

## A mistake

The test that we wrote does not assert anything on the events. Let's fix that.
I comment out the assignment of `events` and paste the expected test output:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an application select screen:</span>

<span class="sd">&gt;&gt;&gt; StartupApplication.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">GAMELOOP_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
## The looping concept

This startup application should run in an infinite loop. In each iteration it
should init the game loop and show the game selection screen. Once the
selection has been made, it should quit the game loop and run the command. Then
it starts all over.

Let's try the looping thing.

I start by TDDing the loop conditions:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">InifiteLoopCondition</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">active</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; InifiteLoopCondition().active()</span>
<span class="sd">        True</span>
<span class="sd">        &quot;&quot;&quot;</span>
</pre></div>
</div></div>
That fails. Fix by return true. The other:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FiteLoopCondition</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">iterations</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">iterations</span> <span class="o">=</span> <span class="n">iterations</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">count</span> <span class="o">=</span> <span class="mi">0</span>

    <span class="k">def</span> <span class="nf">active</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; condition = FiteLoopCondition(iterations=2)</span>
<span class="sd">        &gt;&gt;&gt; condition.active()</span>
<span class="sd">        True</span>
<span class="sd">        &gt;&gt;&gt; condition.active()</span>
<span class="sd">        True</span>
<span class="sd">        &gt;&gt;&gt; condition.active()</span>
<span class="sd">        False</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="n">flag</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">count</span> <span class="o">&lt;</span> <span class="bp">self</span><span class="o">.</span><span class="n">iterations</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">count</span> <span class="o">+=</span> <span class="mi">1</span>
        <span class="k">return</span> <span class="n">flag</span>
</pre></div>
</div></div>
I actually got the condition wrong here at first. I'm glad I wrote a test for
it. The previous example, `TestLoopCondition`, above is actually wrong. Even
for really simple code like this, having tests is nice.

Let's see if we can use a loop condition and have the test show us that two
loops are actually made.

I change

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">run_in_test_mode</span><span class="p">(</span><span class="n">events</span><span class="o">=</span><span class="p">[]):</span>
        <span class="o">...</span>
        <span class="n">StartupApplication</span><span class="p">(</span><span class="n">loop</span><span class="p">)</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
        <span class="o">...</span>
</pre></div>
</div></div>
to

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">run_in_test_mode</span><span class="p">(</span><span class="n">events</span><span class="o">=</span><span class="p">[]):</span>
        <span class="o">...</span>
        <span class="n">StartupApplication</span><span class="p">(</span>
            <span class="n">loop</span><span class="o">=</span><span class="n">loop</span><span class="p">,</span>
            <span class="n">loop_condition</span><span class="o">=</span><span class="n">FiteLoopCondition</span><span class="p">(</span><span class="mi">2</span><span class="p">)</span>
        <span class="p">)</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
        <span class="o">...</span>
</pre></div>
</div></div>
I also notice that i misspelled finite. I fix that and then add the parameter
to the class. Test passes. Let's add an actual loop:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">while</span> <span class="bp">self</span><span class="o">.</span><span class="n">loop_condition</span><span class="o">.</span><span class="n">active</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
</pre></div>
</div></div>
This, expectedly, output another loop which I add to the assertion. Perfect!

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>    GAMELOOP_INIT =&gt;
        resolution: (1280, 720)
        fps: 60
    GAMELOOP_QUIT =&gt;
</pre></div>
</div></div>
We are not yet using the `InfiniteLoopCondition`. Let's change that by adding a
`create` method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">():</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; isinstance(StartupApplication.create(), StartupApplication)</span>
<span class="sd">        True</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">return</span> <span class="n">StartupApplication</span><span class="p">(</span>
            <span class="n">loop</span><span class="o">=</span><span class="n">GameLoop</span><span class="o">.</span><span class="n">create</span><span class="p">(),</span>
            <span class="n">loop_condition</span><span class="o">=</span><span class="n">InifiteLoopCondition</span><span class="p">()</span>
        <span class="p">)</span>
</pre></div>
</div></div>
I also add this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="n">StartupApplication</span><span class="o">.</span><span class="n">create</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
And when I run

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ python startup.py
</pre></div>
</div></div>
It indeed creates a new window every time I close it.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ git commit -a -m &#39;Add startup entry point and have it loop.&#39;
[main aadd1a2] Add startup entry point and have it loop.
 1 file changed, 60 insertions(+), 5 deletions(-)
</pre></div>
</div></div>
## Selecting a game

What is the simplest possible solution for selecting a game?

I imagine that the display shows an icon for each game that can be selected.
Then you move a cursor over it and press a key to select it.

I start by getting some games on the screen:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">100</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">200</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;Balloon Shooter&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
It looks like this:

<p>
<center>
![Games in startup screen.](games.png)
</center>
</p>

I think we also need a cursor:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">100</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">200</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;Balloon Shooter&quot;</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">500</span><span class="p">),</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;pink&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
It looks like this:

<p>
<center>
![Cursor in startup screen.](cursor.png)
</center>
</p>

Now I think two things are missing. The first is that at the press of a button,
the game closest to the cursor should start. The second is that you also need
to be able to move the cursor.

I think working on movement is secondary. It is more important to be able to
start **one** game instead of nothing. So let's work on that first.

## Starting a game

I want to write a test for the new behavior, but I find that testing at the top
level is tedious and error prone. I would therefore like to start by
refactoring and extracting a `StartupScene` maybe that has an interface that is
easier to test. I end up with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">100</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">200</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;Balloon Shooter&quot;</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">500</span><span class="p">),</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;pink&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
I'm sure this refactoring works because I have tests to cover it.

Commit!

Now, let's see if we can write a test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">When XBOX_A is pressed, I start the game that is closest to the cursor:</span>

<span class="sd">&gt;&gt;&gt; scene = StartupScene()</span>
<span class="sd">&gt;&gt;&gt; scene.event(GameLoop.create_event_joystick_down(XBOX_A))</span>
<span class="sd">SuperTux</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
I make it pass like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_joystick_down</span><span class="p">(</span><span class="n">XBOX_A</span><span class="p">):</span>
            <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
This is obviously faking it. It is not supposed to print the name of the game,
it is supposed to run it, or, wait a minute. This class is not supposed to run
it, the top-level class is.

Let's scratch this and start over.

## Starting a game (again)

Let's have a look at the top-level test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an application select screen:</span>

<span class="sd">&gt;&gt;&gt; StartupApplication.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_TEXT =&gt;</span>
<span class="sd">    x: 100</span>
<span class="sd">    y: 100</span>
<span class="sd">    text: &#39;SuperTux&#39;</span>
<span class="sd">DRAW_TEXT =&gt;</span>
<span class="sd">    x: 100</span>
<span class="sd">    y: 200</span>
<span class="sd">    text: &#39;Balloon Shooter&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 500</span>
<span class="sd">    radius: 20</span>
<span class="sd">    color: &#39;pink&#39;</span>
<span class="sd">GAMELOOP_QUIT =&gt;</span>
<span class="sd">GAMELOOP_INIT =&gt;</span>
<span class="sd">    resolution: (1280, 720)</span>
<span class="sd">    fps: 60</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_TEXT =&gt;</span>
<span class="sd">    x: 100</span>
<span class="sd">    y: 100</span>
<span class="sd">    text: &#39;SuperTux&#39;</span>
<span class="sd">DRAW_TEXT =&gt;</span>
<span class="sd">    x: 100</span>
<span class="sd">    y: 200</span>
<span class="sd">    text: &#39;Balloon Shooter&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 500</span>
<span class="sd">    radius: 20</span>
<span class="sd">    color: &#39;pink&#39;</span>
<span class="sd">GAMELOOP_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This shows our game loop runs twice, but there is no mention that a command is
run. Let's modify

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">while</span> <span class="bp">self</span><span class="o">.</span><span class="n">loop_condition</span><span class="o">.</span><span class="n">active</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
</pre></div>
</div></div>
to

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">while</span> <span class="bp">self</span><span class="o">.</span><span class="n">loop_condition</span><span class="o">.</span><span class="n">active</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
            <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;TODO: run </span><span class="si">{</span><span class="bp">self</span><span class="o">.</span><span class="n">startup_scene</span><span class="o">.</span><span class="n">get_command</span><span class="p">()</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
It complains that `get_command` does not exist. Let's add it:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">get_command</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="p">[</span><span class="s2">&quot;supertux2&quot;</span><span class="p">]</span>

    <span class="o">...</span>
</pre></div>
</div></div>
We are now getting a somewhat expected test failure:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Differences (ndiff with -expected +actual):
    + TODO: run [&#39;supertux2&#39;]
    + TODO: run [&#39;supertux2&#39;]
      GAMELOOP_INIT =&gt;
          resolution: (1280, 720)
          fps: 60
      CLEAR_SCREEN =&gt;
</pre></div>
</div></div>
I was thinking to fake this and postpone running the actual command. To do it
properly we need an infrastructure wrapper for running commands. I'll just do
it.

Here is a first faked version:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Command</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">Command</span><span class="p">()</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">Command</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">command</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;COMMAND&quot;</span><span class="p">,</span> <span class="p">{</span><span class="s2">&quot;command&quot;</span><span class="p">:</span> <span class="n">command</span><span class="p">})</span>
</pre></div>
</div></div>
Instead of printing the command, it sends a notification so that we can assert
that the event happens at the right time in the test. That is, we can assert
that a command is run after the game loop is quit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>...
GAMELOOP_QUIT =&gt;
COMMAND =&gt;
    command: [&#39;supertux2&#39;]
...
</pre></div>
</div></div>
This works. Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ git commit -a -m &#39;Run command from StartupScene when game loop is quit.&#39;
[main 4c47b18] Run command from StartupScene when game loop is quit.
 1 file changed, 31 insertions(+), 5 deletions(-)
</pre></div>
</div></div>
For this to actually do something, we need to flesh out `Command`. Here is what
I end up with:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Command</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    &gt;&gt;&gt; Command.create().run([&quot;echo&quot;, &quot;hello&quot;])</span>

<span class="sd">    &gt;&gt;&gt; Command.create().run([&quot;command-that-does-not-exist&quot;])</span>
<span class="sd">    Traceback (most recent call last):</span>
<span class="sd">      ...</span>
<span class="sd">    FileNotFoundError: [Errno 2] No such file or directory: &#39;command-that-does-not-exist&#39;</span>

<span class="sd">    &gt;&gt;&gt; Command.create_null().run([&quot;command-that-does-not-exist&quot;])</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create</span><span class="p">():</span>
        <span class="k">return</span> <span class="n">Command</span><span class="p">(</span><span class="n">subprocess</span><span class="o">=</span><span class="n">subprocess</span><span class="p">)</span>

    <span class="nd">@staticmethod</span>
    <span class="k">def</span> <span class="nf">create_null</span><span class="p">():</span>
        <span class="k">class</span> <span class="nc">NullSubprocess</span><span class="p">:</span>
            <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">command</span><span class="p">):</span>
                <span class="k">pass</span>
        <span class="k">return</span> <span class="n">Command</span><span class="p">(</span><span class="n">subprocess</span><span class="o">=</span><span class="n">NullSubprocess</span><span class="p">())</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">subprocess</span><span class="p">):</span>
        <span class="n">Observable</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">subprocess</span> <span class="o">=</span> <span class="n">subprocess</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">command</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">notify</span><span class="p">(</span><span class="s2">&quot;COMMAND&quot;</span><span class="p">,</span> <span class="p">{</span><span class="s2">&quot;command&quot;</span><span class="p">:</span> <span class="n">command</span><span class="p">})</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">subprocess</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">command</span><span class="p">)</span>
</pre></div>
</div></div>
When the startup application is run and then quit, SuperTux is actually
started.

This is actually some real progress.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ git commit -a -m &#39;Command actually runs commands.&#39;
[main 270440e] Command actually runs commands.
 1 file changed, 23 insertions(+), 2 deletions(-)
</pre></div>
</div></div>
## Selection behavior

Let's review the `StartupScene`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">get_command</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="p">[</span><span class="s2">&quot;supertux2&quot;</span><span class="p">]</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">100</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">200</span><span class="p">),</span> <span class="n">text</span><span class="o">=</span><span class="s2">&quot;Balloon Shooter&quot;</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">500</span><span class="p">),</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;pink&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
We have higher-level tests in place that checks that whatever `get_command`
returns is run when the game loop quits.

I think it should now be fairly easy to write tests for selection behavior.
Let's first modify the event handler to also exit the game loop when `XBOX_A`
is pressed:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; StartupScene().event(GameLoop.create_event_user_closed_window())</span>
<span class="sd">        Traceback (most recent call last):</span>
<span class="sd">          ...</span>
<span class="sd">        gameloop.ExitGameLoop</span>

<span class="sd">        &gt;&gt;&gt; StartupScene().event(GameLoop.create_event_joystick_down(XBOX_A))</span>
<span class="sd">        Traceback (most recent call last):</span>
<span class="sd">          ...</span>
<span class="sd">        gameloop.ExitGameLoop</span>
<span class="sd">        &quot;&quot;&quot;</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">()</span> <span class="ow">or</span> <span class="n">event</span><span class="o">.</span><span class="n">is_joystick_down</span><span class="p">(</span><span class="n">XBOX_A</span><span class="p">):</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>
</pre></div>
</div></div>
Now let's think about what `get_command` should return. It should return the
command of the game that is closest to the cursor. Let's write two tests for
that:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_command</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; scene = StartupScene()</span>

<span class="sd">        &gt;&gt;&gt; scene.move_cursor(x=100, y=100)</span>
<span class="sd">        &gt;&gt;&gt; scene.get_command()</span>
<span class="sd">        [&#39;supertux2&#39;]</span>

<span class="sd">        &gt;&gt;&gt; scene.move_cursor(x=100, y=200)</span>
<span class="sd">        &gt;&gt;&gt; scene.get_command()</span>
<span class="sd">        [&#39;python&#39;, &#39;/home/.../agdpp/agdpp.py&#39;]</span>
<span class="sd">        &quot;&quot;&quot;</span>
</pre></div>
</div></div>
It complains that `move_cursor` does not exist. I add it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span> <span class="o">=</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">500</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">move_cursor</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span> <span class="o">=</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
I also modify the drawing code to use this point for the cursor.

Now the second test case fails:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Failed example:
    scene.get_command()
Differences (ndiff with -expected +actual):
    - [&#39;python&#39;, &#39;/home/.../agdpp/agdpp.py&#39;]
    + [&#39;supertux2&#39;]
</pre></div>
</div></div>
I make a quick and dirty fix, because I want to go quickly to green so that I
can refactor and generalize the solution:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>    <span class="k">def</span> <span class="nf">get_command</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span><span class="o">.</span><span class="n">y</span> <span class="o">==</span> <span class="mi">200</span><span class="p">:</span>
            <span class="k">return</span> <span class="p">[</span><span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;/home/.../agdpp/agdpp.py&quot;</span><span class="p">]</span>
        <span class="k">return</span> <span class="p">[</span><span class="s2">&quot;supertux2&quot;</span><span class="p">]</span>
</pre></div>
</div></div>
And this is my favorite state of programming. This is actually where some
design happens. I have the safety net of the tests and I can push code around
until I think it looks good and the next thing is easy to add.

Here is what I come up with this time:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span> <span class="o">=</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">500</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">500</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">games</span> <span class="o">=</span> <span class="p">[</span>
            <span class="n">Game</span><span class="p">(</span>
                <span class="n">name</span><span class="o">=</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">,</span>
                <span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">100</span><span class="p">),</span>
                <span class="n">command</span><span class="o">=</span><span class="p">[</span><span class="s2">&quot;supertux2&quot;</span><span class="p">],</span>
            <span class="p">),</span>
            <span class="n">Game</span><span class="p">(</span>
                <span class="n">name</span><span class="o">=</span><span class="s2">&quot;Balloon Shooter&quot;</span><span class="p">,</span>
                <span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">200</span><span class="p">),</span>
                <span class="n">command</span><span class="o">=</span><span class="p">[</span><span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="s2">&quot;/home/.../agdpp/agdpp.py&quot;</span><span class="p">],</span>
            <span class="p">),</span>
        <span class="p">]</span>

    <span class="k">def</span> <span class="nf">get_command</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="nb">min</span><span class="p">(</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">games</span><span class="p">,</span>
            <span class="n">key</span><span class="o">=</span><span class="k">lambda</span> <span class="n">game</span><span class="p">:</span> <span class="n">game</span><span class="o">.</span><span class="n">distance_to</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">cursor</span><span class="p">)</span>
        <span class="p">)</span><span class="o">.</span><span class="n">command</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">game</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">games</span><span class="p">:</span>
            <span class="n">game</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="n">loop</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">cursor</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;pink&quot;</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
And here is the `Game` class:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">name</span><span class="p">,</span> <span class="n">position</span><span class="p">,</span> <span class="n">command</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">name</span> <span class="o">=</span> <span class="n">name</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="n">position</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">command</span> <span class="o">=</span> <span class="n">command</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">,</span> <span class="n">text</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">name</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">distance_to</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">point</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">distance_to</span><span class="p">(</span><span class="n">point</span><span class="p">)</span>
</pre></div>
</div></div>
This implementation still passes all tests and is also generalized. Nice!

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ git commit -a -m &#39;Run the command closest to the cursor.&#39;
[main 921c71f] Run the command closest to the cursor.
 1 file changed, 64 insertions(+), 7 deletions(-)
</pre></div>
</div></div>
## Cursor movement

Next I want to work on cursor movement so that we can actually select
different games.

I'm not quite sure how to write a low-level test for this in `GameScene`, so I
write a top-level test instead:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; StartupApplication.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_joystick_motion(axis=1, value=1.0)],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ],</span>
<span class="sd">...     iterations=1</span>
<span class="sd">... ).filter(&quot;DRAW_CIRCLE&quot;)</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 500</span>
<span class="sd">    radius: 20</span>
<span class="sd">    color: &#39;pink&#39;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 500</span>
<span class="sd">    y: 501</span>
<span class="sd">    radius: 20</span>
<span class="sd">    color: &#39;pink&#39;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We assert that the cursor is drawn in two different positions given a joystick
motion event.

The gist of the implementation is here:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_joystick_motion</span><span class="p">():</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">get_axis</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">dx</span> <span class="o">=</span> <span class="n">event</span><span class="o">.</span><span class="n">get_value</span><span class="p">()</span>
            <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">get_axis</span><span class="p">()</span> <span class="o">==</span> <span class="mi">1</span><span class="p">:</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">dy</span> <span class="o">=</span> <span class="n">event</span><span class="o">.</span><span class="n">get_value</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="n">delta</span> <span class="o">=</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">dx</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">dy</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">delta</span><span class="o">.</span><span class="n">length</span><span class="p">()</span> <span class="o">&gt;</span> <span class="mf">0.05</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">delta</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="n">dt</span><span class="p">))</span>
</pre></div>
</div></div>
The `update` method did not exist on `StartupScene` before. The pattern how
it is called is here:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupApplication</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">startup_scene</span><span class="o">.</span><span class="n">event</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">startup_scene</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">startup_scene</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>
</pre></div>
</div></div>
So the scene will receive these calls in order:

* `event`
* `update`
* `draw`

This represents one game loop cycle. If this pattern becomes more permanent, we
can move the top-level test down to `StartupApplication` and have that test
call `event` + `update` and assert that the cursor moved. But for now, I want
the confidence that the high-level test gives, that everything is actually
working together.

I also test this in game to fist of all make sure that I got the axis right and
also to tweak numbers so that speed feels good. The length check is needed
because joystick movement events rarely return a value of 0. If we only move
the joystick a tiny bit, we don't want the cursor to move.

Also, we should probably add constant names for the axis to not compare to
numbers. Maybe `XBOX_AXIS_Y` for example.

Anyway, when I try this out, it actually works. I can move the cursor around,
and when I press `XBOX_A` the game closest to the cursor is started.

## Finishing touches

I want to visualize the game that is closest to the cursor. Let's do it with
another color.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">game</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">games</span><span class="p">:</span>
            <span class="n">game</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="n">loop</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">game_closest_to_cursor</span><span class="p">())</span>

    <span class="k">def</span> <span class="nf">game_closest_to_cursor</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="nb">min</span><span class="p">(</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">games</span><span class="p">,</span>
            <span class="n">key</span><span class="o">=</span><span class="k">lambda</span> <span class="n">game</span><span class="p">:</span> <span class="n">game</span><span class="o">.</span><span class="n">distance_to</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">cursor</span><span class="p">)</span>
        <span class="p">)</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Game</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">,</span> <span class="n">closest</span><span class="p">):</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_text</span><span class="p">(</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">,</span>
            <span class="n">text</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">name</span><span class="p">,</span>
            <span class="n">color</span><span class="o">=</span><span class="s2">&quot;lightblue&quot;</span> <span class="k">if</span> <span class="n">closest</span> <span class="ow">is</span> <span class="bp">self</span> <span class="k">else</span> <span class="s2">&quot;black&quot;</span>
        <span class="p">)</span>
</pre></div>
</div></div>
I modify tests to assert the correct color. This works perfectly.

Next I want to fix the games that are configured. I want them to display
evenly on the screen, and I want to have a "QUIT" game that runs a shutdown
command to shut down the Pi.

Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartupScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">cursor</span> <span class="o">=</span> <span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">400</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">300</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">games</span> <span class="o">=</span> <span class="p">[</span>
            <span class="n">Game</span><span class="p">(</span>
                <span class="n">name</span><span class="o">=</span><span class="s2">&quot;SuperTux&quot;</span><span class="p">,</span>
                <span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">100</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">100</span><span class="p">),</span>
                <span class="n">command</span><span class="o">=</span><span class="p">[</span><span class="s2">&quot;supertux2&quot;</span><span class="p">],</span>
            <span class="p">),</span>
            <span class="n">Game</span><span class="p">(</span>
                <span class="n">name</span><span class="o">=</span><span class="s2">&quot;Balloon Shooter&quot;</span><span class="p">,</span>
                <span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">400</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">300</span><span class="p">),</span>
                <span class="n">command</span><span class="o">=</span><span class="p">[</span><span class="s2">&quot;python3&quot;</span><span class="p">,</span> <span class="s2">&quot;agdpp.py&quot;</span><span class="p">],</span>
            <span class="p">),</span>
            <span class="n">Game</span><span class="p">(</span>
                <span class="n">name</span><span class="o">=</span><span class="s2">&quot;QUIT&quot;</span><span class="p">,</span>
                <span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">1000</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">600</span><span class="p">),</span>
                <span class="n">command</span><span class="o">=</span><span class="p">[</span><span class="s2">&quot;shutdown&quot;</span><span class="p">,</span> <span class="s2">&quot;now&quot;</span><span class="p">],</span>
            <span class="p">),</span>
        <span class="p">]</span>
</pre></div>
</div></div>
And it looks like this:

<p>
<center>
![Final startup screen.](final.png)
</center>
</p>

## Trying on the Pi

I change the startup script, `/etc/xdg/autostart/game_console_start.desktop`,
to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>[Desktop Entry]
Name=Game console start
Exec=/home/pi/game_console_pc.sh
</pre></div>
</div></div>
Where `/home/pi/game_console_pc.sh` is this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="ch">#!/usr/bin/env bash</span>

<span class="nb">exec</span> &gt; /home/pi/game_console_pc.log

<span class="nb">exec</span> <span class="m">2</span>&gt;<span class="p">&amp;</span><span class="m">1</span>

<span class="nb">cd</span> /home/pi/agdpp

<span class="k">for</span> retry <span class="k">in</span> <span class="m">1</span> <span class="m">2</span> <span class="m">5</span> <span class="m">10</span> giveup<span class="p">;</span> <span class="k">do</span>
	<span class="k">if</span> <span class="o">[</span> <span class="nv">$retry</span> <span class="o">=</span> giveup <span class="o">]</span><span class="p">;</span> <span class="k">then</span>
		<span class="nb">echo</span> giving up
		<span class="nb">break</span>
	<span class="k">elif</span> git pull --ff-only<span class="p">;</span> <span class="k">then</span>
		<span class="nb">break</span>
	<span class="k">else</span>
		<span class="nb">echo</span> Retrying <span class="k">in</span> <span class="nv">$retry</span>
		sleep <span class="nv">$retry</span>
	<span class="k">fi</span>
<span class="k">done</span>

python3 startup.py
</pre></div>
</div></div>
And it works beautifully.

Why did I not test drive this startup script? Good question. I for sure spend
some time debugging the loop, which, by the way, is needed to give the Pi time
to connect to the wireless network before it can download the latest version of
the startup application and balloon shooter.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>pi@raspberrypi:~ $ cat game_console_pc.log
fatal: unable to access &#39;https://github.com/rickardlindberg/agdpp.git/&#39;: Could not resolve host: github.com
Retrying in 1
fatal: unable to access &#39;https://github.com/rickardlindberg/agdpp.git/&#39;: Could not resolve host: github.com
Retrying in 2
fatal: unable to access &#39;https://github.com/rickardlindberg/agdpp.git/&#39;: Could not resolve host: github.com
Retrying in 5
Already up to date.
</pre></div>
</div></div>
I feel like this script is maybe not part of the game itself. So that is one
reason why I just "hacked" it together on the Pi. But I'm not entirely happy
that it exists only there, and not in some repo, and doesn't have any tests.

However, for now, it works fine, but there is another problem. It is not
possible to quit the balloon shooter with the gamepad. So once you start it,
you are stuck in it.

## Add balloon shooter quit

I modify `GameScene` by adding a check for `XBOX_START`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">()</span> <span class="ow">or</span> <span class="n">event</span><span class="o">.</span><span class="n">is_joystick_down</span><span class="p">(</span><span class="n">XBOX_START</span><span class="p">):</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>
        <span class="o">...</span>
</pre></div>
</div></div>
And by printing events, I figure out the value of `XBOX_START`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">XBOX_START</span> <span class="o">=</span> <span class="mi">7</span>
</pre></div>
</div></div>
## Summary

Finally, I have the first version of the setup that I had in mind.

I find it a little difficult to document all my thinking in this DevLog format.
I feel like I make hundreds of decisions every minute when programming, and
writing about all of them seems impossible. I think one solution would be to
cover smaller changes in each DevLog. Your questions and commends are very
welcome.

Even if these DevLogs are not valuable to anyone else, they are valuable to me
because I get to practice writing and explaining my thinking.

See you next time!
