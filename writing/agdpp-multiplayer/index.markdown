---
title: 'DRAFT: Multiplayer'
date: 2023-06-24
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

For every story we work on, the balloon shooter feels more and more like a real
game. The initial goal of this project was to create a game that me and my son
will enjoy playing together. At this point, I think the most valuable thing we
can work on is adding support for multiplayer. That's what we will work on in
this episode.

## A new layer

The entry point for the balloon shooter looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="n">BalloonShooter</span><span class="o">.</span><span class="n">create</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
The balloon shooter class instantiates a `GameScene` which implements the logic
of our game:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span> <span class="o">=</span> <span class="n">GameScene</span><span class="p">(</span><span class="n">Rectangle</span><span class="o">.</span><span class="n">from_size</span><span class="p">(</span><span class="o">*</span><span class="bp">self</span><span class="o">.</span><span class="n">resolution</span><span class="p">))</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
This means that as soon as we start the application, we enter the gameplay mode
and can start playing right away.

I imagine that multiplayer mode works by first selecting which players should
participate in shooting balloons, and after that, the gameplay mode is entered.

We want to go from this structure

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>BalloonShooter
    GameScene
</pre></div>
</div></div>
to something like

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>BalloonShooter
    NewGameScene
        StartScene
        GameScene
</pre></div>
</div></div>
We want to add another level that first directs calls to a start screen (or
player select screen) and once that is done, initializes the game scene.

Current test for `GameScene` should pass unchanged, but tests for
`BalloonShooter` will need some modifications. I imagine that those tests need
to select a player before asserting something from the gameplay mode.

## Refactor to new structure

Let's start by slowly and carefully refactor towards this new structure, using
our tests as a safety net to give us feedback about how we're doing.

I want to call the new layer `GameScene`, but that name is already taken. The
current game scene is really the gameplay scene, so we rename it to that. Then
we create the new `GameScene` which just forwards its calls to the gameplay
scene:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">screen_area</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">gameplay</span> <span class="o">=</span> <span class="n">GameplayScene</span><span class="p">(</span><span class="n">screen_area</span><span class="o">=</span><span class="n">screen_area</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">gameplay</span><span class="o">.</span><span class="n">event</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">gameplay</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">gameplay</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="n">loop</span><span class="p">)</span>
</pre></div>
</div></div>
And insert it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gu">@@ -113,7 +113,7 @@ class BalloonShooter:</span>
     def __init__(self, loop):
         self.loop = loop
         self.resolution = (1280, 720)
<span class="gd">-        self.game_scene = GameplayScene(Rectangle.from_size(*self.resolution))</span>
<span class="gi">+        self.game_scene = GameScene(screen_area=Rectangle.from_size(*self.resolution))</span>
</pre></div>
</div></div>
The new layer is now added, all tests are passing, and we have a point in our
code (`GameScene`) where we can put functionality to choose between a start
scene and a gameplay scene.

Before we can work on that behavior, we need a start scene.

## Start scene

We write the initial version of `StartScene` like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    I report players when on player has shot twice:</span>

<span class="sd">    &gt;&gt;&gt; start = StartScene(screen_area=Rectangle.from_size(500, 500))</span>
<span class="sd">    &gt;&gt;&gt; start.get_players() is None</span>
<span class="sd">    True</span>

<span class="sd">    &gt;&gt;&gt; start.event(GameLoop.create_event_joystick_down(XBOX_A))</span>
<span class="sd">    &gt;&gt;&gt; start.update(0)</span>
<span class="sd">    &gt;&gt;&gt; start.get_players() is None</span>
<span class="sd">    True</span>

<span class="sd">    &gt;&gt;&gt; start.event(GameLoop.create_event_joystick_down(XBOX_A))</span>
<span class="sd">    &gt;&gt;&gt; start.update(0)</span>
<span class="sd">    &gt;&gt;&gt; start.get_players()</span>
<span class="sd">    [&#39;one&#39;]</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">screen_area</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span> <span class="o">=</span> <span class="n">InputHandler</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">shots</span> <span class="o">=</span> <span class="mi">0</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">action</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">shots</span> <span class="o">+=</span> <span class="mi">1</span>

    <span class="k">def</span> <span class="nf">get_players</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">shots</span> <span class="o">&gt;</span> <span class="mi">1</span><span class="p">:</span>
            <span class="k">return</span> <span class="p">[</span><span class="s2">&quot;one&quot;</span><span class="p">]</span>
</pre></div>
</div></div>
The idea is that a player (keyboard, gamepad) selects to be part of the game by
shooting. When all players have entered, one of them can shoot again to start
the game. This is not yet complete.

When writing this blog post and looking at the code, I notice two problems.
First of all "on" should be "one" in the test description. Second of all, the
implementation does not check events at all, so this test, for example, will
pass:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; start = StartScene(screen_area=Rectangle.from_size(500, 500))</span>
<span class="sd">&gt;&gt;&gt; start.update(0)</span>
<span class="sd">&gt;&gt;&gt; start.get_players() is None</span>
<span class="sd">True</span>
<span class="sd">&gt;&gt;&gt; start.update(0)</span>
<span class="sd">&gt;&gt;&gt; start.get_players()</span>
<span class="sd">[&#39;one&#39;]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
So if we were to take this start scene into play now, we just need to wait for
two iterations (2/60th of a second) and it would report players `['one'`]. That
does not seem correct, but we can get back to it later.

Let's work on integrating the start scene.

## Take start scene into play

## Summary

See you in the next episode!

## TODO

    commit 9804dce5d6f789274161a7b2c84a36f43cd33c23
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 14:31:38 2023 +0200

        Rename GameScene -> GameplayScene (so that we can create a StartScene and a GameScene to coordinate the two.)

    ...

    commit af8d01b4ba7cce46dcd223309e26a79a43515348
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 17:01:46 2023 +0200

        Bows are layed out evenly at the bottom of the screen.

    * got carried away and improved the start screen

        * was able to reuse Balloons just for the animation!

    commit bc7d1ca865a51b47e8efd0004dca288ebcc9ec33
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 17:20:36 2023 +0200

        Start screen draws helpful text.

    ...

    commit 4b03cc6b896b716e2ccbf546f600e49913cbfada (HEAD -> main, origin/main)
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 17:58:33 2023 +0200

        Nicer looking start screen.

    * want to go play sometimes
        * that feeling when you have created something and want to go have a
          look at it
        * same feeling as when I made websites 20 years ago
        * that feeling!

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>