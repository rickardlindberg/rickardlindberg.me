---
title: 'DRAFT: Multiplayer'
date: 2023-06-26
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

For every story that we work on, the balloon shooter feels more and more like a
real game. The initial goal of this project was to create a game that me and my
son will enjoy playing *together*. At this point, I think the most valuable
thing we can work on towards that goal is adding support for multiplayer.
That's what we will work on in this episode.

## A new layer

The entry point for the balloon shooter looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="n">BalloonShooter</span><span class="o">.</span><span class="n">create</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
The balloon shooter class instantiates a game scene which implements the logic
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
This means that as soon as we start the game, we enter the gameplay mode and
can start playing right away.

I imagine that multiplayer mode works by first selecting which players should
participate in shooting balloons, and after that, the gameplay mode is entered
and each player get their own bow to shoot with.

We want to go from this structure:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>BalloonShooter
    GameScene
</pre></div>
</div></div>
To something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>BalloonShooter
    NewGameScene
        StartScene
        GameScene
</pre></div>
</div></div>
We want to add another level that first directs calls to a start screen (or
player select screen) and once players are selected, initializes the game scene
and directs call to that.

The current tests for `GameScene` should pass unchanged, but tests for
`BalloonShooter` will need some modifications. I imagine that those tests need
to select a player before asserting something from the gameplay mode. We'll see
later.

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
We insert this new layer in `BalloonShooter` like this:

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

We write the initial version of the start scene like this:

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
The idea is that a player (keyboard or gamepad) selects to be part of the game
by shooting. When all players have entered, one of them can shoot again to
start the game. This functionality is not yet fully implemented. But this will
do for now.

When writing this blog post and looking at the code, I notice two problems.
First of all "on" should be "one" in the test description. Second of all, the
implementation does not check events at all, so a test that does not simulate
any events will still pass.  So if we were to take this start scene into play
now, we just need to wait for two iterations (2/60th of a second) and it would
report players `['one'`]. That does not seem correct.

Let's fix that. We modify the test to do two updates and the assertions should
be the same:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; start.event(GameLoop.create_event_joystick_down(XBOX_A))</span>
<span class="sd">&gt;&gt;&gt; start.update(0)</span>
<span class="sd">&gt;&gt;&gt; start.update(0)</span>
<span class="sd">&gt;&gt;&gt; start.get_players() is None</span>
<span class="sd">True</span>

<span class="sd">&gt;&gt;&gt; start.event(GameLoop.create_event_joystick_down(XBOX_A))</span>
<span class="sd">&gt;&gt;&gt; start.update(0)</span>
<span class="sd">&gt;&gt;&gt; start.update(0)</span>
<span class="sd">&gt;&gt;&gt; start.get_players()</span>
<span class="sd">[&#39;one&#39;]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
I wonder how common the event + update pattern is in our tests. Perhaps we can
benefit from a test helper something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">cycle</span><span class="p">(</span><span class="n">sprite</span><span class="p">,</span> <span class="n">events</span><span class="o">=</span><span class="p">[],</span> <span class="n">dt</span><span class="o">=</span><span class="mi">0</span><span class="p">):</span>
    <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
        <span class="n">sprite</span><span class="o">.</span><span class="n">event</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>
    <span class="n">sprite</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
    <span class="n">sprite</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
</pre></div>
</div></div>
We might try it in a few places and see if the tests read better. But not now.
The modification to the tests forces us to check events. We do it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">StartScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">input_handler</span><span class="o">.</span><span class="n">get_shoot</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">shots</span> <span class="o">+=</span> <span class="mi">1</span>

    <span class="o">...</span>
</pre></div>
</div></div>
With that fix out of the way, let's work on integrating the start scene.

## Take start scene into play

The game scene currently forwards all calls to the gameplay scene. To take the
start scene into play, we first want the start scene to be active, and have the
game scene forward calls to it. Once players have been selected, we want the
game scene to switch the active scene to the gameplay scene.

We express that in the following test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    Initially, I draw the start scene:</span>

<span class="sd">    &gt;&gt;&gt; game = GameScene(screen_area=Rectangle.from_size(500, 500))</span>
<span class="sd">    &gt;&gt;&gt; isinstance(game.active_scene, StartScene)</span>
<span class="sd">    True</span>

<span class="sd">    When players have been selected, I draw the gameplay scene:</span>

<span class="sd">    &gt;&gt;&gt; game.event(GameLoop.create_event_keydown(KEY_SPACE))</span>
<span class="sd">    &gt;&gt;&gt; game.update(0)</span>
<span class="sd">    &gt;&gt;&gt; isinstance(game.active_scene, StartScene)</span>
<span class="sd">    True</span>

<span class="sd">    &gt;&gt;&gt; game.event(GameLoop.create_event_keydown(KEY_SPACE))</span>
<span class="sd">    &gt;&gt;&gt; game.update(0)</span>
<span class="sd">    &gt;&gt;&gt; isinstance(game.active_scene, StartScene)</span>
<span class="sd">    False</span>
<span class="sd">    &quot;&quot;&quot;</span>
</pre></div>
</div></div>
This is an example of an overlapping, sociable test. To make the scene switch
happen, we need `StartScene.get_players` to return something. Since the game
scene uses the real start scene, and not a mock, the only way to make it return
something is to perform the same actions as we did in the start scene tests.

To make this test pass, we initialize an active scene variable to the start
scene and switch it to the gameplay scene once we have selected players:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">screen_area</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">screen_area</span> <span class="o">=</span> <span class="n">screen_area</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">active_scene</span> <span class="o">=</span> <span class="n">StartScene</span><span class="p">(</span><span class="n">screen_area</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">screen_area</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">active_scene</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">active_scene</span><span class="p">,</span> <span class="n">StartScene</span><span class="p">):</span>
            <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">active_scene</span><span class="o">.</span><span class="n">get_players</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">active_scene</span> <span class="o">=</span> <span class="n">GameplayScene</span><span class="p">(</span><span class="n">screen_area</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">screen_area</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
The test talks about switching to a gameplay scene, but it only asserts that
the start scene is *not* active anymore. We could probably clarify that.

I'm also not sure how I feel about the assertions that checks the type of the
active scene. But I don't have any ideas for a better way to express that. If
you do, please let me know.

When we run the game now it shows a blank purple screen. If we shoot twice we
enter the gameplay scene and the game starts as before. Perfect!

We do not yet take players into account and we can still not have multiple
players. What we do have is a skeleton with a few more pieces where this new
functionality will fit.

The game works fine now (if we know that we have to shoot twice to get passed
the start screen), but a test fails. It is the test for the balloon shooter.
Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="sd">&quot;&quot;&quot;</span>
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

<span class="sd">    The game loop is initialized and cleaned up:</span>

<span class="sd">    &gt;&gt;&gt; events.filter(&quot;GAMELOOP_INIT&quot;, &quot;GAMELOOP_QUIT&quot;)</span>
<span class="sd">    GAMELOOP_INIT =&gt;</span>
<span class="sd">        resolution: (1280, 720)</span>
<span class="sd">        fps: 60</span>
<span class="sd">    GAMELOOP_QUIT =&gt;</span>

<span class="sd">    ...</span>
<span class="sd">    &quot;&quot;&quot;</span>
</pre></div>
</div></div>
This test is at the outermost level, so it includes all objects. Before, the
gameplay scene received the events from the test, but now the start scene
receives them. The start scene does not handle the user closed window event
which results in this test just hanging.

That failure teaches us that we can't quit the application when we are in the
start scene, only when we are in the gameplay scene. That is probably not
correct. Thank you test for pointing that out. However, the assertions that
follow check for example that a balloon is drawn, so the test expects to be in
the gameplay mode. We modify the test to include two shoot events so that we
end up in the gameplay scene:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; events = BalloonShooter.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [GameLoop.create_event_keydown(KEY_SPACE)],</span>
<span class="sd">...         [GameLoop.create_event_keydown(KEY_SPACE)],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And, we are back to green!

Here is yet another example of overlapping, sociable testing. We yet again have
to simulate two shoot events to select players.

One downside of this approach is that if we were to change the logic for
selecting players. Say that we first need to shoot and the turn left. Then we
would have to modify three test I think. One way to make that less of a problem
in this particular situation is to create a test helper something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">events_to_select_one_player</span><span class="p">():</span>
    <span class="k">return</span> <span class="p">[</span>
        <span class="n">GameLoop</span><span class="o">.</span><span class="n">create_event_keydown</span><span class="p">(</span><span class="n">KEY_SPACE</span><span class="p">),</span>
        <span class="n">GameLoop</span><span class="o">.</span><span class="n">create_event_keydown</span><span class="p">(</span><span class="n">KEY_SPACE</span><span class="p">),</span>
    <span class="p">]</span>
</pre></div>
</div></div>
We could use that test helper in all tests (with some modification) and now
there is only one place in the tests that knows about what events that gets us
from the start scene to the gameplay scene with one player.

## Players to game scene

Our skeleton for the new feature is not quite complete. The gameplay scene does
not know about players. Let's fix that by passing the players from the start
scene to the gameplay scene like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gu">@@ -162,7 +162,10 @@ class GameScene:</span>
         self.active_scene.update(dt)
         if isinstance(self.active_scene, StartScene):
             if self.active_scene.get_players():
<span class="gd">-                self.active_scene = GameplayScene(screen_area=self.screen_area)</span>
<span class="gi">+                self.active_scene = GameplayScene(</span>
<span class="gi">+                    screen_area=self.screen_area,</span>
<span class="gi">+                    players=self.active_scene.get_players()</span>
<span class="gi">+                )</span>
</pre></div>
</div></div>
To make this work we also add that argument to the constructor:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gu">@@ -333,11 +336,13 @@ class GameplayScene(SpriteGroup):</span>
     []
     &quot;&quot;&quot;

<span class="gd">-    def __init__(self, screen_area, balloons=[], arrows=[]):</span>
<span class="gi">+    def __init__(self, screen_area, balloons=[], arrows=[], players=[&quot;default&quot;]):</span>
</pre></div>
</div></div>
Now, I think our skeleton is complete. What do I mean by that? I mean that all
the pieces are connected they way we think they should be. Now we can work
individually on the start scene and the gameplay scene. The start scene needs
to be able to select multiple players and should return those players in the
list. The gameplay scene should take players into account and create one bow
per player that it can control.

## Flesh out

* Start scene can select multiple players.
* Game scene creates multiple bows.
* Input handler handles multiple players.

```
commit 9804dce5d6f789274161a7b2c84a36f43cd33c23
Author: Rickard Lindberg <rickard@rickardlindberg.me>
Date:   Sun May 7 14:31:38 2023 +0200

    Rename GameScene -> GameplayScene (so that we can create a StartScene and a GameScene to coordinate the two.)

...

commit af8d01b4ba7cce46dcd223309e26a79a43515348
Author: Rickard Lindberg <rickard@rickardlindberg.me>
Date:   Sun May 7 17:01:46 2023 +0200

    Bows are layed out evenly at the bottom of the screen.
```

When we run the game now, it greets us with an empty start scene:

<p>
<center>
![Empty start scene.](empty-start.png)
</center>
</p>

I shoot once with the keyboard, then twice with the gamepad and am taken to
this scene where the keyboard and the gamepad can control their own bow:

<p>
<center>
![First version of multiplayer.](multiplayer-first.png)
</center>
</p>

And we have the first version of a working multiplayer mode!

## Polishing

* got carried away and improved the start screen
    * was able to reuse Balloons just for the animation!

<p>
<center>
![Start scene with instructions.](start-instructions.png)
</center>
</p>

<p>
<center>
![Players with different colors.](multiplayer-colors.png)
</center>
</p>

## Summary

* want to go play sometimes
    * that feeling when you have created something and want to go have a
      look at it
    * same feeling as when I made websites 20 years ago
    * that feeling!

See you in the next episode!
