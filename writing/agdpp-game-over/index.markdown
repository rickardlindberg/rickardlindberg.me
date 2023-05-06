---
title: Game over?
date: 2023-05-06
tags: agdpp
agdpp: true
---

When we worked on [shooting the
arrow](/writing/agdpp-shooting-arrow/index.html) we concluded that it was
tedious to restart the game after each shot. When the arrow goes outside the
screen, we want the game to be over instead and the arrow to be reset. Let's
work on that in this episode.

## Do we really need game over?

If we implement game over now, there will be game over after every shot.
Because there is no way to hit the balloon just yet.

If you play a game where it is game over immediately, would you enjoy it?

Perhaps game over is not the right story to work on? It is a solution to the
problem that you don't have any arrows to shoot after the first one.

How about if you get a new arrow immediately? So you can just keep firing?

From before, these are the stories we though about as needed for an initial
balloon shooter:

* Balloon moves downwards
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* Real graphics instead of circles

Let's think about this. For minimal, I don't think we need real graphics. The
circles convey the idea just fine.

* Balloon moves downwards
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* ~~Real graphics instead of circles~~

I'm not sure the balloon needs to move downwards either. The current movement
pattern is fine.

* ~~Balloon moves downwards~~
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* ~~Real graphics instead of circles~~

And we can do something else instead of game over.

* ~~Balloon moves downwards~~
* Arrow can hit balloon
* Point is given for hit
* ~~Game over when miss~~
* ~~Real graphics instead of circles~~

That leaves us with this:

* Arrow can hit balloon
* Point is given for hit
* New arrow when the current one has been shot

We can always make something smaller. And what we initially thought we needed,
we don't need. At least not yet. When we play the game, we quite quickly find
out what is needed next. Working software. In the hands of its users. Powerful.

Let's work on spawning arrows now so that we can enjoy shooting arrows for a
longer time without having to restart our game.

## Acceptance criteria

I can think of two test:

* You get a new arrow when you shoot the current one
* When an arrow goes outside the screen, we stop rendering it

The second test is kind of internal. If we render thousands of arrows outside
the screen, no one will notice. Until there is a performance issue or an out of
memory crash or something like that.

On the other hand, it makes sense, from a gameplay perspective, to talk about
arrows going off the screen as being deactivated. Otherwise it might be that
they come back after a while, but now instead move downwards.

## How to write the tests?

All tests for our game are currently written at the top-level. Here is the test
that checks for behavior when we press the space key:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">We run the game for a few frames, press the space key, let it run for a few</span>
<span class="sd">frames, then quit:</span>

<span class="sd">&gt;&gt;&gt; events = BalloonShooter.run_in_test_mode(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_keydown_space()],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [GameLoop.create_event_user_closed_window()],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>

<span class="sd">The arrow moves:</span>

<span class="sd">&gt;&gt;&gt; arrow_head_positions = events.filter(&quot;DRAW_CIRCLE&quot;, radius=10).collect(&quot;x&quot;, &quot;y&quot;)</span>
<span class="sd">&gt;&gt;&gt; len(arrow_head_positions) &gt; 1</span>
<span class="sd">True</span>
<span class="sd">&gt;&gt;&gt; len(set(arrow_head_positions)) &gt; 1</span>
<span class="sd">True</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We filter out `DRAW_CIRCLE` events with radius 10 with the assumption that the
only circle drawn with radius 10 is the arrow head.

This test assumes only one arrow.

If we were to draw another arrow, there would be no way of identifying the two
different arrows in this test.

So writing the new tests at this level feels difficult and error prone.

Testing is so hard.

Let's see if we can make a new attempt at extracting a subsystem where this new
behavior is easier to test.

## Sprite group and game scene refactoring

Our game currently looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span> <span class="o">=</span> <span class="n">Balloon</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="n">Arrow</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span> <span class="o">=</span> <span class="p">[</span><span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="p">]</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
            <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
        <span class="k">for</span> <span class="n">sprite</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span><span class="p">:</span>
            <span class="n">sprite</span><span class="o">.</span><span class="n">tick</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
        <span class="k">for</span> <span class="n">sprite</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span><span class="p">:</span>
            <span class="n">sprite</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
I have an idea for how to push much of this logic down one level so that it can
more easily be tested. Let's give it a try.

We keep all our sprites in a list. Managing a list of sprites seems like a good
job for a new class. We create a `SpriteGroup` class that works like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; class TestSprite:</span>
<span class="sd">...     def update(self, dt):</span>
<span class="sd">...         print(f&quot;TEST SPRITE update {dt}&quot;)</span>
<span class="sd">...     def draw(self, loop):</span>
<span class="sd">...         print(f&quot;TEST SPRITE draw {loop}&quot;)</span>

<span class="sd">&gt;&gt;&gt; group = SpriteGroup([TestSprite()])</span>
<span class="sd">&gt;&gt;&gt; x = TestSprite()</span>
<span class="sd">&gt;&gt;&gt; y = group.add(x)</span>
<span class="sd">&gt;&gt;&gt; x is y</span>
<span class="sd">True</span>

<span class="sd">&gt;&gt;&gt; group.update(4)</span>
<span class="sd">TEST SPRITE update 4</span>
<span class="sd">TEST SPRITE update 4</span>

<span class="sd">&gt;&gt;&gt; group.draw(None)</span>
<span class="sd">TEST SPRITE draw None</span>
<span class="sd">TEST SPRITE draw None</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
A sprite is any object that can respond to `update` and `draw` calls. When we
update and draw the sprite group, it calls the corresponding methods on all its
sprites.

We can use this new class in our game like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">BalloonShooter</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span> <span class="o">=</span> <span class="n">Balloon</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="n">Arrow</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">all_sprites</span> <span class="o">=</span> <span class="n">SpriteGroup</span><span class="p">([</span><span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="p">])</span>

    <span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
            <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">all_sprites</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">all_sprites</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>

    <span class="o">...</span>
</pre></div>
</div></div>
Not much of a difference. Mainly we moved the looping over the sprites from our
game to the sprite group class. I think this is a bit cleaner, but it makes
testing no easier.

But we are not done yet. Now, let's extract a lower level object that we call
`GameScene`.

The init of the game then changes to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span> <span class="o">=</span> <span class="n">GameScene</span><span class="p">()</span>
</pre></div>
</div></div>
And the tick method changes to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">tick</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">,</span> <span class="n">events</span><span class="p">):</span>
    <span class="k">for</span> <span class="n">event</span> <span class="ow">in</span> <span class="n">events</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span><span class="o">.</span><span class="n">event</span><span class="p">(</span><span class="n">event</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="n">dt</span><span class="p">)</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">clear_screen</span><span class="p">()</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">game_scene</span><span class="o">.</span><span class="n">draw</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="p">)</span>
</pre></div>
</div></div>
That is, we defer event handling, updating, and drawing to the game scene.

The game scene looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Balloon</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Arrow</span><span class="p">())</span>

    <span class="k">def</span> <span class="nf">event</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
            <span class="k">raise</span> <span class="n">ExitGameLoop</span><span class="p">()</span>
        <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">is_keydown_space</span><span class="p">():</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
</pre></div>
</div></div>
It inherits from `SpriteGroup` so it gets the `update` and `draw` "for free".
And the event handling code is extracted from game. (Since the `event` method
does not have access to the game loop, we can't call `loop.quit()` to exit, so
we instead raise the exception that `loop.quit()` would have raised. Initially
I thought it would be nice if the loop hide the quit mechanism, and so I did
not want to expose the exception. That made this part difficult to write, so I
reverted that decision. We constantly adapt the design to the current needs.
Perhaps hiding was not the right decision? Or perhaps it was. This will do for
now.)

Now that we have a new lower-level object, has testing become any easier?

## Slow progress

I feel like this feature we are working on is quite easy to implement. It will
just be a couple of lines of code. Yet, here we are many hours into a sprite
group refactoring that we are not sure will even pay off. Why? Only so that we
can write a test that "allow" us to write those couple of lines that actually
implement this feature.

When we work in an agile way, we constantly change our software, and having a
good safety net in the form of a test suite allows us to make changes
confidently.

But if it always takes x minutes to write the test and x/10 minutes to
implement the thing, is it really worth it?

My suspicion and hope is that testing time varies. When a feature requires a
design change, things will take a little longer. With a new design in place,
new features can more easily be added (and tested). Until another design
challenge comes a long.

## State based testing

Let's see how we can test our lower-level game scene object. It is now
responsible for some behavior that the balloon shooter class was previously
responsible for, so we should be able to write some tests that check the same
behavior.

Let's try initial state: the balloon should animate and the arrow should stay
still. Here are tests for that:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    Initial state</span>
<span class="sd">    =============</span>

<span class="sd">    The balloon animates:</span>

<span class="sd">    &gt;&gt;&gt; game = GameScene()</span>
<span class="sd">    &gt;&gt;&gt; first_position = game.get_balloon_position()</span>
<span class="sd">    &gt;&gt;&gt; game.update(10)</span>
<span class="sd">    &gt;&gt;&gt; second_position = game.get_balloon_position()</span>
<span class="sd">    &gt;&gt;&gt; first_position == second_position</span>
<span class="sd">    False</span>

<span class="sd">    The arrow stays still:</span>

<span class="sd">    &gt;&gt;&gt; game = GameScene()</span>
<span class="sd">    &gt;&gt;&gt; first_position = game.get_arrow_position()</span>
<span class="sd">    &gt;&gt;&gt; game.update(10)</span>
<span class="sd">    &gt;&gt;&gt; second_position = game.get_arrow_position()</span>
<span class="sd">    &gt;&gt;&gt; first_position == second_position</span>
<span class="sd">    True</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="o">...</span>
</pre></div>
</div></div>
We create a game scene, query some of its state, update it, query some it its
state again and make some assertions.

When we wrote these test at the balloon shooter level, we had to assert that
circles were drawn in specific locations. In this test, no drawing is involved.

In order for the tests above to work, we have to write getters to expose some
internal state:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_balloon_position</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">balloon</span><span class="o">.</span><span class="n">get_position</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">get_arrow_position</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">get_position</span><span class="p">()</span>
</pre></div>
</div></div>
These are only used in tests.

For a long time, I was reluctant do this. Mainly because I've been taught that
objects should not expose internals to the outside world. That is bad object
oriented design.

But [James
writes](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#visible-behavior)

> For mutable objects, provide a way for changes in state to be observed,
> either with a getter method or an event.

So if we don't want to use mocks (which we are practicing), exposing state via
getters is probably fine.

One thing that we are not testing with these new tests is that the balloon and
the arrow are actually drawn at the positions that are returned by the getters.

We could probably write tests where we call the draw method as well and observe
`DRAW_CIRCLE` events and see that they match. But I think the trade off is not
worth it in this case. We still have the top-level tests that check that
things are drawn on the screen, and the likelihood that we don't draw at the
position that the getter returns is quite small I think.

Anyway, now that we are (mostly) fine with writing getters to expose internal
state, testing should be a little smoother.

## Tests for new arrow behavior

Let's start with the initial state. We introduce the concept of flying arrows
(arrows that have been shot) and check that there aren't any in the beginning:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene()</span>
<span class="sd">&gt;&gt;&gt; game.get_flying_arrows()</span>
<span class="sd">[]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We make it work like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">())</span>

    <span class="k">def</span> <span class="nf">get_flying_arrows</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">()</span>

    <span class="o">...</span>
</pre></div>
</div></div>
We also add the `get_sprites` getter in the `SpriteGroup` class. Again, this
getter is only used in tests. This bothers me again. Not that it is only used
in tests, but that this feels like bad object oriented design. Perhaps it would
be cleaner it the sprite group only provided a `get_count` method? Or something
more specific instead of just exposing its internal collection.

But we are fine with exposing internal state for testing purposes. So we don't
think too much about it now. But let's keep it in the back of our minds for the
future.

Let's move on to shooting so that we get some flying arrows. Here is the test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene()</span>
<span class="sd">&gt;&gt;&gt; initial_position = game.get_arrow_position()</span>
<span class="sd">&gt;&gt;&gt; game.event(GameLoop.create_event_keydown_space())</span>
<span class="sd">&gt;&gt;&gt; game.update(10)</span>

<span class="sd">It makes the arrow fire:</span>

<span class="sd">&gt;&gt;&gt; flying = game.get_flying_arrows()</span>
<span class="sd">&gt;&gt;&gt; len(flying)</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; flying[0].get_position() == initial_position</span>
<span class="sd">False</span>

<span class="sd">The initial arrow stays the same:</span>

<span class="sd">&gt;&gt;&gt; game.get_arrow_position() == initial_position</span>
<span class="sd">True</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We simulate a shot by sending a space keydown event followed by an update. We
assert that we now have a flying arrow and that its position is not the
original position of the arrow (it has moved). Furthermore we assert that the
current arrow position is the same as the initial meaning that we still have an
arrow that we can shoot.

The implementation: change the event handler for keydown space from this

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="bp">self</span><span class="o">.</span><span class="n">arrow</span><span class="o">.</span><span class="n">shoot</span><span class="p">()</span>
</pre></div>
</div></div>
to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Arrow</span><span class="p">(</span><span class="n">shooting</span><span class="o">=</span><span class="kc">True</span><span class="p">))</span>
</pre></div>
</div></div>
So the arrow that we shoot actually stays the same and we create a new arrow
instance which will be the one shot.

At this point we can actually shoot multiple arrows in the game:

<center>
![Multiple arrows.](multiple-arrows.png)
</center>

## Remove arrows outside screen

We are almost there. But if we keep running the game for long enough, we will
get an out of memory error. So we need to remove arrows that go outside the
screen.

We write this test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; game = GameScene(space)</span>
<span class="sd">&gt;&gt;&gt; game.event(GameLoop.create_event_keydown_space())</span>
<span class="sd">&gt;&gt;&gt; game.update(10000)</span>
<span class="sd">&gt;&gt;&gt; game.get_flying_arrows()</span>
<span class="sd">[]</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
If we decrease the number in `update` the flying arrows collection will not be
empty because the arrow that we shoot has not had time to fly off screen yet.

We make this test pass by overriding the `update` method of the sprite group
and doing the collision detection to remove flying arrows outside the screen:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">)</span>
        <span class="k">for</span> <span class="n">x</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
            <span class="k">if</span> <span class="n">x</span><span class="o">.</span><span class="n">hits_space</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">space</span><span class="p">):</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">sprites</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">x</span><span class="p">)</span>
</pre></div>
</div></div>
Now the `get_sprites` method that we wrote before, for testing purposes, comes
in handy. Iterating over sprites in a collection seems like a reasonable
behavior. I still don't think we should expose the internal collection. That is
bad. But we could expose some kind of iterator.

Oh, and to remove the sprite, we actually reach into the fields of the flying
arrows group and call it's `remove` method. Yikes. But the tests pass. Let's
commit and see if we can improve this.

We come up wit this instead:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
    <span class="n">SpriteGroup</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">)</span>
    <span class="k">for</span> <span class="n">arrow</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
        <span class="k">if</span> <span class="n">arrow</span><span class="o">.</span><span class="n">hits_space</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">space</span><span class="p">):</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">arrow</span><span class="p">)</span>
</pre></div>
</div></div>
`x` is not a very meaningful name, so we call it `arrow` instead. Then we call
a new `remove` method on the sprite group. No more reaching into internal
fields and modifying them.

The sprite group looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">SpriteGroup</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_sprites</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="nb">list</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">sprites</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">remove</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">sprite</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">sprites</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">sprite</span><span class="p">)</span>
</pre></div>
</div></div>
We ensure that `get_sprites` returns a new list so that the internal list is
never exposed. This has two benefits:

1. The sprite group is in control of its own collection. No one on the outside
   can modify it.

2. It is safe to call `remove` at any time. Before we removed sprites from the
   collection we were iterating over. That is, in general, is a bad idea.

I didn't cover how the collision detection works or what `space` is. If you are
curious, check out the details in [this
commit](https://github.com/rickardlindberg/agdpp/commit/4956769829b3426c9f0bb3fbe48ccde3150ca5a7).
The complete source code from this episode is on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/shoot-multiple-arrows).

## Summary

The big breakthrough in this episode was the realization that it's OK to write
getters to expose internal state for testing purposes. We saw that one of those
getters turned out to be useful for the production code as well. I think this
will make testing easier, and we will try to write as few getters as possible
and only expose "sane" state. We still want to do good object oriented design.

See you in the next episode!
