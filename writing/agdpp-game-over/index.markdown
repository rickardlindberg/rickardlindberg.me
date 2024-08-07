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

```python
"""
We run the game for a few frames, press the space key, let it run for a few
frames, then quit:

>>> events = BalloonShooter.run_in_test_mode(
...     events=[
...         [],
...         [],
...         [GameLoop.create_event_keydown_space()],
...         [],
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )

The arrow moves:

>>> arrow_head_positions = events.filter("DRAW_CIRCLE", radius=10).collect("x", "y")
>>> len(arrow_head_positions) > 1
True
>>> len(set(arrow_head_positions)) > 1
True
"""
```

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

```python
class BalloonShooter:

    def __init__(self, loop):
        ...
        self.balloon = Balloon()
        self.arrow = Arrow()
        self.sprites = [self.balloon, self.arrow]

    def tick(self, dt, events):
        for event in events:
            if event.is_user_closed_window():
                self.loop.quit()
            elif event.is_keydown_space():
                self.arrow.shoot()
        for sprite in self.sprites:
            sprite.tick(dt)
        self.loop.clear_screen()
        for sprite in self.sprites:
            sprite.draw(self.loop)

    ...
```

I have an idea for how to push much of this logic down one level so that it can
more easily be tested. Let's give it a try.

We keep all our sprites in a list. Managing a list of sprites seems like a good
job for a new class. We create a `SpriteGroup` class that works like this:

```python
"""
>>> class TestSprite:
...     def update(self, dt):
...         print(f"TEST SPRITE update {dt}")
...     def draw(self, loop):
...         print(f"TEST SPRITE draw {loop}")

>>> group = SpriteGroup([TestSprite()])
>>> x = TestSprite()
>>> y = group.add(x)
>>> x is y
True

>>> group.update(4)
TEST SPRITE update 4
TEST SPRITE update 4

>>> group.draw(None)
TEST SPRITE draw None
TEST SPRITE draw None
"""
```

A sprite is any object that can respond to `update` and `draw` calls. When we
update and draw the sprite group, it calls the corresponding methods on all its
sprites.

We can use this new class in our game like this:

```python
class BalloonShooter:

    def __init__(self, loop):
        ...
        self.balloon = Balloon()
        self.arrow = Arrow()
        self.all_sprites = SpriteGroup([self.balloon, self.arrow])

    def tick(self, dt, events):
        for event in events:
            if event.is_user_closed_window():
                self.loop.quit()
            elif event.is_keydown_space():
                self.arrow.shoot()
        self.all_sprites.update(dt)
        self.loop.clear_screen()
        self.all_sprites.draw(self.loop)

    ...
```

Not much of a difference. Mainly we moved the looping over the sprites from our
game to the sprite group class. I think this is a bit cleaner, but it makes
testing no easier.

But we are not done yet. Now, let's extract a lower level object that we call
`GameScene`.

The init of the game then changes to this:

```python
def __init__(self, loop):
    ...
    self.game_scene = GameScene()
```

And the tick method changes to this:

```python
def tick(self, dt, events):
    for event in events:
        self.game_scene.event(event)
    self.game_scene.update(dt)
    self.loop.clear_screen()
    self.game_scene.draw(self.loop)
```

That is, we defer event handling, updating, and drawing to the game scene.

The game scene looks like this:

```python
class GameScene(SpriteGroup):

    def __init__(self):
        SpriteGroup.__init__(self)
        self.balloon = self.add(Balloon())
        self.arrow = self.add(Arrow())

    def event(self, event):
        if event.is_user_closed_window():
            raise ExitGameLoop()
        elif event.is_keydown_space():
            self.arrow.shoot()
```

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

```python
class GameScene(SpriteGroup):

    """
    Initial state
    =============

    The balloon animates:

    >>> game = GameScene()
    >>> first_position = game.get_balloon_position()
    >>> game.update(10)
    >>> second_position = game.get_balloon_position()
    >>> first_position == second_position
    False

    The arrow stays still:

    >>> game = GameScene()
    >>> first_position = game.get_arrow_position()
    >>> game.update(10)
    >>> second_position = game.get_arrow_position()
    >>> first_position == second_position
    True
    """

    ...
```

We create a game scene, query some of its state, update it, query some it its
state again and make some assertions.

When we wrote these test at the balloon shooter level, we had to assert that
circles were drawn in specific locations. In this test, no drawing is involved.

In order for the tests above to work, we have to write getters to expose some
internal state:

```python
class GameScene(SpriteGroup):

    ...

    def get_balloon_position(self):
        return self.balloon.get_position()

    def get_arrow_position(self):
        return self.arrow.get_position()
```

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

```python
"""
>>> game = GameScene()
>>> game.get_flying_arrows()
[]
"""
```

We make it work like this:

```python
class GameScene(SpriteGroup):

    ...

    def __init__(self):
        ...
        self.flying_arrows = self.add(SpriteGroup())

    def get_flying_arrows(self):
        return self.flying_arrows.get_sprites()

    ...
```

We also add the `get_sprites` getter in the `SpriteGroup` class. Again, this
getter is only used in tests. This bothers me again. Not that it is only used
in tests, but that this feels like bad object oriented design. Perhaps it would
be cleaner it the sprite group only provided a `get_count` method? Or something
more specific instead of just exposing its internal collection.

But we are fine with exposing internal state for testing purposes. So we don't
think too much about it now. But let's keep it in the back of our minds for the
future.

Let's move on to shooting so that we get some flying arrows. Here is the test:

```python
"""
>>> game = GameScene()
>>> initial_position = game.get_arrow_position()
>>> game.event(GameLoop.create_event_keydown_space())
>>> game.update(10)

It makes the arrow fire:

>>> flying = game.get_flying_arrows()
>>> len(flying)
1
>>> flying[0].get_position() == initial_position
False

The initial arrow stays the same:

>>> game.get_arrow_position() == initial_position
True
"""
```

We simulate a shot by sending a space keydown event followed by an update. We
assert that we now have a flying arrow and that its position is not the
original position of the arrow (it has moved). Furthermore we assert that the
current arrow position is the same as the initial meaning that we still have an
arrow that we can shoot.

The implementation: change the event handler for keydown space from this

```python
self.arrow.shoot()
```

to this:

```python
self.flying_arrows.add(Arrow(shooting=True))
```

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

```python
"""
>>> game = GameScene(space)
>>> game.event(GameLoop.create_event_keydown_space())
>>> game.update(10000)
>>> game.get_flying_arrows()
[]
"""
```

If we decrease the number in `update` the flying arrows collection will not be
empty because the arrow that we shoot has not had time to fly off screen yet.

We make this test pass by overriding the `update` method of the sprite group
and doing the collision detection to remove flying arrows outside the screen:

```python
class GameScene(SpriteGroup):

    ...

    def update(self, dt):
        SpriteGroup.update(self, dt)
        for x in self.flying_arrows.get_sprites():
            if x.hits_space(self.space):
                self.flying_arrows.sprites.remove(x)
```

Now the `get_sprites` method that we wrote before, for testing purposes, comes
in handy. Iterating over sprites in a collection seems like a reasonable
behavior. I still don't think we should expose the internal collection. That is
bad. But we could expose some kind of iterator.

Oh, and to remove the sprite, we actually reach into the fields of the flying
arrows group and call it's `remove` method. Yikes. But the tests pass. Let's
commit and see if we can improve this.

We come up wit this instead:

```python
def update(self, dt):
    SpriteGroup.update(self, dt)
    for arrow in self.flying_arrows.get_sprites():
        if arrow.hits_space(self.space):
            self.flying_arrows.remove(arrow)
```

`x` is not a very meaningful name, so we call it `arrow` instead. Then we call
a new `remove` method on the sprite group. No more reaching into internal
fields and modifying them.

The sprite group looks like this:

```python
class SpriteGroup:

    ...

    def get_sprites(self):
        return list(self.sprites)

    def remove(self, sprite):
        self.sprites.remove(sprite)
```

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
