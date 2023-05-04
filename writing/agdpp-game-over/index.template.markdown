---
title: Game over?
date: 2023-04-24
tags: agdpp,draft
agdpp: true
---

When we worked on [shooting the
arrow](/writing/agdpp-shooting-arrow/index.html) we concluded that it was
tedious to restart the game after each shot. When the arrow goes outside the
screen, we want the game to be over instead and the arrow reset. Let's work on
that in this episode.

## Do we really need game over?

If we implement game over now, there will be game over after every shot.
Because there is no way to hit the balloon just yet.

If you play a game where it is game over immediately, would you enjoy it?

Perhaps game over is not right story to work on? It is a solution to the
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

I'm not sure the balloon needs to move downwards either. The current movement
pattern is fine.

And we can do something else instead of game over. That leaves us with this:

* Arrow can hit balloon
* Point is given for hit
* New arrow when the current one has been shot

We can always make something smaller. And what we initially thought we needed,
we don't need. At least yet. When we play the game, we quite quickly find out
what is needed next. Working software. In the hands of its users.

Let's work on spawning arrows now so that we can enjoy shooting arrows for a
longer time without having to restart our game.

## Acceptance

I can think of two test:

* You get a new arrow when you shoot the current one
* When an arrow goes outside the screen, we stop rendering it

The second test is kind of internal. If we render thousands of arrows outside
the visible screen, no one will notice. Until there is a performance issue or
an out of memory crash or something like that.

On the other hand, it makes sense, from a gameplay perspective, to talk about
arrows going off the screen as being deactivated. Otherwise it might be that
they come back after a while, but now instead move downwards.

## How to write the test?

All tests for our game are currently written at the top-level. Here is the test
that checks for behavior when we press the space key:

$:output:python:
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
$:END

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

$:output:python:
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
$:END

I have an idea how to push much of this logic down one level so that it can
more easily be tested. Let's give it a try.

We keep all our sprites in a list. Managing a list of sprites seems like a good
job for a new class. We create a `SpriteGroup` class that works like this:

$:output:python:
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
$:END

A sprite is any object that can respond to `update` and `draw` calls. When we
update and draw the sprite group, it calls the corresponding methods on all its
sprites.

We can use this new class in our game like this:

$:output:python:
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
$:END

Not much of a difference. Mainly we moved the looping over the sprites from our
game to the sprite group class. I think this is a bit cleaner, but it makes
testing no easier.

But we are not done yet. Now, let's extract a lower level object that we call
`GameScene`.

The init of the game then changes to this:

$:output:python:
def __init__(self, loop):
    ...
    self.game_scene = GameScene()
$:END

And the tick method changes to this:

$:output:python:
def tick(self, dt, events):
    for event in events:
        self.game_scene.event(event)
    self.game_scene.update(dt)
    self.loop.clear_screen()
    self.game_scene.draw(self.loop)
$:END

That is, we defer event handling, updating, and drawing to the game scene.

The game scene looks like this:

$:output:python:
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
$:END

It inherits from `SpriteGroup` so it gets the `update` and `draw` "for free".
And the event handling code is extracted from game. (Since the `event` function
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
design change, things will take a little longer. Wit the new design in place,
new features can more easily be added (and tested). Until another design
challenge comes a long.

## State based testing

Breakthrough!

$:output:python:
$:END

## TODO

* Breakthrough: getters to expose state! State based testing!

    commit 285f424514dbde48e82ee75c66f45930ad027ad2 (HEAD -> main)
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Tue Apr 25 06:12:28 2023 +0200

        Test initial state of game scene with state based testing.

            #joysticks = {}

                #for event in pygame_events:
                #    print(event)
                #    if event.type == pygame.JOYDEVICEADDED:
                #        joy = pygame.joystick.Joystick(event.device_index)
                #        joysticks[joy.get_instance_id()] = joy
                #        print(joy)

<Event(1539-JoyButtonDown {'joy': 1, 'instance_id': 1, 'button': 0})>
<Event(1540-JoyButtonUp {'joy': 1, 'instance_id': 1, 'button': 0})>
<Event(1536-JoyAxisMotion {'joy': 1, 'instance_id': 1, 'axis': 0, 'value': 0.003906369212927641})>
<Event(1536-JoyAxisMotion {'joy': 1, 'instance_id': 1, 'axis': 1, 'value': -0.003936887722403638})>

* TDD did not work for joystick movement. I needed to "feel" what was right. I
  did it as a spike.

* Inline event handling.

* Extract event handler for angle-change-number.

* Make tests independent of specifics speeds/angles so that they can be
  adjusted until they feel good without tests failing.

* This episode: state base testing got me going

* Next episode: fast flow of features building up to first balloon shooter

* Next: adding joystick

* Next: adding score text instead of circles

## Summary

See you in the next episode!
