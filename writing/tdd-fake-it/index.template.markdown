---
title: "TDD trick: fake it!"
date: 2023-04-12
tags: tdd,draft
---

The first step in the TDD loop is to think about what test to write. I find it
easiest to do that from the outside-in. I might not yet know what different
parts my system will consist of (especially in the beginning), but I do know
some behavior of the entire system.

The problem with outside-in is that the test might be difficult to write
because you don't have enough infrastructure in place to make the appropriate
assertions.

In this blog post I want to show you a trick to overcome that problem: fake
it! (This is also similar to what James describes in [Grow Evolutionary
Seeds](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#grow-seeds).)

## The example

The example I will be using comes from a [game](/projects/agdpp/index.html)
that I'm working on.

So far, it doesn't do much. It just animates a circle:

<center>
![Animated circle.](animation.png)
</center>

The behavior of this system can be described as this:

> I draw an animated circle until the user closes the window.

## Fake it!

With an empty project, it seems quite difficult to write a test that actually
checks for that. What to do? Let's fake it! Here it is:

$:output:python:
class Game:

    """
    I draw an animated circle until the user closes the window.

    >>> game = Game()
    >>> game.run()
    DRAW_CIRCLE
    EXIT
    """

    def run(self):
        print("DRAW_CIRCLE")
        print("EXIT")
$:END

Really? How is that useful?

First of all we have got a description of one behavior of our system. We also
have code that verifies that behavior, even though it is not yet complete.

From here it is usually easier to see what to continue with.

Get [something on the
screen](https://www.artima.com/articles/the-simplest-thing-that-could-possibly-work),
and we can more easily criticize it and improve it.

## Refactor

Are you kidding? Refactor already?

To make this game testable, I want to separate the logic of the game from the
infrastructure needed to initialize and use a graphics library.

I imagine a game loop class that is responsible for that.

We can evolve the design in that direction even with what we have now.

Here it is:

$:output:python:
class Game:

    """
    I draw an animated circle until the user closes the window.

    >>> game = Game(GameLoop())
    >>> game.run()
    DRAW_CIRCLE
    EXIT
    """

    def __init__(self, loop):
        self.loop = loop

    def run(self):
        self.loop.run(self)

    def tick(self):
        self.loop.draw_circle()

class GameLoop:

    def run(self, game):
        game.tick()
        print("EXIT")

    def draw_circle(self):
        print("DRAW_CIRCLE")
$:END

The game now gets a loop as a dependency, and maybe you can see how this would
be possible to test now. We could inject some kind of test double as the loop
and verify that it is called correctly.

Notice that we were able to do this refactoring with the safety net of the
test. We have one teeny, tiny test that asserts something fake, but it still
helped us do this refactoring.

## Infrastructure wrapper

For the test double version of the game loop, I want to use some patterns from
[Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks).

I want to turn the game loop into an infrastructure wrapper. One part of that
pattern is that it should emit events of what it's doing so that tests can
observe it.

Let's replace the print statements, that we used to fake this, with events.
Here it is:

$:output:python:
class GameLoop(Observable):

    def run(self, game):
        game.tick()
        self.notify("EXIT", {})

    def draw_circle(self):
        self.notify("DRAW_CIRCLE", {})
$:END

The test for the game then changes to this:

$:output:python:
"""
I draw an animated circle until the user closes the window.

>>> loop = GameLoop()
>>> events = loop.track_events()
>>> Game(loop).run()
>>> events
DRAW_CIRCLE =>
EXIT =>
"""
$:END

(I won't show the code for `Observable`. If you are curious, to know the
details, you can read
[here](https://github.com/rickardlindberg/agdpp/blob/initial-game-loop/events.py).)

And this is starting to look more real now. There is no real faking going on in
the game and its test any more. It seems logical to assert that the game loop
emits those events.

But the game loop is still just emitting events, it's not actually doing
anything.

## Make game loop more real

Another pattern of an infrastructure wrapper is that it should be nullable.
That means that we can instantiate two version of our game loop:

$:output:python:
GameLoop.create()
GameLoop.create_null()
$:END

The creation methods look like this:

$:output:python:
class GameLoop(Observable):

    @staticmethod
    def create():
        return GameLoop(pygame)

    @staticmethod
    def create_null():
        class NullPygame:
            def init(self):
                pass # Do nothing
            ...
        return GameLoop(NullPygame())

    ...
$:END

The null version works exactly the same as the real version except it nulls out
all the calls that actually do anything with pygame. This is useful in tests so
that we don't open windows and actually draw graphics when we don't need to.

The first step of making the game loop real is to flesh out some calls to
pygame. Here they are:

$:output:python:
class GameLoop(Observable):

    ...

    def run(self, game):
        self.notify("PYGAME_INIT", {})
        self.pygame.init()
        self.screen = self.pygame.display.set_mode((1280, 720))
        game.tick()
        self.pygame.display.flip()
        self.notify("EXIT", {})
        self.pygame.quit()

    def draw_circle(self):
        self.notify("DRAW_CIRCLE", {})
        self.pygame.draw.circle(self.screen, "red", (50, 50), 40)
$:END

Our game test can use the null version of the game loop and will continue to
work as they did before.

When actually running our game, we create the real version of the game loop
which will also include all the real calls to pygame.

We can also test the real game loop in isolation, passing it a test game, to
make sure that we call pygame correctly.

If we run this now, it will actually open a window and draw a circle.

## Summary

The game loop still doesn't implement a loop. Hopefully you can see how to
continue to flesh that part out.

Once we have finished removing all the fakes from the game loop we are done.

The game test makes sense and additional tests for the game loop also make
sense and pass.

So there it is.

Start with something fake. Do a bit of design. Remove one fake at a time until
you are done. I find that a good workflow when getting started.

(If you want more details about this example in particular, check out my
[article](/writing/agdpp-game-loop/index.html) about how I implemented this
part of the game.)
