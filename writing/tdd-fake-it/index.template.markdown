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
because we don't have enough infrastructure in place to make the appropriate
assertions.

In this blog post I want to show you a trick to overcome that problem: fake
it! (This is also similar to what James describes in [Grow Evolutionary
Seeds](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#grow-seeds).)

## The example

I will illustrate this trick with an example from a
[game](/projects/agdpp/index.html) that I'm working on.

So far, the game doesn't do much. It just animates a circle:

<center>
![Animated circle.](animation.png)
</center>

The behavior of the game can be described as this:

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

Really? How is this useful?

First of all we have got a description of one behavior of our system. We also
have code that verifies that behavior, even though it is not yet complete. (The
example shown in the docstring of the game class is actually an executable test
written using Python's
[doctest](https://docs.python.org/3/library/doctest.html) module.)

From here it is usually easier to see what to continue with. When we have
[something on the
screen](https://www.artima.com/articles/the-simplest-thing-that-could-possibly-work),
we can more easily criticize it and improve it.

So, what next?

## Refactor

Are you kidding? Refactor already?

To make this game testable, I want to separate the logic of the game from the
infrastructure needed to initialize and use a graphics library.  I imagine a
game loop class that is responsible for that.

We can evolve the design in that direction even with what we have now. Here it
is:

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

The game now gets a loop as a dependency. Can you see how this would be
possible to test now? We could inject some kind of test double as the loop and
verify that it is called correctly.

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

Let's replace the print statements, that we used to fake actions, with events.
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

(I won't show the code for `Observable`. If you are curious to know the
details, you can look
[here](https://github.com/rickardlindberg/agdpp/blob/initial-game-loop/events.py).)

And this is starting to look more real now. There is no real faking going on in
the game or its test any more. It seems logical to assert that the game loop
emits those events.

But the game loop is still just emitting events, it's not actually doing
anything.

## Make game loop more real

The game loop should initialize a graphics library and provide to the game a
way to draw on the screen. We will use [pygame](https://www.pygame.org/news)
for that. So our game loop will be an infrastructure wrapper around pygame
providing a clean interface for our game to draw on the screen.

We need to make the `run` method and the `draw_circle` method do something
real. Here is a first version:

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

Notice that have an instance variable called `pygame`.

When we test our game class, we don't actually want to execute any pygame code
that creates windows and draws circles on the screen. Therefore we use another
pattern of an infrastructure wrapper which is that it can be nullable. That
means that we can instantiate two version of our game loop:

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

Our game test can use the null version of the game loop and will continue to
work as it did before.

When actually running our game, we create the real version of the game loop
which will include all the real calls to pygame.

We can also test the real game loop in isolation, passing it a test game, to
make sure that we call pygame correctly:

$:output:python:
"""
>>> GameLoop.create().run(TestGame())
"""
$:END

This will actually open a window and draw whatever the test game draws. We can
program the test game to exit immediately so that the test suite will not hang
waiting for user input. But we will still see a flashing window which is a bit
distracting.

To verify that our implementation of `draw_circle` works, we have to inspect
the output visually. A test like the one above only asserts that we call pygame
functions correctly, not that the output looks the way we want.

This is a general problem with infrastructure that it is difficult to test,
because it involves the real world.

Anyway, that's a little beside the point of this article. Where were we?

## Looping and animation

If we run our game now, it will actually show a window with a circle on it.
But the window will close immediately.

That is because the game loop still doesn't implement a loop.

Let's have a look at our game test again:

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

It talks about *animating* a circle, and about *waiting* for the user to close
the window. But there is nothing in the setup or assertions about this. We are
missing something.

Hopefully, at this point, it is a bit more clear where to continue.

Once we implement the loop and some event handling, I think the initial
behavior of our game will actually be fully realized.

Here is what the final test for the game looks like when I continued fleshing
out all the fakes and missing pieces:

$:output:python:
"""
I draw an animated circle until the user closes the window.

>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [],
...         [pygame.event.Event(pygame.QUIT)],
...     ]
... )
>>> events = loop.track_events()
>>> Game(loop).run()
>>> events
PYGAME_INIT =>
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 50
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 51
PYGAME_QUIT =>
"""
$:END

## Summary

We started with something fake, then did a bit of design, then removed one fake
at a time until there were no fakes left.

I find this a useful way of working, especially when getting started. Once you
have some structure in place it is easier to see where you need to add
additional tests and functionality.

(If you want more details about this example in particular, check out my
[article](/writing/agdpp-game-loop/index.html) about how I implemented this
part of the game.)
