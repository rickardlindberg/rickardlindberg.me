---
title: "TDD trick: fake it"
date: 2023-04-12
tags: tdd,draft
---

The first step in the TDD loop is to think about what test you want to write. I
find it easiest to do that from the outside-in. I might not yet know what
different parts my system will consist of (especially in the beginning), but I
do know some behavior of the system.

The problem with outside-in is that the test might be difficult to write
because you don't have enough infrastructure in place to make the appropriate
assertions.

In this blog post I want to show you a trick to overcome that problem: faking
it. (This is also similar to what James describes in [Grow Evolutionary
Seeds](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#grow-seeds).)

## The example

Animated game...

## First behavior

> I draw an animated circle until the user closes the window.

## Fake it

With an empty project, that test seems quite difficult to get in place. So
let's fake it. Here it is:

$:output:python:
class Game:

    """
    I draw an animated circle until the user closes the window.

    >>> game = Game(GameLoop())
    >>> game.run()
    DRAW_CIRCLE
    EXIT
    """

    def run(self):
        print("DRAW_CIRCLE")
        print("EXIT")
$:END

Really? How is that useful?

First of all we have got a description of one behavior of the system. We also
have code that verifies that behavior, even though it is not yet complete.

From here it is usually easier to see what to continue with.

## Refactor

Are you kidding? Refactor already?

To make this game testable, I want to separate the logic of the game from the
infrastructure needed to initialize and use a graphics library.

I imagine a game loop class that is responsible for that and also for calling
our game in every loop.

I can evolve the design in that direction even with what we have now.

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
be possible to test now.

Notice that we could do this refactoring with the safety net of the test. We
have one teeny tiny test that asserts something fake, but it still helped us do
this refactoring safely.

## Infrastructure wrapper

I want to use some patterns from [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks).
To test this.

I want to turn the game loop into an infrastructure wrapper. One part of that
pattern is that it should emit events of what it's doing.

So next I want to replace the print statements, that we used to fake this, with
events. Here it is:

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

And this is starting to look more real now.

But the game loop is still just emitting events, it's not actually doing
anything.

## Make game loop more real

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
