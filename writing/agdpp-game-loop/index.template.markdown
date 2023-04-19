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

$:output:python:
# Example file showing a basic pygame "game loop"
import pygame

# pygame setup
pygame.init()
screen = pygame.display.set_mode((1280, 720))
clock = pygame.time.Clock()
running = True

while running:
    # poll for events
    # pygame.QUIT event means the user clicked X to close your window
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # fill the screen with a color to wipe away anything from last frame
    screen.fill("purple")

    # RENDER YOUR GAME HERE

    # flip() the display to put your work on screen
    pygame.display.flip()

    clock.tick(60)  # limits FPS to 60

pygame.quit()
$:END

When we run it, it shows an empty screen:

<center>
![Tutorial output.](tutorial.png)
</center>

## Draw something

An empty screen is not that interesting, so let's see if we can get an
animation going.

We add a call to draw a circle and some logic to animate it:

$:output:python:
...

pos_x = 50
dt = 0

while running:

    ...

    if pos_x > 500:
        pos_x = 50
    else:
        pos_x += dt*0.3

    pygame.draw.circle(screen, "red", (pos_x, 50), 40)

    ...

    dt = clock.tick(60)  # limits FPS to 60

...
$:END

This seems to work. We get an animated circle:

<center>
![Animated circle.](animation.png)
</center>

## Refactor to clarify

Next we separate the logic of the game loop from the logic of our game. We
refactor in small steps, testing manually that everything works, and end up
with this for our game:

$:output:python:
class Game:

    def __init__(self, loop):
        self.loop = loop
        self.pos_x = 50

    def run(self):
        self.loop.run(self)

    def tick(self, dt, screen):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return True
        if self.pos_x > 500:
            self.pos_x = 50
        else:
            self.pos_x += dt*0.3
        screen.fill("purple")
        pygame.draw.circle(screen, "red", (self.pos_x, 50), 40)
$:END

And this for our game loop:

$:output:python:
class GameLoop:

    def run(self, game):
        pygame.init()
        screen = pygame.display.set_mode((1280, 720))
        clock = pygame.time.Clock()
        running = True
        dt = 0
        while running:
            if game.tick(dt, screen):
                running = False
            pygame.display.flip()
            dt = clock.tick(60)
        pygame.quit()
$:END

And it is all used like this:

$:output:python:
Game(GameLoop()).run()
$:END

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
        print("DRAW_CIRCLE")

class GameLoop:

    def run(self, game):
        game.tick()
        print("EXIT")
$:END

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

The `GameLoop` now looks like this:

$:output:python:
class GameLoop(Observable):

    def run(self, game):
        game.tick()
        self.notify("EXIT", {})

    def draw_circle(self):
        self.notify("DRAW_CIRCLE", {})
$:END

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

$:output:python:
class GameLoop(Observable):

    @staticmethod
    def create():
        return GameLoop(pygame)

    @staticmethod
    def create_null():
        class NullPygame:
            ...
        return GameLoop(NullPygame())

    def __init__(self, pygame):
        Observable.__init__(self)
        self.pygame = pygame

    ...
$:END

We write a test for the game loop that checks that the proper events are fired:

$:output:python:
"""
I init and clean up pygame:

>>> loop = GameLoop.create_null()
>>> events = loop.track_events()
>>> loop.run(NullGame())
>>> events
PYGAME_INIT =>
EXIT =>
"""
$:END

And we also create a test that uses the real Pygame module:

$:output:python:
"""
>>> GameLoop.create().run(NullGame())
"""
$:END

This test will actually cause a window to pop up on the screen, so it is a bit
distracting, but it makes sure we are calling Pygame correctly.

The game loop now looks like this:

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

We are getting closer. If we run the game now and look carefully, we can see
that a circle is drawn on the screen for a split second before it closes.  That
is because there is no loop yet. We just render one frame and then exit.  Time
to fix that.

## Loop and events

Let's start with our game. The test for it should simulate an event that the
user closes the window, and first then exit the application:

$:output:python:
"""
I draw an animated circle until the user closes the window.

>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [pygame.event.Event(pygame.QUIT)],
...     ]
... )
>>> events = loop.track_events()
>>> Game(loop).run()
>>> events
PYGAME_INIT =>
DRAW_CIRCLE =>
PYGAME_QUIT =>
"""
$:END

The `tick` method is modified to look like this:

$:output:python:
def tick(self, events):
    for event in events:
        if event.type == pygame.QUIT:
            return True
    self.loop.draw_circle()
$:END

So we made the decision that the game loop should exit if the tick method
returns true.

We add the ability for the null version of the game loop to simulate events:

$:output:python:
@staticmethod
def create_null(events=[]):
    class NullPygame:
        def init(self):
            self.event = NullEvent()
            ...
    class NullEvent:
        def get(self):
            if events:
                return events.pop(0)
            return []
    return GameLoop(NullPygame())
$:END

And we modify the run method to actually do a loop and pass events to the
`tick` method:

$:output:python:
def run(self, game):
    ...
    running = True
    while running:
        if game.tick(self.pygame.event.get()):
            running = False
    ...
$:END

If we do not configure tests for our game to simulate the quit event, the test
will hang in an infinite loop.

If we run the game now, the circle stays on the screen until we close the
window. But it doesn't move. Let's work on that.

## Test animation

We modify the test for our game by simulating one more event so that one more
frame is rendered. That gives us two calls to draw circle:

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

We had to add the `x` argument to the `DRAW_CIRCLE` event so that we could
observe that it changed:

$:output:python:
def draw_circle(self, x):
    self.notify("DRAW_CIRCLE", {"x": x})
    self.pygame.draw.circle(self.screen, "red", (x, 50), 40)
$:END

We also had to make a new call to clear the screen. If we don't clear the
screen we end up with circles drawn on top of each other. Clearing the screen
works like this:

$:output:python:
def clear_screen(self):
    self.notify("CLEAR_SCREEN", {})
    self.screen.fill("purple")
$:END

And the implementation for the animation looks like this:

$:output:python:
class Game:

    def __init__(self, loop):
        ...
        self.x = 50

    def tick(self, dt, events):
        ...
        if self.x > 500:
            self.x = 50
        else:
            self.x += dt
        self.loop.clear_screen()
        self.loop.draw_circle(self.x)
$:END

To make the animation frame rate independent, we also had to include the delta
time. This is implemented in the game loop similar to how we did it in the
spike:

$:output:python:
def run(self, game):
    ...
    dt = 0
    while running:
        if game.tick(dt, self.pygame.event.get()):
            running = False
        ...
        dt = clock.tick(60)
    ...
$:END

At this point our test-driven implementation does the same thing that our spike
does. We are now in a good place to move forward.

## Refactor exit

Before closing this episode, let's take advantage of our test suite and explore
an alternative way to exit the application. Instead of having a boolean return
from `tick` indicating if we should exit or not, which I think is a bit
unclear, let's try an exception. Here it is:

$:output:python:
class ExitGameLoop(Exception):
    pass
$:END

The tick method then turns into this:

$:output:python:
def tick(self, dt, events):
    for event in events:
        if event.type == pygame.QUIT:
            raise ExitGameLoop()
    ...
$:END

And the game loop into this:

$:output:python:
def run(self, game):
    ...
    try:
        while True:
            game.tick(dt, self.pygame.event.get())
            ...
    except ExitGameLoop:
        pass
    self.notify("PYGAME_QUIT", {})
    self.pygame.quit()
$:END

I like this better. And all the tests still pass, unchanged.

## Summary

We have now recreated the functionality that we had in the spike, added the
ability to test it, and improved the design with the safety net of our tests.
Great success!

You can browse the [complete source
code](https://github.com/rickardlindberg/agdpp/tree/initial-game-loop) from
this episode.

See you in the next episode!
