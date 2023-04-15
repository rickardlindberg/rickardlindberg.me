---
title: Separating pygame completely from the rest of the game
date: 2023-04-13
tags: agdpp,draft
---

In this episode we reflect on our current design. I see something that bothers
me. We talk about it and how to fix it.

## The problem

Right now our game is split up into two main classes: the game and the game
loop. The game contains the logic of our game, while the game loop is
responsible for setting up pygame and calling our game on every frame.

Almost all references to pygame are contained in the game loop class. Our game
knows almost nothing about pygame. Almost. And that bothers me.

Let's have a look at the test for our game:

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

We can see references to pygame in two places. First when we create the quit
event. We create an instance of a pygame event and pass that to the null
version of the game loop. Later in the events that we assert on, there are
event names mentioning pygame (`PYGAME_INIT` and `PYGAME_QUIT`).

Why does this bother me?

One purpose of introducing the game loop class was to separate pygame code from
our game. One reason to do that is that our game becomes easier to test. And if
it's easier to test, it suggests that the design is also better. (People claim
at least. So let's go with that here.)

But some details of pygame are leaking out.

If `GameLoop` were instead called `PygameGameLoop`, I would be more fine with
this. But I think the design would be more clear if the game didn't know
anything at all about pygame.

Our game loop uses the [infrastructure
wrapper](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#infrastructure-wrappers)
pattern. One purpose of that pattern is to isolate and contain infrastructure
code so that the user of it can be provided an interface that is optimal for
its consumption. In our case we want to design our game loop to fit exactly
what our game needs. And the loop should encapsulate all the details of how to
make that happen (using pygame).

Our game now needs to know that the pygame quit event is fired when the user
closes the window. But wouldn't it be more clear if that could be expressed in
the code something like this instead?

$:output:python:
if event.is_user_closed_window():
    self.loop.quit()
$:END

Above, the game does not need to know about pygame and can directly express the
idea that if the user closes the window, the game loop should be quit.

Enough talking, let's see if we can fix this.

## Wrapping events

The tick method of our game now looks like this:

$:output:python:
def tick(self, dt, events):
    for event in events:
        if event.type == pygame.QUIT:
            self.loop.quit()
    ...
$:END

The interface is that events is a list of pygame event instances.

Let's change that to instead be instances of a new event class that we control.
Here is a first version:

$:output:python:
class Event:

    def __init__(self, pygame_event):
        self.pygame_event = pygame_event

    def is_user_closed_window(self):
        return self.pygame_event.type == pygame.QUIT

    def __repr__(self):
        return repr(self.pygame_event)
$:END

Now we can change how the game loop calls the game from this

$:output:python:
game.tick(dt, self.pygame.event.get())
$:END

to this

$:output:python:
game.tick(dt, [Event(x) for x in self.pygame.event.get()])
$:END

This breaks our tests saying that

    AttributeError: 'Event' object has no attribute 'type'

We modify our game to use our new method instead:

$:output:python:
def tick(self, dt, events):
    for event in events:
        if event.is_user_closed_window():
            self.loop.quit()
$:END

All tests are passing again.

We can rely on our tests for this refactoring.

<a
href="https://github.com/rickardlindberg/agdpp/commit/ac00de877b8f4ee58716c0030c8b2ecab19a318e"><code>git commit -a -m 'Wrap events to tick for a nicer interface.'</code></a>

## Test still mentions pygame

Our test for the game still creates pygame events:

$:output:python:
"""
>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [],
...         [pygame.event.Event(pygame.QUIT)],
...     ]
... )
"""
$:END

Here we would like to instead express the idea that we want to simulate a user
closes the window event without mentioning any more details.

Here is one attempt:

$:output:python:
"""
>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
"""
$:END

And in game loop, we add this:

$:output:python:
@staticmethod
def create_event_user_closed_window():
    return pygame.event.Event(pygame.QUIT)
$:END

Now the game loop knows the details of how to create pygame events. I think
this is better. But there is one thing that still bothers me.

The tick method expects a list of events with the interface that we make up,
but `create_event_user_closed_window` creates a pygame event. So right now,
there is no way for us to test the tick method in isolation, because there is
no way to create events. If we expose the `Event` class we could do something
like this in a test:

$:output:python:
"""
>>> game.tick(dt=1, events=[Event(GameLoop.create_user_close_window())])
"""
$:END

I'm not sure I like that. I think I would feel better if
`create_event_user_closed_window` returned an event with our interface. Let's
try that.

We modify it:

$:output:python:
@staticmethod
def create_event_user_closed_window():
    return Event(pygame.event.Event(pygame.QUIT))
$:END

Test fail:

    AttributeError: 'Event' object has no attribute 'type'

I think we are now wrapping events in events. Let's unpack the pygame event
when creating the null version, going from this

$:output:python:
class NullEvent:
    def get(self):
        if events:
            return events.pop(0)
        return []
$:END

to this

$:output:python:
class NullEvent:
    def get(self):
        if events:
            return [x.pygame_event for x in events.pop(0)]
        return []
$:END

Now another test fails:

$:output:python:
>>> GameLoop.create_null(events=[["some event"]]).run(game)
$:END

We just changed the interface of `create_null` to expect `Event` instances.
Here we are passing a string. It should really be a pygame event. But we ignore
that for now and just change to this:

$:output:python:
>>> GameLoop.create_null(events=[[Event("some event")]]).run(game)
$:END

All tests are passing. Success!

We add a test to illustrate the usage of the factory method:

$:output:python:
@staticmethod
def create_event_user_closed_window():
    """
    >>> GameLoop.create_event_user_closed_window().is_user_closed_window()
    True
    """
    return Event(pygame.event.Event(pygame.QUIT))
$:END

At this point our game no longer depends on pygame. It only depends on the game
loop which provides the infrastructure needed for writing a game (graphics,
user input, music, etc). We can remove the import of pygame so the top of the
file now looks like this:

$:output:python:
#!/usr/bin/env python3

from gameloop import GameLoop

class Game:
    ...
$:END

<a
href="https://github.com/rickardlindberg/agdpp/commit/3686e4d1f5740301f2177810cfa26fa093153c17"><code>git commit -a -m 'Wrap events so that our game now longer knows about pygame.'</code></a>

## Get rid of last pygame reference

There is still one place left where our game test refers to pygame:

$:output:python:
"""
...
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

Some events that the game loop emits have pygame in their name.

Imagine that we could plug any game loop implementation into our game and from
the game's perspective, they all worked the same. Then it doesn't make sense
for it to emit events that talk about the underlying technology to realize the
game loop.

Let's rename `PYGAME_INIT` to `GAMELOOP_INIT` and `PYGAME_QUIT` to
`GAMELOOP_QUIT`. And while we are improving events, let's also add the
resolution and fps to the init event so that we can observe them.

Here is how the test reads then:

$:output:python:
"""
...
>>> events
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 50
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 51
GAMELOOP_QUIT =>
"""
$:END

<a
href="https://github.com/rickardlindberg/agdpp/commit/b6ef6430ee93bf9b933f5f06d69d9666ca2d1cd2"><code>git commit -a -m 'Game loop emits events with clearer names.'</code></a>

## Unnecessary work?

At this point, our game knows nothing about pygame. It only relies on the
interface of the game loop. That is something that we control and can design
specifically for what our game needs. And should we want to switch out pygame
for another graphics library, we only need to modify the game loop, not our
game.

You might object that this seems like too much speculative design. What is the
likelyhood that we want to switch graphics package? And can't we deal with
those problems when they arise?

On the one hand, I think that objection is valid. But I would like to see this
from another angle. The purpose of this change was not to make pygame easily
replaceable. The purpose of the change was to design a clean interface for our
game where the different classes had different responsibilities. Only as a side
effect pygame became more replaceable.

Had we designed a base class `GameLoop` and then derived a `PygameGameLoop`
from it, then I think we would have designed speculatively. There is only one
implementation of the game loop right now. We don't even anticipate any more
implementations, so why make a "placeholder" in our design where a second
implementation could be plugged in?

## Summary

Making design changes in the beginning is generally easier. The further you go
in the wrong direction, the harder it is to undo. (But with careful
refactoring, it is always possible.) One problem is that it might be harder to
see problems with the design early on. How are a few references to pygame
problematic? I think thinking in terms of code smells instead of in terms of
speculative design is useful here.

By the way, I'm not sure the design I choose here is the "correct" one. I'm
just trying to learn and practice here and explain my thinking. But I think if
I give attention to design early on, the future will be easier.

See you in the next episode!
