---
title: Shooting arrow
date: 2023-04-22
tags: agdpp,draft
agdpp: true
---

In this episode we continue towards the first version of the balloon shooter.
It's time to shoot the arrow!

## Video version

The video version of this episode:

<center>
...
</center>

## Reacp

We are trying to create an absolute minimum version of a balloon shooter game
that we can show to our customer and ask if that was what he had in mind. Our
idea for minimal is this:

* 1 balloon falling down the screen
* 1 arrow pointing in a fixed direction
* 1 button to shoot that single arrow
* Then game over

In the
[previous](/writing/agdpp-demo-and-game-idea/index.html) episode, we took the
first step by drawing a balloon and an arrow. Here is what it looks like:

<center>
![Balloon shooter.](shooter.png)
</center>

And here is a list of possible stories to work on next:

* Balloon moves downwards
* **Arrow animates when shot**
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* Real graphics instead of circles

I sometimes find it hard to look too far into the future. Perhaps that is true
for our customer as well. I find it much easier to look at the current state of
the software and ask myself "What to work on next?"

When I run the game now, all I want to do is fire that arrow and see it flying
across the screen. So that is our next story!

## Acceptance

When working in an agile way, things might seem reversed from what you are used
to. For example, when doing TDD, we write the test before we write the code.
The same applies for stories. Before starting work on a story, we should figure
out the acceptance criteria. How do we know when we are done?

For the story with shooting arrow I think the acceptance criteria is that you
should see the arrow flying across the screen when you press the space key. Our
customer agrees on that.

The next step is to figure out how to write an automated test for that.

## The test we want to write

Just to recap, the test for the ballon shooter looks like this:

$:output:python:
"""
>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
>>> events = loop.track_events()
>>> BalloonShooter(loop).run()
>>> events
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 50
    y: 50
    radius: 40
    color: 'red'
...
"""
$:END

That is, we run the ballonn shooter game, configure a set of events that should
be simulated, and then assert that certain things happens (game loop inits,
circles are drawn on screen, etc).

The test we want to write the for shoot behavior starts like this:

$:output:python:
"""
The arrow moves when it is shot by pressing the space key:

>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [GameLoop.create_event_keydown_space()],
...         [],
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
>>> events = loop.track_events()
>>> BalloonShooter(loop).run()
"""
$:END

We introduce a new event, keydown space, and simulate that it happens after one
frame, and then we simulate a couple of more frames.

This partial test fails because this new event does not yet exist, so let's fix
it.

## Adding a new event

$:output:python:
class GameLoop(Observable):

    ...

    @staticmethod
    def create_event_keydown_space():
        """
        >>> GameLoop.create_event_keydown_space().is_keydown_space()
        True
        """
        return Event(pygame.event.Event(pygame.KEYDOWN, key=pygame.K_SPACE))
$:END

$:output:python:
class Event:

    ...

    def is_keydown_space(self):
        return self.pygame_event.type == pygame.KEYDOWN and self.pygame_event.key == pygame.K_SPACE
$:END

## TODO

* move on to filter out draw events, not possible

* implement more filtering in Events

* discuss how to get x, y coordinates

* implement collect

* finally a real failure

* implement quickly and now head of the arrow moves

* fix arrow animation

## Getting tangled up in tests

I'm not happy with the current tests. On the other hand, I'm not sure how to
improve them either.

I think I need to spend some time with my notebook. Maybe running. Hammock?

## Summary

The arrow now moves. So exciting! But it's a bummer that you have to restart
the game after each shot. Therefore I think the game over screen might be most
interesting to work on next. When the arrow misses (goes outside the screen)
the game should be over, and the arrow should be reset.

After that, I think collision check with balloon would be most interesting.

All those require tests of course. So we should probably work on getting
comfortable with the test setup as well.

See you in the next episode!
