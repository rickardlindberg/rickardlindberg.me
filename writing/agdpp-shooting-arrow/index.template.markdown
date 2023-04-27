---
title: Shooting the arrow
date: 2023-04-22
tags: agdpp
agdpp: true
---

In this episode we continue towards the first version of the balloon shooter.
It's time to shoot the arrow!

## Video version

The video version of this episode:

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/CfhEcp9Qghc" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
</center>

## Recap

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

Just to recap, the test for the balloon shooter looks like this:

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

That is, we run the balloon shooter game, configure a set of events that should
be simulated, and then assert that certain things happens (game loop inits,
circles are drawn on screen, etc).

The test we want to write the for the shoot behavior starts like this:

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
frame, and then we simulate a couple of more frames. The reason that we include
a couple of frames is that we want to observe that the arrows moves between
different frames.

This partial test fails because this new event does not yet exist, so let's fix
that.

## Adding a new event

We add the new event like this:

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

We figure out the Pygame event parameters to use by reading the documentation.

We verify that we got it correct by printing something when we get the keydown
space event when running the game. We press different keys to see if it
correctly only captures the space key.

It seems to work.

## Filtering events

The initial test now runs, but if we print all the events that we get, there is
a lot of noise:

$:output:python:
"""
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

What we want to observe in that list of events is that the arrow has been drawn
in different positions (indicating movement).

So first we want to filter out the `DRAW_CIRCLE` events that are for the head
of the arrow.

We want to write like this:

$:output:python:
"""
>>> events.filter("DRAW_CIRCLE", radius=10)
"""
$:END

Filtering on event fields is not yet possible, but we own the library, and the
fix goes smoothly.

Once that is done, we get this list of events:

$:output:python:
"""
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 10
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 10
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 10
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 10
    color: 'blue'
"""
$:END

This means that when we ran our game in test mode, four frames where drawn and
here are all the circles with radius 10. We use 10 here because we know that
the head of the arrow is the only circle that is drawn with radius 10. But it
is not bullet proof. It would be better if we could pass an id to the draw
method call that is included in the event as well so that we could more
accurately identify objects. But this will do for now.

## Extracting positions

In the output above, we can look at the x and y coordinates and see if they
change. But there are also other fields that we don't care about in this test.
Let's filter out the position like this:

$:output:python:
"""
>>> events.filter("DRAW_CIRCLE", radius=10).collect("x", "y")
[(500, 500), (500, 500), (500, 500), (500, 500)]
"""
$:END

Again, the `collect` method does not exist, but we can extend our library
with it.

Now we have a list of positions where the head of the arrow is drawn. It
doesn't seem to change, which we can see more clearly by making the collection
into a set and seeing that it has only one element:

$:output:python:
"""
>>> set(events.filter("DRAW_CIRCLE", radius=10).collect("x", "y"))
{(500, 500)}
"""
$:END

## Real test failure

We want the arrow to move, so let's write an assert like this:

$:output:python:
"""
>>> len(arrow_head_positions) > 1
True
>>> len(set(arrow_head_positions)) > 1
True
"""
$:END

That is, we should get more than one position, and the set of all those
positions should also be larger than one, indicating movement.

The first assertion passes, but the other one fails. That is expected. Finally
we have the assertion failure that we wanted. Took a bit of time, huh? That
might tell us something about the design of our system. We'll talk about it
in [another episode](/writing/agdpp-thinking-about-test-setup/index.html).

## Implementation

First, we modify the event handler to check for the space key and shoot the
arrow if so:

$:output:python:
class BalloonShooter:

    def tick(self, dt, events):
        for event in events:
            if event.is_user_closed_window():
                self.loop.quit()
            elif event.is_keydown_space():
                self.arrow.shoot()
        ...

    ...
$:END

The shooting mechanism, we implement like this:

$:output:python:
class Arrow:

    def __init__(self):
        self.y = 500
        self.shooting = False

    def shoot(self):
        self.shooting = True

    def tick(self, dt):
        if self.shooting:
            self.y -= dt
$:END

We also adjust the drawing code so that all three circles that are drawn for
the arrow are drawn relative to the now variable y position.

$:output:python:
def draw(self, loop):
    loop.draw_circle(x=500, y=self.y, color="blue", radius=10)
    loop.draw_circle(x=500, y=self.y+20, color="blue", radius=15)
    loop.draw_circle(x=500, y=self.y+40, color="blue", radius=20)
$:END

The arrow now moves when we press the space key. Success! Unfortunately we only
have one shot. Then we need to restart the game to get a new arrow. We will fix
that in another story.

## Getting tangled up in tests

The shooting works, but I think we forgot to test the initial case. If we don't
press the space key, the arrow should stay still.

We can change the tick method of the arrow to this, and all tests still pass:

$:output:python:
def tick(self, dt):
    self.y -= dt
$:END

But that makes the arrow move immediately, even if not shot, which was not
intended.

We write a few more specific tests for the behavior of the arrow:

$:output:python:
"""
I stay still if I've not been fired:

>>> arrow = Arrow()
>>> initial_y = arrow.y
>>> arrow.tick(1)
>>> arrow.tick(1)
>>> arrow.tick(1)
>>> initial_y == arrow.y
True

I move upwards when fired:

>>> arrow = Arrow()
>>> initial_y = arrow.y
>>> arrow.shoot()
>>> arrow.tick(1)
>>> arrow.tick(1)
>>> arrow.tick(1)
>>> arrow.y < initial_y
True
"""
$:END

That forces us to add the if statement again.

I'm not sure I'm entirely happy that we access the y variable like that. I
prefer to see all fields of a class in Python as private (unless it's a pure
data object). But we got the test working at least.

I'm not happy with the current tests. On the other hand, I'm not sure how to
improve them either.

Maybe it's time to go for a jog or spend some time off the computer with my
notebook to see if any better ideas emerge. But for now, we leave it like this.

## Summary

The arrow now moves. So exciting! But it's a bummer that you have to restart
the game after each shot. Therefore I think the game over screen might be most
interesting to work on next. When the arrow misses (goes outside the screen)
the game should be over, and the arrow should be reset.

After that, I think collision check with balloon would be most interesting.

All those require tests of course. So we should probably work on getting
comfortable with the test setup as well.

The source code from this episode is on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/shoot-arrow).

See you in the next episode!
