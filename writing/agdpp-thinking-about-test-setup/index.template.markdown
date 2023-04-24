---
title: Thinking about test design
date: 2023-04-23
tags: agdpp,draft
agdpp: true
---

In the [previous](/writing/agdpp-shooting-arrow/index.html) episode we were not
quite happy with the design of our tests. Are we testing things at the right
level? Do we see any smells? Are we missing tests? We will take some time in
this episode to reflect on those questions so that things will go smooth(er),
testing wise, when working on the next story.

## A concrete problem

In the previous episode I had a feeling that everything was not alright with
the tests. I poked around a bit and noticed that I could change some vital
production code that would break the game without my tests noticing.

Let's have a look.

The event handling in the game now looks like this:

$:output:python:
class BalloonShooter:

    ...

    def tick(self, dt, events):
        for event in events:
            if event.is_user_closed_window():
                self.loop.quit()
            elif event.is_keydown_space():
                self.arrow.shoot()
        ...
$:END

If we skip the check for the space key and always shoot the arrow, like this:

$:output:python:
class BalloonShooter:

    ...

    def tick(self, dt, events):
        for event in events:
            if event.is_user_closed_window():
                self.loop.quit()
        self.arrow.shoot()
        ...
$:END

Then the arrow goes off immediately when the game starts, which is obviously
not correct, but all tests still pass.

## Coverage and expectation

The promise of TDD and automated testing is that you can have very high test
coverage. What I aspire to is to create a test suite that within seconds tells
me if I broke the production code with my change. It might not be realistic,
but when I find a case where I could break my production code without my tests
noticing, I want to look more closely.

Testing can for sure be seen as a tradeoff. At some point it probably costs
more to test than what you gain.

One reason to not test is that you don't know how. That can be solved by
practicing. Another reason might be that you decide that the cost is not worth
it. However, the cost of testing goes down the better you get at it.

And in this series I'm trying to learn as well and document that process. So
let's analyze the problem with the gap in the test suite and see what we can
come up with.

## What test is missing?

The balloon shooter has two tests. Here are their descriptions:

1. I draw the initial scene of the game which consists of a balloon and an
   arrow and quit when the user closes the window.

2. The arrow moves when it is shot by pressing the space key.

The first test is checking that the initial frame is drawn correctly.

The second test is checking that the arrow moves when we press space.

But there is no test checking that the arrow stays still if we don't press
space.

I think we tried to remedy that in the previous episode by adding these two
"lower-level tests" to the arrow:

$:output:python:
class Arrow:

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

    ...
$:END

The problem is that the event checking logic is not in the `Arrow` class, but
in the `BalloonShooter` class. So what we want to test can't be tested at this
level.

## Where to test?

Writing all tests as "top-level tests" which includes all objects is not a
good idea. The test setup will get complicated. The asserts will get difficult
to write. The test will be slower.

So we want to test at as a low level as possible.

So if we want to test the initial state of the arrow in the `Arrow` class, we
need to move event handling logic into it, so that it's its responsibility and
does not need to be tested in `BalloonShooter`.

On the other hand, when you start writing tests for smaller subsystems, those
subsystems become harder to refactor. Suppose you are not happy with the
subsystems that you have created, and you want a different design. If you do
that refactoring, you also have to modify the tests to fit. That makes
refactoring and design harder. So you don't want to do it too early.

In our case, I think we did it too early. Our game class only has one test, and
we already started extracting subsystems and writing tests there.

Let's see if we can fix that.

## Testing initial state

Before we had this test for the initial state:

$:output:python:
"""
I draw the initial scene of the game which consists of a balloon and an
arrow and quit when the user closes the window.

>>> BalloonShooter.run_in_test_mode(
...     events=[
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 50
    y: 50
    radius: 40
    color: 'red'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 10
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 520
    radius: 15
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 540
    radius: 20
    color: 'blue'
GAMELOOP_QUIT =>
"""
$:END

This only captures the initial frame. Let's see if we can rewrite this.

We start with this:

$:output:python:
"""
I am a balloon shooter game!

Initial state
=============

We run the game for a few frames, then quit:

>>> events = BalloonShooter.run_in_test_mode(
...     events=[
...         [],
...         [],
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
"""
$:END

This is just the setup. We simulate that we start the game, run it for a couple
of frames, then quit.

What are some behaviors that we expect to see here?

$:output:python:
"""
The game loop is initialized and cleaned up:

>>> events.filter("GAMELOOP_INIT", "GAMELOOP_QUIT")
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
GAMELOOP_QUIT =>
"""
$:END

$:output:python:
"""
The balloon is drawn animated:

>>> events.filter("DRAW_CIRCLE", radius=40).collect("x", "y")
[(50, 50), (51, 50), (52, 50)]
"""
$:END

$:output:python:
"""
The arrow is drawn in a fixed position:

>>> set(events.filter("DRAW_CIRCLE", radius=10).collect("x", "y"))
{(500, 500)}
>>> set(events.filter("DRAW_CIRCLE", radius=15).collect("x", "y"))
{(500, 520)}
>>> set(events.filter("DRAW_CIRCLE", radius=20).collect("x", "y"))
{(500, 540)}
"""
$:END

These new cases cover all the cases in the old test, so we can remove the old.
Furthermore it also checks that the arrow doesn't move so that we no longer can
do the mistake of always shooting the arrow. Success!

## Reflecting on new test setup

The test for the initial state is structured by simulating a run of the game,
collecting events of what happened, and then making specific assertions about
those events to check different behavior.

Let's structure the rests of the test that way too:

$:output:python:
"""
User presses space key
======================

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

If there are more things that should happen when we press the space key, we can
add asserts for it. But for now, I don't think there is any.

## Summary

All ours test for the game are now written at the top-level. They include all
the objects. And they are written close to the acceptance criteria for stories.
Don't we need lower-level tests as well?

We for sure can't keep testing all aspects of the game with top-level tests.
Subsystems must for sure emerge that are easier to test. We will keep that in
mind for the future and look extra carefully at what those subsystems might be.
But for now, we are happy that we closed the gap in our test suite.

See you in the next episode!
