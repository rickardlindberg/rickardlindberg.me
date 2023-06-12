---
title: Does TDD work when building a game?
date: 2023-06-12
tags: agdpp
agdpp: true
---

When I [started](/writing/agdpp-introduction/index.html) this series, one of
the areas that I wanted to explore was how well TDD works when building a game.

The short answer is that I find that it works equally well as on any other
project I've done.

The longer answer is that TDD might not be enough. There are some things which
are hard to test in a game. That's what I want to talk about in this blog post
and also how we can overcome the limits of TDD in those situations.

## Example: joystick movement

What is hard to test with TDD? Here is an example.

A [few episodes ago](/writing/agdpp-logitech-gamepad-f310/index.html), we
worked on controlling the game with a gamepad. The gamepad has a little
joystick (correct name?) and when we push it to the left, we want the arrow to
turn left, and likewise for right. That logic works well to implement with TDD,
however, that is not enough. It is also important that it "feels good" to turn
the arrow. That it responds fast enough, that it turns with an appropriate
speed, and so on. That part, I think, is impossible to work out with TDD since
we don't know the correct answer. The only way to get the correct answer is to
experiment. Try turning with different parameters and see which ones feel
better.

We can still use TDD for the first part if we relax the assertions. For
example, we can't assert that the arrow is turned by a specific amount, but we
can assert that it is turned more to the left than before. Here is an example
of that:

$:output:python:
"""
>>> initial_angle = game.get_arrow_angle()
>>> game.event(GameLoop.create_event_joystick_motion(axis=0, value=-0.5))
>>> game.update(1)
>>> game.get_arrow_angle() < initial_angle
True
"""
$:END

We assert the basic functionality in the test, and then we can play the game
and adjust parameters for turning until it feels good, and this test will
continue to work without modification.

## Testing in-game?

What are you saying Rickard? That we can't verify the behavior of the game with
tests and that we need to play the game to make sure it works? And do this
repeatedly? That sounds like a very long feedback loop. What about if we have
to tweak numbers for an effect that only happens after you score 100 points?
Should we play the game, score 100 points, look at the effect, tweak a
parameter, and do it all over again?

You certainly need to play the game to be able to get some details right. But
we should not need to play "unnecessary parts" over and over again until we
reach the point in the game that we are interested in testing.

So far, the balloon shooter is not that complex. If we want to test turn speed
for example, we can start the game and start turning. No additional steps
needed. However, when the game becomes more complex (start screen, player
selection, etc) and requires more interaction to get to where we want to go,
I think we should approach it differently.

## Test applications

The idea that I have, that I have not yet tested, is to create small test
applications or sub-games that can only be run in development.

For the case with the effect after 100 points, perhaps we can create a custom
game that starts immediately with 99 points. We can shoot down one more balloon
and observe the effect. That is a faster feedback loop.

This approach might require our design to change a bit. It must be easy to
start the game with a particular scene and configuration for example.

## Applications outside games

I have used this approach in [Timeline](/projects/timeline/index.html).
Timeline is a GUI application, so a different domain than games. But the GUI
elements have the same problem: you can't assert in tests that they "look
good".

Instead of running the application and opening the correct dialog, we
implemented a `--halt-gui` flag in our test runner. When that flag is present,
the tests will open GUI dialogs with mock data and you can visually inspect
that they look good:

<p>
<center>
![Timeline halt GUI.](timeline-halt-gui.png)
</center>
</p>

Your workflow for modifying a dialog can than be

1. run test suite with specific test and `--halt-gui` flag
2. inspect the dialog
3. tweak layout
4. repeat

This makes the feedback loop a little faster. There is no need to constantly
run the application and click your way to the correct dialog.

## What about real application testing?

Should we never run our application for real? Should we only get feedback from
tests and test applications?

No, I believe it is also important to use the application to get a sense of
what needs improving.

In [Python 110 - Now
Fleets?](https://ronjeffries.com/articles/-y023/python/-o110/110/), Ron writes

> And it seems to me that with a video-oriented game, we always wind up needing
> to watch it run, both for confidence and because, every now and then, we
> discover a problem.

My goal is to not have to run the game for confidence. I want faster feedback
for confidence.

However, I do agree that you will discover problems when running the game. But
I think that is true for any kind of application, not just games.

## Summary

Those are my reflections on using TDD for a game so far. Do you believe that
creating test applications for faster feedback is a good idea?  Please [let me
know](/contact/index.html).

See you in the next episode!
