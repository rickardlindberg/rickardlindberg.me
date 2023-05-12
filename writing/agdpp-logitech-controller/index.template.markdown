---
title: Programming a Logitech controller
date: 2023-05-12
tags: agdpp,draft
agdpp: true
---

I recently bought a pair of Logitech Xbox-style controllers that me and my son
has used to play Super Tux Kart.

I want to be able to use those controllers in the balloon shooter as well. My
suspicion is that the balloon shooter will feel many times more as a "real"
game if we can control it using a real game controller. Even though we are all
about having fun here and learning, we still want this to feel like a real
game, not some toy example. So let's get started.

## Learning about events

How do we capture events from a Logitech controller?

One way to find out is to print all the events that pygame generates. we can
for example do that here:

$:output:python:
class BalloonShooter:

    ...

    def tick(self, dt, events):
        for event in events:
            print(event)
            ...
$:END

This makes the test suite fail since the print statement is outputting event
information that the test does not expect to find.

This might be a downside of doctest, that it captures stdout and asserts on it.
Normally a print statement should not affect the logic of the code, so it
should be fine.

On the other hand, if we use print statements for debugging, maybe it's a good
thing that out test suite fails so that we are remembered to keep the debug
session short and remove it once we are done.

Anyway, if we run the game now and press keys we can see things like this in
the output:

$:output:text:
<Event(771-TextInput {'text': ' ', 'window': None})>
<Event(769-KeyUp {'unicode': ' ', 'key': 32, 'mod': 0, 'scancode': 44, 'window': None})>
<Event(768-KeyDown {'unicode': '', 'key': 1073742049, 'mod': 1, 'scancode': 225, 'window': None})>
<Event(768-KeyDown {'unicode': '', 'key': 1073742050, 'mod': 257, 'scancode': 226, 'window': None})>
$:END

So we can clearly see what pygame thinks is happening.

But when we press a key on the Logitech controller, nothing happens.

But we see something about a joystick further up:

$:output:text:
...
$:END

## Initializing joysticks

We read about joysticks in the [pygame documentation](). It seems like they
must be initialized before events are generated for them.

...

## Summary

See you in the next episode!

## TODO

$:output:python:
$:END

<center>
![Bug with drawing circles on negative x values.](negative-x-draw-bug.png)
</center>

* Joystick

    * work towards a nice input handler (mocks vs state based)
    * Angle class

    commit 3b17692a64e48fc8712c0c085cdf94e063926251
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sat Apr 29 06:08:41 2023 +0200

        Event looping is done in loop.

    ...

    commit 1e6d7ba35e721c591a7246a71af633ccbe17e0df
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Tue May 2 06:41:55 2023 +0200

        InputHandler does not now arrow angle, just the turn angle.
