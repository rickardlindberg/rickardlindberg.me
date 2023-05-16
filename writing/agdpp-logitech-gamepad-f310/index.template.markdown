---
title: Programming a Logitech Gamepad F310
date: 2023-05-12
tags: agdpp,draft
agdpp: true
---

I recently bought a pair of Logitech gamepads that me and my son use when
playing [SuperTuxKart](https://supertuxkart.net/Main_Page):

<p>
<center>
![Logitech Gamepad F310.](logitech-gamepad-f310.png)
</center>
</p>

I want to be able to use those gamepads in the balloon shooter as well. My
suspicion is that the balloon shooter will feel many times more like a "real"
game if we can control it using "real" game controllers. Even though we are all
about having fun here and learning, we still want this to feel like a real
game, not some toy example. So let's get started.

## Learning about events

How do we capture events from a Logitech gamepad?

One way to find out is to print all the events that pygame generates. We can
for example do that in the `tick` method:

$:output:python:
class BalloonShooter:

    ...

    def tick(self, dt, events):
        for event in events:
            print(event)
            ...
$:END

This makes the test suite fail since the print statement is outputting event
information that the tests do not expect to find.

This might be a downside of doctest, that it captures stdout and asserts on it.
Normally a print statement should not affect the function of the code, so it
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

But when we press a key on the Logitech gamepad, nothing happens.

However, if we look at the beginning of the event log, we see this:

$:output:text:
<Event(1541-JoyDeviceAdded {'device_index': 0, 'guid': '030000006d0400001dc2000014400000'})>
$:END

Is this our Logitech gamepad?

## Initializing joysticks

We read about joysticks in the [pygame
documentation](https://www.pygame.org/docs/ref/joystick.html). It seems like
they must be initialized before events are generated for them.

> Joysticks are initialised on creation and are shut down when deallocated.
> Once the device is initialized the pygame event queue will start receiving
> events about its input.

We try to mimic the example in the documentation to initialize joysticks:

$:output:python:
class GameLoop(Observable):

    ...

    def run(self, game, resolution=(1280, 720), fps=60):
        ...
        joysticks = {}
        try:
            while True:
                pygame_events = self.pygame.event.get()
                for event in pygame_events:
                    if event.type == pygame.JOYDEVICEADDED:
                        joy = self.pygame.joystick.Joystick(event.device_index)
                        joysticks[joy.get_instance_id()] = joy
                    else:
                        game.event(Event(event))
                ...
$:END

We don't handle `JOYDEVICEREMOVED` yet. We probably should, but unless we
unplug the gamepad while running the game, we should be fine I think.

This change passes all the tests. However, we are never simulating the
`JOYDEVICEADDED` event, so the code is never executed.

I think we will get faster feedback by just testing this thing for real. We can
come back and describe the joystick handling code in the form of tests later on
if we feel the need. And maybe test the `JOYDEVICEREMOVED` as well.

Anyway, if we run the game now and press keys on the gamepad, we see events
like this:

$:output:text:
<Event(1536-JoyAxisMotion {'joy': 0, 'instance_id': 0, 'axis': 0, 'value': 0.003906369212927641})>
<Event(1539-JoyButtonDown {'joy': 0, 'instance_id': 0, 'button': 0})>
<Event(1540-JoyButtonUp {'joy': 0, 'instance_id': 0, 'button': 0})>
$:END

I feel a disproportional sense of excitement and joy over this. We can now get
input from the Logitech gamepad. We are real game developers now! Now it's a
matter of mapping events to actions in our game.

## Isolating input handling

For now, we want to be able to play our game with both the keyboard and the
Logitech gamepad. I will most likely use the gamepad 99% of the time, but if
you don't have it, we still want you to be able to play the game.

Input handling is therefore something that is starting to become a little
complicated. It's not just a matter of mapping one event to one action.

Now, we have this:

$:output:python:
class GameScene(SpriteGroup):

    ...

    def event(self, event):
        ...
        elif event.is_keydown_space():
            self.flying_arrows.add(self.arrow.clone_shooting())
        elif event.is_keydown_left():
            self.arrow.angle_left()
        elif event.is_keydown_right():
            self.arrow.angle_right()
$:END

That is a one to one mapping between events and actions.

We still want this code to look similar but allow multiple events to generate
the same actions.

Here is what we come up with:

$:output:python:
class GameScene(SpriteGroup):

    ...

    def event(self, event):
        self.input_handler.action(event)

    def update(self, dt):
        self.input_handler.update(dt)
        if self.input_handler.get_shoot():
            self.flying_arrows.add(self.bow.clone_shooting())
        self.bow.turn(self.input_handler.get_turn_angle())
$:END

So we pass along events to an input handler, then we query it in the `update`
method, asking it if a shot action was triggered (from either input device),
and if so, modify `flying_arrows` as before. We do something similar for
turning the arrow. But instead of asking the input handler if a left/right
action was triggered, we ask it for an angle that we should turn the arrow.
Since the arrow can be turned with variable speed with the Logitech gamepad,
this makes more sense.

Before we move on to the input handler, I want to discuss another thing that is
new here: the bow.

## Bow

Instead of doing `arrow.angle_left/right()` we do `bow.turn(angle)`. We have
extracted a concept called bow.

Right now it is a wrapper around an arrow, but the idea is that you might want
to draw more graphics for the bow.

Here is what it looks like:

$:output:python:
class Bow(SpriteGroup):

    def __init__(self):
        SpriteGroup.__init__(self)
        self.arrow = self.add(Arrow())

    def turn(self, angle):
        self.arrow.set_angle(self.arrow.angle.add(angle))

    def clone_shooting(self):
        return self.arrow.clone_shooting()

    ...
$:END

I'm not sure that bow is the right name. Do we shoot arrows with a bow in our
game? Or is it some kind of cannon? I think we need to ask our product owner.

At the moment we are not doing any drawing except the arrow, so the bow just
acts as a placeholder to attract new functionality. And the concept of a bow
makes sense. You need to shoot the arrow with something. And when you shoot,
the arrow leaves the bow and goes into the list of flying arrows.

## Input handler

Ok, on to the input handler.

It is responsible for handling events and keeping some state of that those
events should result in.

Let's look at how it handles shooting:

$:output:python:
class InputHandler:

    def __init__(self):
        ...
        self.shoot_down = ResettableValue(False)

    def get_shoot(self):
        return self.shoot

    def update(self, dt):
        self.shoot = self.shoot_down.get_and_reset()
        ...

    def action(self, event):
        if event.is_keydown(KEY_SPACE) or event.is_joystick_down(XBOX_A):
            self.shoot_down.set(True)
        ...
$:END

It will be called by the game scene like this:

$:output:python:
self.input_handler.action(event)
self.input_handler.update(dt)
if self.input_handler.get_shoot():
    ...
$:END

The `shoot_down` variable remembers if a shoot key/button has been pressed
since the last call to `update`. We only want `get_shoot` to return true one
time when we press a shoot key/button.

The resettable value looks like this:

$:output:python:
class ResettableValue:

    def __init__(self, default):
        self.default = default
        self.value = default

    def get_and_reset(self):
        x = self.get()
        self.reset()
        return x

    def get(self):
        return self.value

    def set(self, value):
        self.value = value

    def reset(self):
        self.value = self.default
$:END

The `is_joystick_down` method on the event is new. We have added wrappers for
new events [before](/writing/agdpp-shooting-arrow/index.html), and this is done
the same way.

The logic for the turn angle is a little more complicated. The input handler
remembers what state the keyboard and gamepad is in. For the keyboard, it is if
a turn key is currently pressed or not. For the gamepad, it is the current x
position of the joystick. We store that state in `arrow_turn_factor`. It is a
value between -1 and 1. -1 means turn full speed to the left. 1 means turn full
speed to the right. The keyboard can only turn with full speed but the gamepad
can turn with variable speed by moving the joystick into different x positions.
(We could imagine that the turn factor for the keyboard increase over time. So
the speed increases the longer you have held a turn button down. That kind of
logic would go in here and the game would still only query for the turn angle.)

Here is the implementation:

$:output:python:
class InputHandler:

    def __init__(self):
        self.arrow_turn_factor = ResettableValue(0)
        ...

    def get_shoot(self):
        return self.shoot

    def get_turn_angle(self):
        return self.turn_angle

    def update(self, dt):
        ...
        self.turn_angle = Angle.fraction_of_whole(self.arrow_turn_factor.get()*dt*1/2000)

    def action(self, event):
        ...
        elif event.is_keydown(KEY_LEFT):
            self.arrow_turn_factor.set(-1)
        elif event.is_keyup(KEY_LEFT):
            self.arrow_turn_factor.reset()
        elif event.is_keydown(KEY_RIGHT):
            self.arrow_turn_factor.set(1)
        elif event.is_keyup(KEY_RIGHT):
            self.arrow_turn_factor.reset()
        elif event.is_joystick_motion() and event.get_axis() == 0:
            if abs(event.get_value()) > 0.01:
                self.arrow_turn_factor.set(event.get_value())
            else:
                self.arrow_turn_factor.reset()
$:END

We also have extracted an `Angle` class.

## Design note

It took me a few refactorings and experimentation to end up with this design
for the input handler.

* work towards a nice input handler (mocks vs state based)

$:output:python:

$:END

## Summary

See you in the next episode!
