---
title: Multiplayer
date: 2023-06-17
tags: agdpp,draft
agdpp: true
---

For every story that we work on, the balloon shooter feels more and more like a
real game. The initial goal of this project was to create a game that me and my
son will enjoy playing together. At this point, I think the most valuable thing
we can work on towards that goal is adding support for multiplayer. That's what
we will work on in this episode.

## A new layer

The entry point for the balloon shooter looks like this:

$:output:python:
if __name__ == "__main__":
    BalloonShooter.create().run()
$:END

The balloon shooter class instantiates a game scene which implements the logic
of our game:

$:output:python:
class BalloonShooter:

    def __init__(self, loop):
        ...
        self.game_scene = GameScene(Rectangle.from_size(*self.resolution))

    def tick(self, dt):
        self.game_scene.update(dt)
        self.loop.clear_screen()
        self.game_scene.draw(self.loop)

    ...
$:END

This means that as soon as we start the game, we enter the gameplay mode and
can start playing right away.

I imagine that multiplayer mode works by first selecting which players should
participate in shooting balloons, and after that, the gameplay mode is entered.

We want to go from this structure:

$:output:text:
BalloonShooter
    GameScene
$:END

To something like this:

$:output:text:
BalloonShooter
    NewGameScene
        StartScene
        GameScene
$:END

We want to add another level that first directs calls to a start screen (or
player select screen) and once that is done, initializes the game scene.

Current tests for `GameScene` should pass unchanged, but tests for
`BalloonShooter` will need some modifications. I imagine that those tests need
to select a player before asserting something from the gameplay mode. We'll see
later.

## Refactor to new structure

Let's start by slowly and carefully refactor towards this new structure, using
our tests as a safety net to give us feedback about how we're doing.

I want to call the new layer `GameScene`, but that name is already taken. The
current game scene is really the gameplay scene, so we rename it to that. Then
we create the new `GameScene` which just forwards its calls to the gameplay
scene:

$:output:python:
class GameScene:

    def __init__(self, screen_area):
        self.gameplay = GameplayScene(screen_area=screen_area)

    def event(self, event):
        self.gameplay.event(event)

    def update(self, dt):
        self.gameplay.update(dt)

    def draw(self, loop):
        self.gameplay.draw(loop)
$:END

We insert this new layer in `BalloonShooter` like this:

$:output:diff:
@@ -113,7 +113,7 @@ class BalloonShooter:
     def __init__(self, loop):
         self.loop = loop
         self.resolution = (1280, 720)
-        self.game_scene = GameplayScene(Rectangle.from_size(*self.resolution))
+        self.game_scene = GameScene(screen_area=Rectangle.from_size(*self.resolution))
$:END

The new layer is now added, all tests are passing, and we have a point in our
code (`GameScene`) where we can put functionality to choose between a start
scene and a gameplay scene.

Before we can work on that behavior, we need a start scene.

## Start scene

We write the initial version of the start scene like this:

$:output:python:
class StartScene(SpriteGroup):

    """
    I report players when on player has shot twice:

    >>> start = StartScene(screen_area=Rectangle.from_size(500, 500))
    >>> start.get_players() is None
    True

    >>> start.event(GameLoop.create_event_joystick_down(XBOX_A))
    >>> start.update(0)
    >>> start.get_players() is None
    True

    >>> start.event(GameLoop.create_event_joystick_down(XBOX_A))
    >>> start.update(0)
    >>> start.get_players()
    ['one']
    """

    def __init__(self, screen_area):
        SpriteGroup.__init__(self)
        self.input_handler = InputHandler()
        self.shots = 0

    def event(self, event):
        self.input_handler.action(event)

    def update(self, dt):
        SpriteGroup.update(self, dt)
        self.shots += 1

    def get_players(self):
        if self.shots > 1:
            return ["one"]
$:END

The idea is that a player (keyboard, gamepad) selects to be part of the game by
shooting. When all players have entered, one of them can shoot again to start
the game. This is not yet complete.

When writing this blog post and looking at the code, I notice two problems.
First of all "on" should be "one" in the test description. Second of all, the
implementation does not check events at all, so a test that does not simulate
any events will still pass.  So if we were to take this start scene into play
now, we just need to wait for two iterations (2/60th of a second) and it would
report players `['one'`]. That does not seem correct.

Let's fix that. We modify the test to do two updates and the assertions should
be the same:

$:output:python:
"""
>>> start.event(GameLoop.create_event_joystick_down(XBOX_A))
>>> start.update(0)
>>> start.update(0)
>>> start.get_players() is None
True

>>> start.event(GameLoop.create_event_joystick_down(XBOX_A))
>>> start.update(0)
>>> start.update(0)
>>> start.get_players()
['one']
"""
$:END

I wonder how common the event + update pattern is in our test. Perhaps we can
benefit from a test helper something like this:

$:output:python:
def cycle(sprite, events=[], dt=0):
    for event in events:
        sprite.event(event)
    sprite.update(dt)
    sprite.update(dt)
$:END

We might try it in a few places and see if the tests read better. But now now.
The modification to the tests force us to check events. We do it like this:

$:output:python:
class StartScene(SpriteGroup):

    def update(self, dt):
        SpriteGroup.update(self, dt)
        self.input_handler.update(dt)
        if self.input_handler.get_shoot():
            self.shots += 1

    ...
$:END

With that fix out of the way, let's work on integrating the start scene.

## Take start scene into play

The game scene currently forwards all calls to the gameplay scene. To take the
start scene into play, we first want the start scene to be active, and have the
game scene forward calls to it. Once players have been selected, we want the
game scene to switch the active scene to the gameplay scene.

We express that in the following test:

$:output:python:
class GameScene:

    """
    Initially, I draw the start scene:

    >>> game = GameScene(screen_area=Rectangle.from_size(500, 500))
    >>> isinstance(game.active_scene, StartScene)
    True

    When players have been selected, I draw the gameplay scene:

    >>> game.event(GameLoop.create_event_keydown(KEY_SPACE))
    >>> game.update(0)
    >>> isinstance(game.active_scene, StartScene)
    True

    >>> game.event(GameLoop.create_event_keydown(KEY_SPACE))
    >>> game.update(0)
    >>> isinstance(game.active_scene, StartScene)
    False
    """
$:END

This is an example of an overlapping, sociable test. To make the scene switch
happen, we need `StartScene.get_players` to return something. Since the game
scene uses the real start scene, and not a mock, the only way to make it return
something is to perform the same actions as we did in the start scene tests.

To make this test pass, we initialize an active scene variable to the start
scene and switch it to the gameplay scene once we have enough players:

$:output:python:
class GameScene:

    def __init__(self, screen_area):
        self.screen_area = screen_area
        self.active_scene = StartScene(screen_area=self.screen_area)

    def update(self, dt):
        self.active_scene.update(dt)
        if isinstance(self.active_scene, StartScene):
            if self.active_scene.get_players():
                self.active_scene = GameplayScene(screen_area=self.screen_area)

    ...
$:END

The test talks about switching to a gameplay scene, but it only asserts that
the start scene is *not* active anymore. We could probably clarify that.

When we run the game now it will show a blank purple screen. If we shoot twice
we will enter the gameplay scene and the game starts as before. Perfect!

We do not yet take players into account and we can still not have multiple
players. What we do have is a skeleton with a few more pieces where this new
functionality will fit.

The game works fine now (if we know that we have to shoot twice to get pass the
start screen), but a test fails. It is the test for the balloon shooter. Here
it is:

$:output:python:
class BalloonShooter:

    """
    We run the game for a few frames, then quit:

    >>> events = BalloonShooter.run_in_test_mode(
    ...     events=[
    ...         [],
    ...         [],
    ...         [],
    ...         [],
    ...         [],
    ...         [],
    ...         [GameLoop.create_event_user_closed_window()],
    ...     ]
    ... )

    The game loop is initialized and cleaned up:

    >>> events.filter("GAMELOOP_INIT", "GAMELOOP_QUIT")
    GAMELOOP_INIT =>
        resolution: (1280, 720)
        fps: 60
    GAMELOOP_QUIT =>

    ...
    """
$:END

This test is at the outermost level, so it includes all objects. Before, the
gameplay scene received the events from the test, but now the start scene
receives them. The start scene does not handle the user closed window event
which results in this test just hanging.

That failure teaches us that we can't quit the application when we are in the
start scene, only when we are in the gameplay scene. That is probably not
correct. Thank you test for pointing that out. However, the assertions that
follow check for example that a balloon is drawn, so the test expects to be in
the gameplay mode. We modify the test to include two shoot events so that we
end up in the gameplay scene:

$:output:python:
"""
>>> events = BalloonShooter.run_in_test_mode(
...     events=[
...         [GameLoop.create_event_keydown(KEY_SPACE)],
...         [GameLoop.create_event_keydown(KEY_SPACE)],
...         [],
...         [],
...         [],
...         [],
...         [],
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
"""
$:END

Here is yet another example of overlapping, sociable testing. We yet again have
to simulate two shoot events to select players.

One downside of this approach is that if we were to change the logic for
selecting players. Say that we first need to shoot and the turn left. Then we
would have to modify 3 test I think. One way to make that less of a problem in
this particular situation is to create a test helper something like this:

$:output:python:
def events_to_select_one_player():
    return [
        GameLoop.create_event_keydown(KEY_SPACE),
        GameLoop.create_event_keydown(KEY_SPACE),
    ]
$:END

We could use that test helper in all tests (with some modification) and now
there is only one place in the tests that knows about what events that gets us
from the start scene to the gameplay scene with one player.

## Players to game scene

$:output:diff:
@@ -162,7 +162,10 @@ class GameScene:
         self.active_scene.update(dt)
         if isinstance(self.active_scene, StartScene):
             if self.active_scene.get_players():
-                self.active_scene = GameplayScene(screen_area=self.screen_area)
+                self.active_scene = GameplayScene(
+                    screen_area=self.screen_area,
+                    players=self.active_scene.get_players()
+                )
$:END

## Flesh out

* Start scene can select multiple players.

## Summary

See you in the next episode!

## TODO

    commit 9804dce5d6f789274161a7b2c84a36f43cd33c23
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 14:31:38 2023 +0200

        Rename GameScene -> GameplayScene (so that we can create a StartScene and a GameScene to coordinate the two.)

    ...

    commit af8d01b4ba7cce46dcd223309e26a79a43515348
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 17:01:46 2023 +0200

        Bows are layed out evenly at the bottom of the screen.

    * got carried away and improved the start screen

        * was able to reuse Balloons just for the animation!

    commit bc7d1ca865a51b47e8efd0004dca288ebcc9ec33
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 17:20:36 2023 +0200

        Start screen draws helpful text.

    ...

    commit 4b03cc6b896b716e2ccbf546f600e49913cbfada (HEAD -> main, origin/main)
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 17:58:33 2023 +0200

        Nicer looking start screen.

    * want to go play sometimes
        * that feeling when you have created something and want to go have a
          look at it
        * same feeling as when I made websites 20 years ago
        * that feeling!

$:output:python:
$:END
