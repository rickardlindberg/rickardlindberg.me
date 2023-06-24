---
title: Multiplayer
date: 2023-06-17
tags: agdpp,draft
agdpp: true
---

For every story we work on, the balloon shooter feels more and more like a real
game. The initial goal of this project was to create a game that me and my son
will enjoy playing together. At this point, I think the most valuable thing we
can work on is adding support for multiplayer. That's what we will work on in
this episode.

## A new layer

The entry point for the balloon shooter looks like this:

$:output:python:
if __name__ == "__main__":
    BalloonShooter.create().run()
$:END

The balloon shooter class instantiates a `GameScene` which implements the logic
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

This means that as soon as we start the application, we enter the gameplay mode
and can start playing right away.

I imagine that multiplayer mode works by first selecting which players should
participate in shooting balloons, and after that, the gameplay mode is entered.

We want to go from this structure

$:output:text:
BalloonShooter
    GameScene
$:END

to something like

$:output:text:
BalloonShooter
    NewGameScene
        StartScene
        GameScene
$:END

We want to add another level that first directs calls to a start screen (or
player select screen) and once that is done, initializes the game scene.

Current test for `GameScene` should pass unchanged, but tests for
`BalloonShooter` will need some modifications. I imagine that those tests need
to select a player before asserting something from the gameplay mode.

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

And insert it like this:

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

We write the initial version of `StartScene` like this:

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
implementation does not check events at all, so this test, for example, will
pass:

$:output:python:
"""
>>> start = StartScene(screen_area=Rectangle.from_size(500, 500))
>>> start.update(0)
>>> start.get_players() is None
True
>>> start.update(0)
>>> start.get_players()
['one']
"""
$:END

So if we were to take this start scene into play now, we just need to wait for
two iterations (2/60th of a second) and it would report players `['one'`]. That
does not seem correct, but we can get back to it later.

Let's work on integrating the start scene.

## Take start scene into play

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
