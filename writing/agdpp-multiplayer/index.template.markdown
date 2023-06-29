---
title: Multiplayer
date: 2023-06-29
tags: agdpp
agdpp: true
---

For every story that we work on, the balloon shooter feels more and more like a
real game. The initial goal of this project was to create a game that me and my
son will enjoy playing *together*. At this point, I think the most valuable
thing we can work on towards that goal is adding support for multiplayer.
So that's the topic for this episode.

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
participate in shooting balloons, and after that, the gameplay mode is entered
and each player gets their own bow to shoot with.

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

We want to add another level that first directs calls to a start scene (or
player select scene) and once players are selected, initializes the game scene
and directs call to that.

The current tests for `GameScene` should pass unchanged, but tests for
`BalloonShooter` will need some modifications. I imagine that those tests need
to select a player before asserting something from the gameplay mode. We'll see
later.

## Refactor to new structure

Let's start by slowly and carefully refactor towards this new structure, using
our tests as a safety net to give us feedback about how we're doing.

I want to call the new layer `GameScene`, but that name is already taken. The
current game scene is really the gameplay scene, so we rename it to that. Then
we create the new game scene which just forwards its calls to the gameplay
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

The idea is that a player (keyboard or gamepad) selects to be part of the game
by shooting. When all players have entered, one of them can shoot again to
start the game. This functionality is not yet fully implemented above. But this
will do for now.

When writing this blog post and looking at the code, I notice two problems.
First of all "on" should be "one" in the test description. Second of all, the
implementation does not check events at all, so if the test does not simulate
any events, it will still pass. If we were to take this start scene into play
now, we just need to wait for two iterations (2/60th of a second) and it would
report players `['one']`. That does not seem correct.

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

I wonder how common the event + update pattern is in our tests. Perhaps we can
benefit from a test helper something like this:

$:output:python:
def cycle(sprite, events=[], dt=0):
    for event in events:
        sprite.event(event)
    sprite.update(dt)
    sprite.update(dt)
$:END

We might try it in a few places and see if the tests read better. But not now.
The modification to the tests forces us to check events. We do it like this:

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
scene and switch it to the gameplay scene once we have selected players:

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

I'm also not sure how I feel about the assertions that checks the type of the
active scene. But I don't have any ideas for a better way to express that. If
you do, please let me know.

When we run the game now, it shows a blank purple screen. If we shoot twice we
enter the gameplay scene and the game starts as before. Perfect!

We do not yet take players into account and we can still not have multiple
players. What we do have is a skeleton with a few more places where this new
functionality can be added.

The game works fine now (if we know that we have to shoot twice to get passed
the start scene), but a test fails. It is the test for the balloon shooter.
Here it is:

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

And, we are back to green!

Here is yet another example of overlapping, sociable testing. We yet again have
to simulate two shoot events to select players.

One downside of this approach is that if we were to change the logic for
selecting players, say that we first need to shoot and then turn left, then we
would have to modify three test I think. One way to make that less of a problem
in this particular situation is to create a test helper something like this:

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

## Pass players to game scene

Our skeleton for the new feature is not quite complete. The gameplay scene does
not know about players. Let's fix that by passing the players from the start
scene to the gameplay scene like this:

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

To make this work we also add that argument to the constructor of the gameplay
scene:

$:output:diff:
@@ -333,11 +336,13 @@ class GameplayScene(SpriteGroup):
     []
     """

-    def __init__(self, screen_area, balloons=[], arrows=[]):
+    def __init__(self, screen_area, balloons=[], arrows=[], players=["default"]):
$:END

Now, I think our skeleton is complete. What do I mean by that? I mean that all
the pieces are connected they way we think they should be. Now we can work
individually on the start scene and the gameplay scene. The start scene needs
to be able to select multiple players and should return those players in the
list. The gameplay scene should take players into account and create one bow
per player that it can control.

## Make input handler player aware

The start scene uses the input handler's `get_shoot` to detect shots:

$:output:python:
class StartScene(SpriteGroup):

    ...

    def update(self, dt):
        SpriteGroup.update(self, dt)
        self.input_handler.update(dt)
        if self.input_handler.get_shoot():
            self.shots += 1
$:END

However, to select multiple players, the start scene must know *who* shot.
Let's modify the input handler to support that. We write this test:

$:output:python:
"""
>>> i = InputHandler()

>>> i.update(0)
>>> i.get_shots()
[]

>>> i.event(GameLoop.create_event_keydown(KEY_SPACE))
>>> i.event(GameLoop.create_event_joystick_down(XBOX_A, instance_id=7))
>>> i.update(0)
>>> i.get_shots()
['keyboard', 'joystick7']

>>> i.update(0)
>>> i.get_shots()
[]
"""
$:END

We create a new `get_shots` method that returns a list of player/input
identifiers. If the shot is triggered by the keyboard, the player identifier is
`keyboard`.  If the shot is triggered by a gamepad, the player identifier is
`joystick` plus the unique id of that joystick. We implement it like this:

$:output:python:
class InputHandler:

    def __init__(self):
        self.shots_triggered = []
        ...

    def event(self, event):
        if event.is_keydown(KEY_SPACE):
            self.shots_triggered.append("keyboard")
        elif event.is_joystick_down(XBOX_A):
            self.shots_triggered.append(self.joystick_id(event))
        ...

    def joystick_id(self, event):
        return f"joystick{event.get_instance_id()}"

    def update(self, dt):
        self.shots = self.shots_triggered
        self.shots_triggered = []
        ...

    def get_shots(self):
        return self.shots

    ...
$:END

## Start scene returns players

Now, let's see if we can make `StartScene.get_players` to return actual player
identifiers instead of hard coded `['one']`. As usual, we start with a test:

$:output:python:
"""
>>> start = StartScene(screen_area=Rectangle.from_size(500, 500))
>>> start.get_players() is None
True
>>> start.event(GameLoop.create_event_joystick_down(XBOX_A, instance_id=7))
>>> start.event(GameLoop.create_event_joystick_down(XBOX_A, instance_id=7))
>>> start.update(0)
>>> start.get_players()
['joystick7']
"""
$:END

We make the test pass like this:

$:output:python:
class StartScene(SpriteGroup):

    def __init__(self, screen_area):
        ...
        self.pending_players = []
        self.players = None

    def update(self, dt):
        ...
        for player in self.input_handler.get_shots():
            if player in self.pending_players:
                self.players = self.pending_players
            else:
                self.pending_players.append(player)

    def get_players(self):
        return self.players

    ...
$:END

## Multiple bows in game scene

At this point, the start scene returns a correct list of players selected and
the only piece missing is for the gameplay scene to create multiple bows and
direct events to the correct bow.

Instead of having just a single bow, we create multiple bows like this:

$:output:diff:
-        self.bow = self.add(Bow())
+        self.bows = {}
+        bow_position = self.screen_area.bottomleft.move(dy=-120)
+        bow_increment = self.screen_area.width / (len(players)+1)
+        for player in players:
+            bow_position = bow_position.move(dx=bow_increment)
+            self.bows[player] = self.add(Bow(position=bow_position))
$:END

Then we forward events to the correct bow like this:

$:output:diff:
-        if self.input_handler.get_shoot():
-            self.flying_arrows.add(self.bow.shoot())
-        self.bow.turn(self.input_handler.get_turn_angle())
+        for player in self.input_handler.get_shots():
+            self.flying_arrows.add(self.bow_for_player(player).shoot())
+        for player, turn_angle in self.input_handler.get_turn_angles().items():
+            self.bow_for_player(player).turn(turn_angle)
$:END

Here we use `InputHandler.get_turn_angles` to get turn angles per player. It is
implemented similarly to how we implemented `InputHandler.get_shots`.

To get the correct bow, we use this:

$:output:python:
def bow_for_player(self, player):
    for input_id, bow in self.bows.items():
        if input_id == player:
            return bow
    return bow
$:END

If no player is found, the last bow is returned. So if you attach another
gamepad after the gameplay mode has entered, it will control the last bow. Not
sure if that is right. We'll have to ask our product owner.

We didn't write any tests for this new behavior. We do have tests that check
that a single player can shoot and turn. That gives us confidence that the new
for loops work. There could be an error in `bow_for_player` so that an input
event controls the wrong bow. The tests would not catch that. But I find
that unlikely, and I'm not worried about it happening.

## End result

If we start the game now, we are greeted, again, with a blank purple screen:

<p>
<center>
![Empty start scene.](empty-start.png)
</center>
</p>

If we shoot once with the keyboard, then twice with the gamepad, we are taken
to this scene where the keyboard and the gamepad can control their own bow:

<p>
<center>
![First version of multiplayer.](multiplayer-first.png)
</center>
</p>

And we have the first version of a working multiplayer mode!

## Polishing

An empty start scene does not feel polished. Let's add some instructions to
inform players how to get passed it. It mostly involves doing `loop.draw_text`
in the draw method. Not very interesting. However, let's also add some animated
balloons in the background to make the scene a little more interesting. Thanks
to the extraction of `Balloons` that we did in the
[previous](/writing/agdpp-spawn-multiple-balloons/index.html) episode, we can
do this with the following lines:

$:output:python:
class StartScene(SpriteGroup):

    def __init__(self, screen_area):
        SpriteGroup.__init__(self)
        positions = [
            Point(
                x=screen_area.get_random_x(),
                y=random.randint(screen_area.topleft.y, screen_area.bottomright.y)
            )
            for x in range(15)
        ]
        self.add(Balloons(
            positions=positions,
            number_of_balloons=len(positions),
            screen_area=screen_area
        ))
        ...

    ...
$:END

I really should have created `screen_area.get_random_x()` or even
`screen_area.get_random_position()`. But I got carried away and wanted a
result quickly. We add a note about that and might address it in a future
refactoring.

Anyway, here is the final result of the start scene:

<p>
<center>
![Start scene with instructions.](start-instructions.png)
</center>
</p>

Here we have selected two players: one with the keyboard and one with a
gamepad. If any of them shoot again, we enter the gameplay mode, or we can
continue to add players by shooting with a different gamepad.

As a final polish we will make different players have different colors. The
result:

<p>
<center>
![Players with different colors.](multiplayer-colors.png)
</center>
</p>

## A reflection on stories

How many stories have we worked on in this episode?

Well, we have added support for multiplayer, isn't that just one story?

But we also did some polishing. Polishing could easily be its own story. Polish
adds value to the players of the game.

So the stories might be

* Add multiplayer support
* Nicer looking, more informative start scene
* Different player colors

The first one is a lot bigger than the others. Is it possible to split it so
that all stories that we work on have roughly the same size? I'm not sure.
Let's think about it.

Let's think about the state that the game was in when we had a start scene but
the players could not be selected. We had visible change in behavior. There was
now a start scene that wasn't there before. But had we added value? Players
expecting multiplayer would be disappointed. Other players would have to shoot
a couple of times extra before they can play the game. That doesn't seem like
value. However, players could see this new start scene and ask questions like
"what is this?" and "what am I supposed to do here?" We can tell them our idea
and they can give us feedback if we are on the right track. Perhaps they want
to start a multiplayer session in a different way? Perhaps they think a
descriptive text on the start scene is more important? That feedback has value.

So we could at least split the first story into two:

* Player selection start scene
* One bow per player

As long as we can show visible progress, I think the story has value.

## Summary

With the new start scene and multiplayer mode, the balloon shooter feels even
more like a real game.  I find myself wanting to go play the game and enjoy
what we have created. That is a really nice feeling.

I am a bit surprised what you can achieve with the only graphics primitives
being circles and text. I mean, the look of the game is pretty bad, the colors
are horrible, and yet the idea comes across nicely and game mechanics can be
felt anyway. I wonder how much of an improvement it would be to improve
graphics. Probably a lot. But I am still surprised how far circles and text
have taken us.

See you in the next episode!
