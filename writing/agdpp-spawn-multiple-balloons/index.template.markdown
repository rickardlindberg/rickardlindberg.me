---
title: Spawn multiple balloons
date: 2023-06-12
tags: agdpp,draft
agdpp: true
---

We [previously](/writing/agdpp-game-over/index.html) had a story about balloons
moving downwards. We scratched that because other stories were more important
for the first version of the balloon shooter. With those stories done, I think
a more realistic balloon spawning and movement pattern is the most valuable
thing we can work on.

## Video version

The video version of this episode:

    TODO

## Code review

Let's review our code and look at how balloons are managed.

Our game scene has a sprite group for balloons which by default contains only
one balloon:

$:output:python:
class GameScene(SpriteGroup):

    ...

    def __init__(self, balloons=[(50, 50)], ...):
        ...
        self.balloons = self.add(SpriteGroup([
            Balloon(Point(x=x, y=y)) for (x, y) in balloons
        ]))
$:END

This sprite group is modified in the `update` method if an arrows hits a
balloon:

$:output:python:
class GameScene(SpriteGroup):

    ...

    def update(self, dt):
        ...
        for arrow in self.flying_arrows.get_sprites():
            ...
            for balloon in self.balloons.get_sprites():
                if arrow.hits_baloon(balloon):
                    self.balloons.remove(balloon)
                    self.balloons.add(Balloon(position=Point(x=50, y=50)))
                    ...
$:END

So the hit balloon is removed, and a new one is added.

How do balloons move?

$:output:python:
class Balloon:

    ...

    def __init__(self, position, radius=40):
        self.position = position
        self.radius = radius

    def update(self, dt):
        if self.position.x > 1200:
            self.position = self.position.set(x=50)
        else:
            self.position = self.position.move(dx=dt*0.3)
$:END

They move from left to right and wrap around at x=1200.

## Strategy

To be able to write more isolated tests for balloon behavior, I want to start
with a few refactorings. I want to extract a `Balloons` class which contains
most logic related to balloons. Then I want to write tests for new behavior.
This is also known as make the change easy, then make the easy change.

We begin by creating the class and using it like this:

$:output:diff:
-        self.balloons = self.add(SpriteGroup([
-            Balloon(Point(x=x, y=y)) for (x, y) in balloons
-        ]))
+        self.balloons = self.add(Balloons(balloons))
$:END

$:output:python:
class Balloons(SpriteGroup):

    def __init__(self, positions):
        SpriteGroup.__init__(self, [
            Balloon(Point(x=x, y=y)) for (x, y) in positions
        ])
$:END

We continue to move some behavior into this new class:

$:output:python:
class Balloons(SpriteGroup):

    ...

    def get_balloon_hit_by_arrow(self, arrow):
        for balloon in self.get_sprites():
            if arrow.hits_baloon(balloon):
                return balloon

    def spawn_new(self):
        self.add(Balloon(position=Point(x=50, y=50)))
$:END

With that in place, we can simplify the update code to this:

$:output:diff:
-            for balloon in self.balloons.get_sprites():
-                if arrow.hits_baloon(balloon):
-                    self.balloons.remove(balloon)
-                    self.balloons.add(Balloon(position=Point(x=50, y=50)))
-                    self.score.add(1)
+            hit_balloon = self.balloons.get_balloon_hit_by_arrow(arrow)
+            if hit_balloon:
+                self.balloons.remove(hit_balloon)
+                self.balloons.spawn_new()
+                self.score.add(1)
$:END

There is probably some more functionality that we can move into the new
balloons class, but let's stop here for now and focus on the new behavior.

(If you want to see this refactoring happening in smaller steps in real time,
check out the video version.)

## Stories

Here is some new behavior that we would like to have:

* balloons move downwards
* balloons appear at different x positions
* multiple balloons are in the air at the same time

If we move them into a test, it looks like this:

$:output:python:
class Balloons(SpriteGroup):

    """
    Balloons move downwards (move to Balloon?):

    ...

    It spawns up to 3 balloons:

    ...

    Balloons outside the screen is removed:

    ...
    """
$:END

<p>
<center>
![Multiple balloons.](multiple-balloons.png)
</center>
</p>

## Break and cleanup

    * then hammock + refactoring cleanup
        * cleaning up feels so good
        * get feature out fast, the cover up the imperfections

    commit 782cda7032896b15d89058b0fe3bc4ccbb54da8c
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 10:12:49 2023 +0200

        Replace (x, y) with position in Arrow and Balloon.

    ...

    commit 18a9a5af49966f9b2c7e8841495687181e7fedfb
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 14:19:21 2023 +0200

        Clean up shooting arrow tests.

## Summary

See you in the next episode!
