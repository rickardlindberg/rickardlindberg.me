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
think we can work on.

## Video version

The video version of this episode:

    TODO

## Code review

Let's review our code and look at how balloons are managed.

Our game scene has a sprite group for balloons which by default only contains
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

## Stories

...

## Summary

See you in the next episode!

## TODO

$:output:python:
$:END

<p>
<center>
![Timeline halt GUI.](timeline-halt-gui.png)
</center>
</p>

    * refactor to make the change easy

    commit b0a34cb7e542aeff0a49a146576986390f1d7d1f
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 08:41:34 2023 +0200

        Extract Balloons.

    ...

    commit c6f3debba566e6cf1f69d300077d8a16483efc82
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Sun May 7 09:24:55 2023 +0200

        Spawn multiple balloons and remove the ones outside the screen.

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

