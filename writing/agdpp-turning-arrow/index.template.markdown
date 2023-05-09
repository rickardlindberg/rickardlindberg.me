---
title: Turning arrow
date: 2023-05-09
tags: agdpp,draft
agdpp: true
---

We have a basic version of a balloon shooter in place. We have a balloon moving
across the screen and an arrow that can be shot and hit the balloon to score a
point.

<center>
![Arrow shooting straight.](points.png)
</center>

When I play the game now, I want to turn the arrow. I think that will make the
game a little more fun. Then you have to control both angle and timing instead
of just timing to hit a balloon. (If we implement that balloons fall downwards
instead, turning will also be necessary to hit balloons that are not straight
above the arrow.)

## How does arrow movement work?

Arrows move first when we press the space key to shoot. Then we create a new
arrow and set its shooting attribute to true:

$:output:python:
class GameScene(SpriteGroup):

    ...

    def event(self, event):
        ...
        elif event.is_keydown_space():
            self.flying_arrows.add(Arrow(shooting=True))
$:END

An arrow that has the shooting attribute set to true moves like this:

$:output:python:
class Arrow:

    ...

    def update(self, dt):
        if self.shooting:
            self.position = self.position.move(dy=-dt)
$:END

That is, it moves straight up in increments of `dt`. There is no concept of
direction yet.

What we would like to have is some kind of direction attribute on the arrow
that we can change. When we shoot, the arrow that we create should get that
direction attribute so that it flies in the same direction that we aimed.

Let's see if we can refactor towards that.

## Clone shooting

We start by creating a method `clone_shooting` on the arrow that should return
a copy of itself (including all attributes) and have the shooting attribute set
to true:

$:output:python:
class Arrow:

    ...

    def clone_shooting(self):
        return Arrow(shooting=True, position=self.position)
$:END

We modify how a flying arrow is added like this:

$:output:diff:
- self.flying_arrows.add(Arrow(shooting=True))
+ self.flying_arrows.add(self.arrow.clone_shooting())
$:END

One change here is that we also clone the arrows position attribute. The
position of the arrow is always the same. Only when we shoot it it changes. But
should we choose to move the arrow to a different start position, the code now takes
that into account and places shooting arrows at the right start positions.

I think this is still a pure refactoring.  There is no change in visible
behavior, but the code is more robust because we can now change the start
position of the arrow, and it will shoot from the right position without we
having to modify any other piece of code. The design is better.

## Work towards arrow velocity

## Derive velocity from angle

## Base drawing on angle

## Changing angle with left/right

## Result

<center>
![Turning arrow.](turning-arrow.png)
</center>

## Summary

Full source code from this episode on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/turning-arrow).

See you in the next episode!

# TODO

$:output:python:
$:END

    commit b08065975c2233f27916a049c75529d1be3295c5
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Thu Apr 27 07:03:44 2023 +0200

        Add Arrow.clone_shooting.

    ...

    commit e2f869113c37b91086888d262c22dd4376d47395
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Fri Apr 28 06:51:48 2023 +0200

        Arrow angle can be changed with keys.
