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

Next we take a small step towards having an arrow velocity. We change the
update method to this:

$:output:python:
class Arrow:

    ...

    def update(self, dt):
        if self.shooting:
            velocity = Point(x=0, y=-dt)
            self.position = self.position.add(velocity)
$:END

Our relatively new `Point` class attracts more and more behavior. Here we added
the `add` method:

$:output:python:
class Point:

    ...

    def add(self, point):
        """
        >>> Point(0, 5).add(Point(1, 1))
        Point(1, 6)
        """
        return self.move(dx=point.x, dy=point.y)
$:END

## Derive velocity from angle

Now we could modify the velocity of the arrow and the update method would move
it in the right direction.

However, I think it is better if we have a concept of an arrow angle that we
can adjust left and right. That would fit our use case better.

We add and angle attribute to the arrow and derive the velocity vector from it:

$:output:python:
class Arrow:

    def __init__(self, shooting=False, position=Point(x=500, y=500)):
        ...
        self.angle = -90

    def update(self, dt):
        if self.shooting:
            self.position = self.position.add(Point.from_angle(self.angle).times(dt))

    ...
$:END

The `Point` class again attracts functionality for converting angles to a unit
vector (length one) and for magnifying vectors:

$:output:python:
class Point:

    ...

    @staticmethod
    def from_angle(degrees):
        """
        >>> p = Point.from_angle(-90)
        >>> int(p.x)
        0
        >>> int(p.y)
        -1

        >>> p = Point.from_angle(0)
        >>> int(p.x)
        1
        >>> int(p.y)
        0
        """
        return Point(
            x=math.cos(math.radians(degrees)),
            y=math.sin(math.radians(degrees))
        )

    def times(self, magnification):
        """
        >>> Point(1, 5).times(2)
        Point(2, 10)
        """
        return Point(x=self.x*magnification, y=self.y*magnification)
$:END

## Base drawing on angle

The arrow now moves correctly based on the angle, but it doesn't draw its three
circles correctly. It looks like this now:

$:output:python:
class Arrow:

    ...

    def draw(self, loop):
        loop.draw_circle(self.position, color="blue", radius=10)
        loop.draw_circle(self.position.move(dy=20), color="blue", radius=15)
        loop.draw_circle(self.position.move(dy=40), color="blue", radius=20)
$:END

That is, it draws the second two circles by moving them downwards, assuming
that the arrow is pointing up.

Let's instead draw them offset by the opposite direction of what the arrow
points:

$:output:python:
class Arrow:

    ...

    def draw(self, loop):
        v = Point.from_angle(self.angle + 180)
        loop.draw_circle(self.position, color="blue", radius=10)
        loop.draw_circle(self.position.add(v.times(20)), color="blue", radius=15)
        loop.draw_circle(self.position.add(v.times(40)), color="blue", radius=20)
$:END

We get the reverse angle by turning it 180 degrees.

Perhaps angle is another case of primitive obsession. If we had an angle class,
we could have a `reverse` method that did this and we would no longer be
required to know about degrees (the angle could be implemented with radians
instead for example). We make a note about that.

Anyway, we can change the angle attribute of the arrow and it will fly in the
right direction and draw correctly. Now there is only one thing left: control
the angle with arrow keys.

## Changing angle with left/right

Here is the test we write for changing arrow angle:

$:output:python:
"""
>>> game = GameScene(space)
>>> game.get_arrow_angle()
-90
>>> game.event(GameLoop.create_event_keydown_left())
>>> game.get_arrow_angle()
-95
>>> game.event(GameLoop.create_event_keydown_right())
>>> game.get_arrow_angle()
-90
"""
$:END

For this to work we need to create new event wrappers for keydown left/right
and add a getter to expose the arrow angle. We have done that before. Same
procedure this time.

We make it pass by handling the events and changing the angle:

$:output:python:
class GameScene(SpriteGroup):

    ...

    def event(self, event):
        ...
        elif event.is_keydown_left():
            self.arrow.angle_left()
        elif event.is_keydown_right():
            self.arrow.angle_right()
$:END

$:output:python:
class Arrow:

    ...

    def angle_left(self):
        self.angle -= 5

    def angle_right(self):
        self.angle += 5
$:END

This almost works, but when we turn and arrow and shoot it, it still goes
straight up. We need to fix the `clone_shooting` method to also clone the
angle.

$:output:python:
class Arrow:

    ...

    def clone_shooting(self):
        """
        It preserves position and angle and set it to shooting:

        >>> arrow = Arrow(position=Point(x=5, y=5), angle=-45)
        >>> new_arrow = arrow.clone_shooting()
        >>> new_arrow.get_position()
        (5, 5)
        >>> new_arrow.angle
        -45
        >>> new_arrow.shooting
        True
        """
        return Arrow(shooting=True, position=self.position, angle=self.angle)
$:END

## Result

Now we can turn the arrow with left/right keys and shoot it in different
directions. It looks like this:

<center>
![Turning arrow.](turning-arrow.png)
</center>

If you want to try it out, the full source code from this episode on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/turning-arrow).

## Summary

Testing continues to go smooth with state based testing and getters to expose
internal state.

What I like to do after implementing a feature it to take a break and then come
back later to review to code for possible improvements. Often times it is small
things like renaming a variable to make it more clear. In this episode we also
noted that angle might benefit being wrapped in an abstraction.

See you in the next episode!
