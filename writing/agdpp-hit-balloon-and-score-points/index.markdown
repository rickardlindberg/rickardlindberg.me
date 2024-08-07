---
title: Hit balloon and score points
date: 2023-05-09
tags: agdpp
agdpp: true
---

We have two stories left before we think we have a first, minimal version of a
balloon shooter game:

* Arrow can hit balloon
* Point is given for hit

In this episode we will work on both of them. We will start with collision
detection between arrow and balloon.

## Clarify behavior with test

To clarify what we mean by arrow can hit balloon, we want to write a test first
that shows the lacking behavior. Then implement the thing. We use the test
both as a design tool to figure out what we are actually going to implement and
as a testing tool to verify that behavior.

Our game scene object currently inits like this:

```python
class GameScene(SpriteGroup):

    def __init__(self, space):
        SpriteGroup.__init__(self)
        self.balloon = self.add(Balloon())
        self.arrow = self.add(Arrow())
        self.flying_arrows = self.add(SpriteGroup())
        self.space = space

    ...
```

We would like to write a test where we shoot an arrow, make it collide with the
balloon, and then assert that the balloon disappears.

With the current design, this is really difficult to do. It can only be done
something like this:

```python
"""
>>> game = GameScene(...)
>>> game.event(GameLoop.create_event_keydown_space())
>>> game.update(??)
>>> game.get_balloon() is None
True
"""
```

But this is really flaky and hard to understand. In order for this to work, we
have to time the shooting and the updating so that the arrow actually hits the
balloon. Even if we get it to work, it will start failing if we for example
change the speed of the arrow. And this test should really not care about arrow
speed.

Let's see if we can do better.

We change the init method to this instead:

```python
class GameScene(SpriteGroup):

    def __init__(self, space, balloons=[(50, 50)], arrows=[]):
        SpriteGroup.__init__(self)
        self.balloons = self.add(SpriteGroup([
            Balloon(x=x, y=y) for (x, y) in balloons
        ]))
        self.arrow = self.add(Arrow())
        self.flying_arrows = self.add(SpriteGroup([
            Arrow(x=x, y=y) for (x, y) in arrows
        ]))
        self.space = space

    ...
```

That is, we make it possible to create a game scene object where we specify
where all the balloons should be and where all the flying arrows should be. We
also change the balloon from a single object to a sprite group. This is not
strictly necessary, but it will make removing hit balloons easier. The default
values for the balloons and arrows mimics the current default. We have one
balloon that starts at (50, 50) and zero flying arrows.

Here is the test that checks initial state:

```python
"""
>>> game = GameScene(space, balloons=[(100, 100)], arrows=[(500, 500)])
>>> len(game.get_balloons())
1
>>> len(game.get_flying_arrows())
1
"""
```

In order for it to work, we expose another getter for the balloon sprites:

```python
def get_balloons(self):
    return self.balloons.get_sprites()
```

The test continues to check that we still have one balloon and one flying arrow
after an update:

```python
"""
>>> game.update(0)
>>> len(game.get_balloons())
1
>>> len(game.get_flying_arrows())
1
"""
```

We update with 0 to ensure that nothing moves. We need to call update to make
the collision detection code run, but to ensure exact positions, we pass 0 as
the delta time. All movements should take the delta time into account, so 0
should result in no movement.

We continue and write the test for hitting a balloon like this:

```python
"""
>>> game = GameScene(space, balloons=[(500, 500)], arrows=[(500, 500)])
>>> len(game.get_balloons())
1
>>> game.update(0)
>>> game.get_balloons()
[]
"""
```

We place the arrow at the center of the balloon, invoke the collision detection
code with update, and assert that there are no longer any balloons.

## Implement arrow hit

In the game update, we already loop over the arrows to remove the ones that are
outside the screen. We add a loop that checks if any arrow hits any of the
balloons. If so, we remove that balloon:

```python
class GameScene(SpriteGroup):

    def update(self, dt):
        ...
        for arrow in self.flying_arrows.get_sprites():
            ...
            for balloon in self.balloons.get_sprites():
                if arrow.hits_baloon(balloon):
                    self.balloons.remove(balloon)

    ...
```

We add `hits_baloon` to arrow:

```python
class Arrow:

    def hits_baloon(self, balloon):
        return balloon.inside(self.x, self.y)

    ...
```

And implement `inside` in balloon like this:

```python
class Balloon:

    def inside(self, x, y):
        return (x-self.x)**2+(y-self.y)**2 <= self.radius**2

    ...
```

This is a bit of a trick in OOP that I learned some time ago that I'm not sure
what I think about. Let me explain.

We could have written the test like this instead:

```python
if balloon.inside(arrow.x, arrow.y):
    self.balloons.remove(balloon)
```

But then the game scene object would have to reach into the arrow object to
access the x and y coordinates.

With `hits_baloon` we introduce one more step in the chain where the arrow
itself pass its coordinates along to `inside`. No need to expose them to the
outside.

I like this because objects can expose less details about themselves. I dislike
this because I think the code sometimes becomes a little harder to read. I
guess the solution is good naming. And I think `arrow.hits_baloon(balloon)`
reads pretty well.

## Demo trick

The game works and if we manage to hit a balloon, it disappears. Again, bummer.
We can shoot infinitely many arrows, but if there are no more balloons to hit,
the game is not that interesting.

We had a situation like this [before](/writing/agdpp-shooting-arrow/index.html)
where you shot the arrow and you could only get a new one by restarting the
game.

One trick I used when I demoed this for the customer was to run the game in a
loop like this:

```bash
$ while true; do ./zero.py rundev; done
```

So when you have no more arrows to shoot or no more balloons to hit, you close
the game window and a new one will immediately pop up.

That way, it is a little smoother to gather feedback on the current game
functionality.

We fixed so that you get more arrows to shoot before. Let's also fix so that a
new balloon is spawned after one is hit so we don't need to restart the game in
a loop anymore.

## Primitive obsession refactoring

Before we start adding new functionality, let's have a look at the code and see
if there is anything that we can improve to make it more clear and make the
future a little smoother.

One thing that I notice is that we are passing around (x, y) coordinates in a
lot of places, and objects keep track of the x and y coordinates. Here is the
balloon class for example:

```python
class Balloon:

    def __init__(self, x, y, radius=40):
        self.x = x
        self.y = y
        self.radius = radius

    def inside(self, x, y):
        return (x-self.x)**2+(y-self.y)**2 <= self.radius**2

    ...
```

This smell is called primitive obsession. It is when you pass around primitive
objects (integers, strings encoding information, etc) instead of an
abstraction. That leads to duplicated logic. Say for example that we want to
move an object, we might do something like this:

```python
self.x += 1
self.y += 2
```

And we probably need to move multiple objects, so this kind of code will be
duplicated in many places.

The solution is to create and abstraction for the concept. In this case, I
choose to call it point:

```python
class Point:

    def __init__(self, x, y):
        self.x = x
        self.y = y
```

We refactor in small, tiny steps to make use of this point.

Eventually, the inside check in the balloon looks like this:

```python
class Balloon:

    def inside(self, position):
        return self.position.distance_to(position) <= self.radius

    ...
```

We are no longer dealing with separate x and y coordinates. We are dealing with
positions.

A big chunk of the hit test has also moved into the new point class:

```python
class Point:

    def distance_to(self, point):
        return math.sqrt((point.x-self.x)**2+(point.y-self.y)**2)

    ...
```

If we are concerned about the performance of the square root, we could write
`inside` like this (equivalent to what we had before):

```python
class Balloon:

    def inside(self, position):
        return self.position.distance_squared_to(position) <= self.radius**2

    ...
```

I think this reads a little worse, and we don't have performance issues yet.

What usually happens when you extract a concept like this point is that it
starts attracting new functionality. Suddenly, there is a logical place to
implement something instead of spreading it across the code base.

Another benefit of this abstraction is that we can now more easily test the
behavior of `distance_to` in isolation. No need to involve a balloon.

## Spawn new balloons

So it's no fun to play the game after you hit the balloon, because then there
are no more balloons to hit. We want to spawn new balloons.

We need to modify our test. It looks like this now:

```python
"""
>>> game = GameScene(space, balloons=[(500, 500)], arrows=[(500, 500)])
>>> len(game.get_balloons())
1
>>> game.update(0)
>>> game.get_balloons()
[]
"""
```

We don't want the balloon list to be empty. We still want it to contain a
balloon. But not the balloon that we just shot down, but another one.

I think we can do it like this:

```python
"""
>>> game = GameScene(space, balloons=[(500, 500)], arrows=[(500, 500)])
>>> balloons = game.get_balloons()
>>> len(balloons)
1
>>> game.update(0)
>>> new_balloons = game.get_balloons()
>>> len(new_balloons)
1
>>> new_balloons == balloons
False
"""
```

We can make the test pass by adding another balloon after the one that has been
shot down has been removed:

```python
class GameScene(SpriteGroup):

    def update(self, dt):
        ...
        for arrow in self.flying_arrows.get_sprites():
            ...
            for balloon in self.balloons.get_sprites():
                if arrow.hits_baloon(balloon):
                    self.balloons.remove(balloon)
                    self.balloons.add(Balloon(position=Point(x=50, y=50)))

    ...
```

This now works, but it is a little hard to actually notice that we hit a
balloon. It should be more clear if we include a score.

## Add score

We have a place in the code where we have hit a balloon. When that happens we
would also like to increase a score. What is the simplest implementation of
that?

What if we just maintain a list of sprites where each sprites represents a
point? Let's see.

```python
class GameScene(SpriteGroup):

    ...

    def __init__(self, space, balloons=[(50, 50)], arrows=[]):
        ...
        self.points = self.add(SpriteGroup())

    def update(self, dt):
        ...
        for arrow in self.flying_arrows.get_sprites():
            ...
            for balloon in self.balloons.get_sprites():
                if arrow.hits_baloon(balloon):
                    ...
                    self.points.add(PointMarker(position=Point(x=700, y=50+len(self.points.get_sprites())*10)))
```

We use the length of the point sprites to calculate the position of the next
point marker.

We also add a getter for the points so that we can test this behavior:

```python
class GameScene(SpriteGroup):

    ...

    def get_points(self):
        return self.points.get_sprites()
```

And here is the `PointMarker` that draws a circle at the given position:

```python
class PointMarker:

    def __init__(self, position):
        self.position = position

    def update(self, dt):
        pass

    def draw(self, loop):
        loop.draw_circle(position=self.position, radius=5, color="yellow")
```

This is what it looks like after a few balloons have been hit:

<center>
![Point markers.](points.png)
</center>

When I showed this to my son, he thought it was a little fun when point markers
appeared on the screen. He also wanted to make the point markers go all across
the screen, and also wanted me to count how many points we had about half way
through. I don't like counting small yellow circles, so we probably need a
better solution for displaying points. We make a note about that.

If you want to try this version or look at the complete source code from this
episode, it is on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/hit-balloon-and-score-points).

## Summary

The state based testing approach continues to work well. Tests are easy to
write, and I don't think the getters that we add to expose internal state are
too problematic.

We now have a first version of a balloon shooter game. Now we have to show it
to our customers, have them play it, gather feedback, and keep improving. One
story at a time.

See you in the next episode!
