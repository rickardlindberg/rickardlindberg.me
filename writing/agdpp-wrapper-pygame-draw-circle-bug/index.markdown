---
title: A case for the infrastructure wrapper
date: 2023-05-14
tags: agdpp
agdpp: true
---

I noticed something strange when playing the game.

When shooting arrows out the left side of the screen, a blue horizontal line is
drawn. It looks something like this:

<center>
![Bug with drawing circles on negative x positions.](negative-x-draw-bug.png)
</center>

But it only shows for a split second and then disappears. What's going on?

## Troubleshooting

We troubleshoot by playing the game and shooting wildly in different
directions.

It seems like the blue horizontal line only appears when we shoot arrows
to the left--not when we shoot up or to the right.

So why does it only show for a split second? Most likely because arrows
outside the screen are removed. But they are only removed if they are far
enough outside the screen. The idea is that arrows are only removed if they are
completely outside the screen. However, I don't think it's working quite like
that at the moment. But arrows can be partially outside the screen before being
removed.

We modify the code to draw a static arrow just outside the screen to the left,
and indeed the blue horizontal line stays on the screen forever. (That's how I
managed to get the screenshot. It was not timing.)

The problem can now be consistently reproduced. Good!

## Bug in Pygame?

At first I thought we might use Pygame wrong in some way. But now I'm starting
to think that there might actually be an issue with Pygame.

Let's ask DuckDuckGo.

It came back with this: [Circles drawn using pygame.draw.circle with negative x
positions are drawn as a horizontal line across the whole screen.](https://github.com/pygame/pygame/issues/3778)

Ha! A bug in Pygame. It all makes sense now. And we are not at fault.

However, we still have an ugly, annoying graphics artifact in our game that we
want to get rid of. How?

## Infrastructure to the rescue

Any place in the code where we draw a circle we have to modify it to handle
negative x values.

Let's see how.

We have used the [infrastructure
wrapper](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#infrastructure-wrappers)
pattern in our game. That means that every time our code interacts with
the outside world, it does so via an infrastructure wrapper.

Anytime we draw something on the screen, we do it via the game loop
infrastructure wrapper. Here is how the arrow draws itself:

```python
class Arrow:

    ...

    def draw(self, loop):
        v = Point.from_angle(self.angle + 180)
        loop.draw_circle(self.position, color="blue", radius=10)
        loop.draw_circle(self.position.add(v.times(20)), color="blue", radius=15)
        loop.draw_circle(self.position.add(v.times(40)), color="blue", radius=20)
```

`draw_circle` above is part of the infrastructure wrapper. In code that we
control. It in turn makes calls to Pygame like this:

```python
class GameLoop(Observable):

    ...

    def draw_circle(self, position, radius=40, color="red"):
        ...
        self.pygame.draw.circle(
            self.screen,
            color,
            (int(position.x), int(position.y)),
            radius
        )
```

So we are actually only calling Pygame's `draw_circle` in one place in our
code.

We patch it like this:

```python
class GameLoop(Observable):

    ...

    def draw_circle(self, position, radius=40, color="red"):
        ...
        if position.x >= 0:
            # https://github.com/pygame/pygame/issues/3778
            self.pygame.draw.circle(
                self.screen,
                color,
                (int(position.x), int(position.y)),
                radius
            )
```

This means that circles drawn partially outside the screen to the left will not
be drawn at all.  Not ideal. But I very much prefer that to an annoying blue
horizontal line.

And when I play the game, I don't notice circles of the arrow disappearing a
little to early. They move so fast anyway.

We could do something more fancy like checking for specific versions of Pygame
where we know this bug exists. But this will do for now.

## Summary

We were able to fix an annoying graphics artifact by adding a single
if-statement to our infrastructure wrapper.

Wrapping third party libraries might seem like unnecessary overhead sometimes,
but the benefit shown in this episode makes me think that we should do it more
often.

See you in the next episode!
