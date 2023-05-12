---
title: A case for the infrastructure wrapper
date: 2023-05-12
tags: agdpp,draft
agdpp: true
---

I noticed something strange when playing the game.

When shooting arrows out the left side of the screen, a blue horizontal line is
drawn. It looks something like this:

<center>
![Bug with drawing circles on negative x values.](negative-x-draw-bug.png)
</center>

It only shows for a split second and then disappears. What's going on?

## Troubleshooting

So it appears that this blue horizontal line only appears when we shoot arrows
to the left--not when we shoot up or to the right.

I base this guess on shooting wildly in different directions.

First I think that that there is some kind of graphic glitch because it is only
drawn for a split second. But I'm able to consistently reproduce it.

I modify the code to draw an arrow just outside the screen to the left, and
indeed the blue horizontal line stays on the screen forever. (That's how I
manage to get this screenshot. It was not timing.)

Can't be a glitch then.

I decided to ask DuckDuckGo.

It gave me this: [Circles drawn using pygame.draw.circle with negative x
positions are drawn as a horizontal line across the whole screen.](https://github.com/pygame/pygame/issues/3778)

Ha! A bug in Pygame. I was a bit worried for a while that we did something
completely forbidden.

So any place in the code where we draw a circle we have to modify it to handle
negative x values.

## Infrastructure to the rescue

We have used the [infrastructure wrapper]() pattern in the codebase for our
game. That means that every time our code wants to interact with the outside
world, it does so via an infrastructure wrapper.

Anytime we want to draw something on the screen, we do it the game loop
infrastructure wrapper. Here is how the arrow draws itself:

$:output:python:
class Arrow:

    ...

    def draw(self, loop):
        v = Point.from_angle(self.angle + 180)
        loop.draw_circle(self.position, color="blue", radius=10)
        loop.draw_circle(self.position.add(v.times(20)), color="blue", radius=15)
        loop.draw_circle(self.position.add(v.times(40)), color="blue", radius=20)
$:END

`draw_circle` above is part of the infrastructure wrapper. In code that we
control. It in turn makes calls to Pygame like this:

$:output:python:
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
$:END

So we are actually only calling Pygame's `draw_circle` in one place in our
code.

We fix it like this:

$:output:python:
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
$:END

That means that circles partially outside the screen will not be drawn at all.
Not ideal. But I very much prefer that to an annoying blue horizontal line.

And when I play the game, I don't notice circles of the arrow disappearing a
little to early. They move so fast anyway.

## Summary

We were able to fix an annoying graphic bug by adding a single if-statement to
our infrastructure wrapper.

Wrapping third party libraries might seem like unnecessary overhead sometimes,
but the benefit shown in this episode make me think that I should do it more
often.

See you in the next episode!
