---
title: Demo and game idea
date: 2023-04-24
tags: agdpp
agdpp: true
---

In this episode we demo the "game" for our customer. We get some feedback on it
and start evolving the game to meet our customer's vision.

## Video version

The video version of this episode:

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/z20IocFrKLY" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
</center>

## Demo

I mentioned to my son that I had started working on a game. He wanted to see
it. I told him that there isn't much to play yet, but that I can show him what
it looks like.

I started the "game" and he watched the circle going back and forth.

For reference, this is what the "game" looks like now:

<center>
![Animated circle.](animation.png)
</center>

Then he tried to interact with the circle. He grabbed the mouse and clicked
away and was disappointed that nothing happened. I told him that you can't do
anything yet, that it's just an animation.

He looked at me sad and said "Dad, this game is boring."

Initially I wasn't planning on showing him the game because in this early stage
there is not much to play, and I knew he was not going to like it. Did I ruin
it?

Then I asked him, "What would you like to do with the circle?"

The sadness in his face disappeared and he started talking about balloons. He
said that he wanted the circle to be a balloon and that he wanted to shoot down
balloons with arrows.

I think we have an idea for a game.

## On early feedback

My initial idea for the game was some kind of tetris variant. I was thinking
that you have two boards and that you could help each other out by clearing
blocks on each other's boards. Cooperation seems more fun that competition.

Turns out, the customer wants a balloon shooter. If we hadn't shown the demo,
we might have never found that out.

## Simplest possible version

In the spirit of
[Ward](https://www.artima.com/articles/the-simplest-thing-that-could-possibly-work#part3)
we ask ourselves what the simplest possible balloon shooter could look like.
What is the absolute minimum version that I can give to my son and he can
somewhat enjoy playing or at least recognize as a balloon shooter? (The goal is
to create a game that *we* can enjoy playing together. That means some kind of
multiplayer mode. But that is another for a later story.)

Here is what I'm thinking:

* 1 balloon falling down the screen
* 1 arrow pointing in a fixed direction
* 1 button to shoot that single arrow
* Then game over

So the only challenge will be to fire the arrow at the right time for it to hit
the balloon. You will either hit and get a point or miss and it'll be game
over.

Oh, and circles for the graphics is probably fine. The balloon can be drawn
with one big circle, and the arrow maybe with three smaller circles in a row.
If I tell my son to imagine they are balloons and arrows, I think he'll accept
that.

## First story

To make progress towards a balloon shooter, I want to work on a story that is
about drawing a balloon and an arrow. Create the initial scene of the game sort
of.

Our game already animates a circle. Let's put all behavior related to that
circle into its own class called `Balloon`. Here is how our game `tick` method
looks like now:

$:output:python:
def tick(self, dt, events):
    ...
    if self.x > 500:
        self.x = 50
    else:
        self.x += dt
    self.loop.clear_screen()
    self.loop.draw_circle(self.x)
$:END

After we extract the balloon class, it looks like this:

$:output:python:
def tick(self, dt, events):
    ...
    self.balloon.tick(dt)
    self.loop.clear_screen()
    self.balloon.draw(self.loop)
$:END

And here is the balloon class:

$:output:python:
class Balloon:

    def __init__(self):
        self.x = 50

    def tick(self, dt):
        if self.x > 500:
            self.x = 50
        else:
            self.x += dt

    def draw(self, loop):
        loop.draw_circle(self.x)
$:END

Any behavior that the balloon should have, we can now test at this lower level.
We can instantiate a balloon, call its tick method, and observe that the right
thing happens. There is no need to involve the game or the game loop. (At least
if we accept exposing somewhat internal state like the x variable.)

With the balloon object in place, it is natural to create a new object called
`Arrow` for our other piece in the balloon shooter game. We create a version
that just draws a circle:

$:output:python:
class Arrow:

    def tick(self, dt):
        pass

    def draw(self, loop):
        loop.draw_circle(10)
$:END

We make sure it is included in the game by modifying the tick method of the
game to also tick and draw the arrow:

$:output:python:
def tick(self, dt, events):
    ...
    self.balloon.tick(dt)
    self.arrow.tick(dt)
    self.loop.clear_screen()
    self.balloon.draw(self.loop)
    self.tick.draw(self.loop)
$:END

We notice a pattern here. It seems like the responsibility of the game is to
call tick and draw on a set of objects. In games (or in Pygame) those objects
are referred to as sprites. My understanding is that a sprite is any visual
element that shows up in the game.

We refactor our game to reflect this new understanding:

$:output:python:
class Game:

    def __init__(self, loop):
        ...
        self.balloon = Balloon()
        self.arrow = Arrow()
        self.sprites = [self.balloon, self.arrow]

    def tick(self, dt, events):
        ...
        for sprite in self.sprites:
            sprite.tick(dt)
        self.loop.clear_screen()
        for sprite in self.sprites:
            sprite.draw(self.loop)

    ...
$:END

Is our game class just becoming a thing layer of loops? Can we move some of
that responsibility to the game loop? I'm not certain yet, so we will leave it
like this for now.

If we run the game now, this is what we see:

<center>
![First scene of balloon shooter.](scene1.png)
</center>

Not very pretty. What is that?

Let's see if we can improve the drawing of the arrow. But remember, not
perfection, but improvement. If I look at the game now, I don't get an idea
what the game is about. We just want to make the arrow slightly more realistic
to convey the meaning of the object. We are still restricted to drawing
circles. The current `draw_circle` looks like this:

$:output:python:
def draw_circle(self, x):
    self.notify("DRAW_CIRCLE", {"x": x})
    self.pygame.draw.circle(self.screen, "red", (x, 50), 40)
$:END

That's stupid! Why is there no ability to specify anything but the
x-coordinate? Well, until now, we haven't needed that. Now that we do need it,
let's add it. No biggie:

$:output:python:
def draw_circle(self, x, y=50, radius=40, color="red"):
    self.notify("DRAW_CIRCLE", {"x": x, "y": y, "radius": radius, "color": color})
    self.pygame.draw.circle(self.screen, color, (x, y), radius)
$:END

We experiment with three circles for the arrow and tweak the numbers until we
think it looks good. Here is the result:

<center>
![Improved drawing of arrow.](scene2.png)
</center>

I don't know about you, but when I see this, I want to press a button to fire
that arrow so it hits the balloon. I'm convinced this is a balloon shooter now.
I hope our customer is as well.

Right, balloon shooter. Before we didn't know what game we should write, so our
game class was just called `Game`. Let's fix that:

$:output:diff:
 from gameloop import GameLoop
 
-class Game:
+class BalloonShooter:
$:END

There! Now the code more accurately represent the ideas that we have in our
minds about this game.

I forget to mention anything about testing. (For the complete workflow, check
out the video.) But here is what the test for the balloon shooter looks like
now:

$:output:python:
"""
I draw the initial scene of the game which consists of a balloon and an
arrow and quit when the user closes the window.

>>> loop = GameLoop.create_null(
...     events=[
...         [],
...         [GameLoop.create_event_user_closed_window()],
...     ]
... )
>>> events = loop.track_events()
>>> BalloonShooter(loop).run()
>>> events
GAMELOOP_INIT =>
    resolution: (1280, 720)
    fps: 60
CLEAR_SCREEN =>
DRAW_CIRCLE =>
    x: 50
    y: 50
    radius: 40
    color: 'red'
DRAW_CIRCLE =>
    x: 500
    y: 500
    radius: 10
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 520
    radius: 15
    color: 'blue'
DRAW_CIRCLE =>
    x: 500
    y: 540
    radius: 20
    color: 'blue'
GAMELOOP_QUIT =>
"""
$:END

We both changed the description, becasue we have a balloon shooter now, not a
generic game, and added checks for it drawing both the balloon and the arrow.

It might be too specific to assert on exact coordinates here. We have to
visually inspect the output and see if we think it looks good. That's what I
did here. And when I was happy, I copied the coordinates into the test.

## Summary

We have completed a first vertical slice of our balloon shooter. There is now a
balloon and an arrow on the screen. This is new behavior that we could show to
our customer and get feedback on. Perhaps he doesn't like the position of the
arrow. If so, we can adjust. Perhaps he can't imagine that the circle is
actually a balloon. If so, we can work on improving the graphics.

What else do we think we need before we have the first version of the balloon
shooter?

* Balloon moves downwards
* Arrow animates when shot
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* Real graphics instead of circles

Which one is the most important one to work on next? We'll tackle that one in
the next episode!

You can browse the [complete source
code](https://github.com/rickardlindberg/agdpp/tree/initial-balloon-shooter-story) from
this episode.

See you!
