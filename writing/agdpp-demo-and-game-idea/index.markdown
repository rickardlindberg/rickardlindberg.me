---
title: 'DRAFT: Demo and game idea'
date: 2023-04-22
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we demo the "game" for our customer. We get some feedback on it
and start evolving the game to meet our customer's vision.

## Demo

I mentioned to my son that I had started working on a game. He wanted to see
it. I told him that there wasn't much to play yet, but I could show him what it
looks like.

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

The sadness in his face faded and he started talking about balloons. He said
that he wanted the circle to be a balloon and that he wanted to shoot down
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
somewhat enjoy playing or at least recognize as a balloon shooter?

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

1. Extract `Balloon`
2. Add `Arrow`
3. Refactor towards "sprite architecture"
    * reflect on it
4. Improve drawing
5. Rename to `BalloonShooter`.

## Summary

We have completed a first vertical slice of our balloon shooter. There is now a
balloon and an arrow on the screen. This is new behavior that we could show to
our customer and get feedback on. Perhaps he doesn't like the position of the
arrow. If so, we can adjust. Perhaps he can't imagine that the circles are
actually an arrow. If so, we can work on improving the graphics.

What else do we think we need before we have the first version of the balloon
shooter?

* Balloon moves downwards
* Balloon respawns when reaching bottom
* Arrow animates when shot
* New arrow appears after shot
* Arrow can burst balloon
* Point is given for hit
* Real graphics instead of circles

Which one is the most important one to work on next? We'll tackle that one in
the next episode!
