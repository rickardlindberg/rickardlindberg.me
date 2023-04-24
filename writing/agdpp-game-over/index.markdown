---
title: 'DRAFT: Game over?'
date: 2023-04-24
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

When we worked on [shooting the
arrow](/writing/agdpp-shooting-arrow/index.html) we concluded that it was
tedious to restart the game after each shot. When the arrow goes outside the
screen, we want the game to be over instead and the arrow reset. Let's work on
that.

## Do we really need game over?

If we implement game over now, there will be game over after every shot.
Because there is no way to hit the balloon just yet.

If you play a game where it is game over immediately, would you enjoy it?

Game over is not really the problem. It is a solution to the problem that you
don't have any arrows to shoot after the first one.

How about if you get a new arrow immediately? So you can just keep firing?

From before, these are the stories we though about as needed for an initial
balloon shooter:

* Balloon moves downwards
* Arrow can hit balloon
* Point is given for hit
* Game over when miss
* Real graphics instead of circles

Let's think about this. For minimal, I don't think we need real graphics. The
circles convey the idea just fine.

I'm not sure the balloon needs to move downwards either. The current movement
pattern is fine.

And we decided to do something else instead of game over. That leaves us with
this:

* Arrow can hit balloon
* Point is given for hit
* New arrow when the current one has been shot

We can always make something smaller.

Let's work on spawning arrows now so that we can enjoy shooting arrows for a
longer time without having to restart our game.

## Acceptance

I can think of two test:

* When an arrow goes outside the screen, we stop rendering it
* You get a new arrow when you shoot the current one

The first test is kind of internal. If we render thousands of arrows outside
the visible screen, no one will notice. Until there is a performance issue or
an out of memory crash or something like that.

On the other hand, it makes sense, from a gameplay perspective, to talk about
arrows going off the screen as being deactivated.

## How to write the test?

## TODO

* God, testing is so fucking hard!
* I tried sprite group refactoring.
* I know exactly how to implement this. It's just one line of code. Why can't I
  just do it?

* Breakthrough: getters to expose state! State based testing!

## Summary

See you in the next episode!
