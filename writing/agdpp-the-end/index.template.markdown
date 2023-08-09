---
title: The end?
date: 2023-08-08
tags: agdpp,draft
agdpp: true
---

When I started this series, my intention was to document my journey of creating
a game using agile methods.

I think I have mostly succeeded in this regard, but at the moment I've done
some development that I have not documented. Furthermore, I did that
development many months ago, so documenting it gets harder and harder because I
forget what I was thinking when I did the development.

Recently though, I've experimented with a new format which I call DevLogs. It
is basically the same thing but a little less polished. I write a DevLog while
doing the development, so there is no risk of falling behind.

In this post I will briefly mention the development that I've done but not
documented and talk a little about future plans.

## Particle effects

I polish the game a little by adding a particle effect system that I use to
render a splashing animation when a balloon is shot.

It looks a little something like this (although it is hard to show in a single
image):

<p>
<center>
![Particles when shooting a balloon.](particles.png)
</center>
</p>

The most interesting piece of code is this:

$:output:python:
class Balloon:

    ...

    def get_hit_particles(self):
        number_of_particles = random.randint(4, 8)
        return [
            BalloonParticle(
                position=self.position.move(
                    dx=random.randint(0, self.radius),
                    dy=random.randint(0, self.radius)
                ),
                radius=self.radius*(random.randint(30, 70)/100),
                velocity=Angle.fraction_of_whole(random.random()).to_unit_point().times(self.speed*2)
            )
            for x
            in range(number_of_particles)
        ]
$:END

It generates a list of particles when a balloon is hit. The particles have a
little randomized position, radius, and velocity.

The radius keeps decreasing as time passes, and when it reaches a low value,
the particle is removed.

The complete diff for this change can be seen on
[GitHub](https://github.com/rickardlindberg/agdpp/compare/b5261a939505c203cd1ffb21462a6772f0381faf...7533ec079dbdeba713526469535a1cc0fc915449).

## Sound!

Me and my son recorded sound effects that are played when shooting a balloon.
We went to the store, bought some balloons, rigged the mic, and popped them.

It was much fun.

The code for integrating the sound is here:

[GitHub](https://github.com/rickardlindberg/agdpp/compare/7533ec079dbdeba713526469535a1cc0fc915449...fcb1757f9b219be55d65d8588c259b96b9dc26ce).

## Medals

When I asked my son what he wanted to work on next, he said he wanted to get a
medal for every 100 balloon that you shoot down.

I added a fun little particle effect again for the animation when you get a
medal:

<p>
<center>
![Medal animation.](medal1.png)
</center>
</p>

The medals stack up in the upper left corner like this:

<p>
<center>
![Medals stacking up.](medal2.png)
</center>
</p>

[GitHub](https://github.com/rickardlindberg/agdpp/compare/fcb1757f9b219be55d65d8588c259b96b9dc26ce...0c8e713a6d938898ddb92164cc86dcb1db19aa0c).

## Test scene

<p>
<center>
![Test scene.](test-scene.png)
</center>
</p>

## Next: double dispatch, decentralized?

## Summary

This post probably marks the end of this series as we know it.  When I continue
this project, it will be in the form of a [DevLog](/tags/devlog/index.html).
See you there!
