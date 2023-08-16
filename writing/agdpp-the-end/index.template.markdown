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

Recently though, I've experimented with a new format which I call
[DevLog](/tags/devlog/index.html). It is basically the same thing but a little
less polished. I write a DevLog while doing the development, so there is no
risk of falling behind. I write about what is going on in my mind as I do the
development.

In this post I will briefly mention the development that I've done but not
documented and then talk a little about future plans for this project.

## Particle effects

I polish the game a little by adding a particle effect system that I use to
render a splashing animation when a balloon is hit.

It looks a little something like this (although it is hard to show in a single
image):

<p>
<center>
![Particles when a balloon is hit.](particles.png)
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
randomized position, radius, and velocity.  The radius keeps decreasing as time
passes, and when it reaches a low enough value, the particle is removed.

The complete diff for this change can be seen on
[GitHub](https://github.com/rickardlindberg/agdpp/compare/b5261a939505c203cd1ffb21462a6772f0381faf...7533ec079dbdeba713526469535a1cc0fc915449).

## Sound effects

Me and my son record sound effects that are played when a balloon is hit.  We
go to the store, buy some balloons, rig the mic, and pop them.  It is much
fun.

The code for integrating the sound can be seen on
[GitHub](https://github.com/rickardlindberg/agdpp/compare/7533ec079dbdeba713526469535a1cc0fc915449...fcb1757f9b219be55d65d8588c259b96b9dc26ce).

This change include adding the `load_sound` method to `GameLoop`:

$:output:python:
class GameLoop(Observable):

    ...

    def load_sound(self, path):
        return Sound(self.pygame.mixer.Sound(path))
$:END

Does it really make sense that you load a sound from the game loop? I'm not
sure. The game loop is the only abstraction that we have for accessing pygame.
That's why it ended up there. But the design here feels a little off to me.
Something to keep in mind for the future. Next time we touch this area of the
code, we might feel the same thing again and have an idea about how to improve.

## Medals

When I ask my son what he wants the game to do next, he says that he wants to
get a medal for every 100 balloon that you shoot down.

I add a fun little particle effect again for the animation when you get a
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

Testing the medal particle effect is tedious. You have to shoot down 100
balloons, then you can see the effect for a split second, then you have to
shoot down 100 more.

When I have done that enough times, I come up with a better idea. And that is
to allow the game to be started in "test mode" where we can trigger the
animation with a press of a button.

I can do it like this:

$:output:text:
$ ./make.py rundev test-scene-score
$:END

Instead of starting the game, it starts a test scene:

$:output:python:
if __name__ == "__main__":
    if sys.argv[1:] == ["test-scene-score"]:
        scene = TestSceneScore()
    else:
        scene = None
    BalloonShooter.create(scene).run()
$:END

This test scene is only used for test purposes and looks like this:

$:output:python:
class TestSceneScore:

    def __init__(self):
        self.score = Score()

    def event(self, event):
        if event.is_user_closed_window():
            raise ExitGameLoop()
        elif event.is_keydown(KEY_SPACE):
            self.score.add_points(100)

    def update(self, dt):
        self.score.update(dt)

    def draw(self, loop):
        self.score.draw(loop)
$:END

It uses the score object (which is used in the real game) and adds 100 points
when we press the space key.

It looks like this:

<p>
<center>
![Test scene.](test-scene.png)
</center>
</p>

This way, I can quickly exercise the animation and validate that it looks good.

## Next: double dispatch, decentralized?

This project has been stalled for a few months. With these side projects, I follow
my interest. And my interest has lately been about writing my own [video
editor](/projects/rlvideo/index.html). And also, after the medal, my son said
that the game was finished. :)

However, right now, I have two ideas that I'm interested in doing. One is
trying a decentralized design that [Ron](https://www.ronjeffries.com/) has been
writing about and doing in his Asteroids Python series.

Another is to create a "game console pc" where I customize this game to first
show a start screen where the game to play can be selected. One game will be
the balloon shooter. Another will be [SuperTux](https://www.supertux.org/)
(which me and my son have played a lot).

## Summary

This post probably marks the end of this series as we know it.  When I continue
this project, it will be in the form of a [DevLog](/tags/devlog/index.html).
See you there!
