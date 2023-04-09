---
title: Agile Game Development with Python and Pygame: The Game Loop
date: 2023-04-05
tags: agdpp,draft
---

In this episode we will look at how to set up the game loop, draw something on
the screen, and setup tests for it. We begin with a spike to learn Pygame
fundamentals and then we look at how to set it up properly with tests.

## Hello World

We start with this example straight from the [Pygame
docs](https://www.pygame.org/docs/):

$:output:python:
# Example file showing a basic pygame "game loop"
import pygame

# pygame setup
pygame.init()
screen = pygame.display.set_mode((1280, 720))
clock = pygame.time.Clock()
running = True

while running:
    # poll for events
    # pygame.QUIT event means the user clicked X to close your window
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

    # fill the screen with a color to wipe away anything from last frame
    screen.fill("purple")

    # RENDER YOUR GAME HERE

    # flip() the display to put your work on screen
    pygame.display.flip()

    clock.tick(60)  # limits FPS to 60

pygame.quit()
$:END

When run, it shows an empty screen:

<center>
![Tutorial output.](tutorial.png)
</center>

## Add drawing of something

An empty screen is not that interesting, so let's see if we can get an
animation going.

We add a call to draw a circle and some logic to move where that circle is
drawn:

$:output:python:
...

pos_x = 50
dt = 0

while running:

    ...

    if pos_x > 500:
        pos_x = 50
    else:
        pos_x += dt*0.3

    pygame.draw.circle(screen, "red", (pos_x, 50), 40)

    ...

    dt = clock.tick(60)  # limits FPS to 60

...
$:END

This seems to work. We get an animated circle:

<center>
![Animated circle.](animation.png)
</center>

## Refactor to clarify game loop concept

Next we want to split the logic of the game loop from the logic of our game. We
refactor in small steps, testing manually that everything works, and end up
with this:

$:output:python:
class Game:

    def __init__(self, loop):
        self.loop = loop
        self.pos_x = 50

    def run(self):
        self.loop.run(self)

    def tick(self, dt, screen):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return True
        if self.pos_x > 500:
            self.pos_x = 50
        else:
            self.pos_x += dt*0.3
        screen.fill("purple")
        pygame.draw.circle(screen, "red", (self.pos_x, 50), 40)

class GameLoop:

    def run(self, game):
        pygame.init()
        screen = pygame.display.set_mode((1280, 720))
        clock = pygame.time.Clock()
        running = True
        dt = 0
        while running:
            if game.tick(dt, screen):
                running = False
            pygame.display.flip()
            dt = clock.tick(60)
        pygame.quit()

Game(GameLoop()).run()
$:END

Remember, we are only doing a spike here. We are trying to learn Pygame and how
we could split the different responsibilities into different classes and how to
possible test it.

The game is now responsible for handling events and drawing the animated circle
and the game loop is responsible for setting up Pygame and calling the game in
a loop.

## How to test this?

I find it easiest to start from the outside when writing tests. What should the
system do? What should our game do?

* I draw a circle on the screen until the user exits.

$:output:python:
class Game:

    """
    I draw an animated circle until the user closes the window.

    >>> game = Game(GameLoop())
    >>> game.run()
    DRAW_CIRCLE
    EXIT
    """

    def __init__(self, loop):
        self.loop = loop

    def run(self):
        self.loop.run(self)

    def tick(self):
        print("DRAW_CIRCLE")

class GameLoop:

    def run(self, game):
        game.tick()
        print("EXIT")

if __name__ == "__main__":
    Game().run()
$:END
