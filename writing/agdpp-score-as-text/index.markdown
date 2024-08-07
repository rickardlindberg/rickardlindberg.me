---
title: Score as text
date: 2023-05-24
tags: agdpp
agdpp: true
---

Currently, the game keeps track of the score by drawing yellow point markers.
One for each point:

<p>
<center>
![Point markers.](points.png)
</center>
</p>

I find it tedious to count them when my son wants to know the score, and I
think he also expressed that he wants to have the score as a number in the
upper right corner.

That's what we will work on in this episode. The end result will look like
this:

<p>
<center>
![Score text.](score-text.png)
</center>
</p>

## Drawing text

The reason that we implemented score display with point markers was that it was
quicker. There was no way to draw text and it was therefore quicker to use
point markers. But that has to change now. Let's see if we can draw some text.

We start by adding a test case to the top-level, initial state test that
asserts that the text '0' is drawn on the screen:

```python
class BalloonShooter:

    """
    I am a balloon shooter game!

    Initial state
    =============

    We run the game for a few frames, then quit:

    >>> events = BalloonShooter.run_in_test_mode(
    ...     events=[
    ...         [],
    ...         [],
    ...         [],
    ...         [],
    ...         [],
    ...         [],
    ...         [GameLoop.create_event_user_closed_window()],
    ...     ]
    ... )

    ...

    The score is drawn:

    >>> set(events.filter("DRAW_TEXT").collect("text"))
    {('0',)}
    """

    ...
```

This fails because there is no event `DRAW_TEXT` yet.

We add a method to draw text and have it emit a `DRAW_TEXT` event like this:

```python
class GameLoop(Observable):

    ...

    def draw_text(self, position, text):
        self.notify("DRAW_TEXT", {
            "x": position.x,
            "y": position.y,
            "text": text,
        })
        f = self.pygame.font.Font(size=100)
        surface = f.render(text, True, "black")
        self.screen.blit(surface, (position.x, position.y))
```

We test the pygame code manually to ensure that we use the text drawing api
correctly and that text appears on the screen.

## Keeping track of score to draw

Before we kept track of the score by adding point markers to a sprite group. We
replace this sprite group with a new `Score` object:

```diff
-        self.points = self.add(SpriteGroup())
+        self.score = self.add(Score())
```

Instead of adding point markers, we just add a number to increase the score:

```diff
-                    self.points.add(PointMarker(position=Point(
-                        x=20+len(self.points.get_sprites())*12,
-                        y=700
-                    )))
+                    self.score.add(1)
```

The `Score` class looks like this:

```python
class Score:

    def __init__(self):
        self.score = 0

    def add(self, points):
        self.score += points

    def update(self, dt):
        pass

    def draw(self, loop):
        loop.draw_text(position=Point(x=1100, y=20), text=str(self.score))
```

## Adapting tests

We already have tests that make sure that we count the score correctly. We have
to modify them slightly to look at the score number instead of counting point
markers in the sprite group. We change them like this:

```diff
-    >>> game.get_points()
-    []
+    >>> game.get_score()
+    0
```

Now all tests pass and the score is displayed with text instead of point
markers. Success!

## Summary

This change went rather smoothly. Adding functionality to draw text was not
that much work. Perhaps we could just as well have done that from the start.
But point markers felt easier at the time.

Our customer is happy with the new score display. When we play the game, we
sometimes decide to go for 100 points, then stop. Perhaps that could become a
competitive aspect? How fast can you get to 100 points? And the time will be
the "final" score? We'll see.

See you in the next episode!
