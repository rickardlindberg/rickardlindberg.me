---
title: 'DRAFT: Does TDD work when building a game?'
date: 2023-06-10
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

When I [started](/writing/agdpp-introduction/index.html) this series, one of
the areas that I wanted to explore was how well TDD works when building a game.

The short answer is that I find that it works as well as on any other project
I've done.

The longer answer is that TDD might not be enough. There are some things which
are hard to test in a game. That's what I want to talk about in this blog post
and also how we can overcome the limits of TDD in those situations.

## Example: joystick movement

What is hard to test with TDD? Here is an example.

A [few episodes ago](/writing/agdpp-logitech-gamepad-f310/index.html), we
worked on controlling the game with a gamepad. The gamepad has a little
joystick (correct name?) and when we push it to the left, we want the arrow to
turn left, and likewise for right. That logic works well to implement with TDD,
however, that is not enough. It is also important that it "feels good" to turn
the arrow. That it responds fast enough, that it turns with an appropriate
speed, and so on. That part, I think, is impossible to work out with TDD since
we don't know the correct answer. The only way to get the correct answer is to
experiment. Try turning with different parameters set and see which ones feel
better.

We can still use TDD for the first part if we relax the assertions. For
example, we can't assert that the arrow is turned by a specific amount, but we
can assert that it is turned more to the left than before. Here is an example
of that:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; initial_angle = game.get_arrow_angle()</span>
<span class="sd">&gt;&gt;&gt; game.event(GameLoop.create_event_keydown(KEY_LEFT))</span>
<span class="sd">&gt;&gt;&gt; game.update(1)</span>
<span class="sd">&gt;&gt;&gt; game.get_arrow_angle() &lt; initial_angle</span>
<span class="sd">True</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We assert the basic functionality in the test, and then we can play the game
and adjust parameters for turning until it feels good, and this test will
continue to work.

## A reflection on test automation

This kind of reasoning about tests reminds me of something that
Dale Emery [wrote](https://mstdn.social/@dhemery/109298626455434624):

> Half of the art of #TestAutomation is making the test code sensitive to
> things you care about and insensitive to things you don't care about.

and

> The other half of the art of #TestAutomation is deciding which things to care
> about.

## Testing in-game

What are you saying Rickard? That we can't verify the behavior of the game with
tests and that we need to play the game to make sure it works? And do this
repeatedly? That sounds like a very long feedback loop. What about if we have
to tweak numbers for an effect that only happens after you scored 100 points?
Should we play the game, score 100 points, look at the effect, tweak a
parameter, and do it all over again?

You certainly need to play the game to be able to get some details right. But
we should not need to play "unnecessary parts" over and over again until we
reach the point in the game that we are interested in testing.

So far, the balloon shooter is not that complex. If we want to test turn speed
for example, we can start the game and start turning. No additional steps
needed. However, when the game becomes more complex (start screen, player
selection, etc) and require more interaction to get to where we want to go,
I think we should approach it differently.

## Test applications

* Quickly takes you to a scene in a game
* Test specific function that don't fit TDD
* Test look and feel of level 99
* Test look and feel of level progression speed

## Application outside games

I have used this approach in [Timeline](/projects/timeline/index.html) as well.
Timeline is a GUI application, so a different domain from games. But the GUI
elements have the same problem: you can't assert in a test that they look good.

Instead of running the application and opening the correct dialog, we
implemented a `--halt-gui` flag in our test runner. When that flag is present,
the tests will open GUI dialogs with mock data and you can visually inspect
that it looks good:

<p>
<center>
![Timeline halt GUI.](timeline-halt-gui.png)
</center>
</p>

Your workflow can than be

1. run test suite with specific test and `--halt-gui` flag
2. inspect the dialog
3. tweak layout
4. repeat

This makes the feedback loop a little faster. There is no need to constantly
run the application and click your way to the correct dialog.

## 

Should we never run our application? Should we only get feedback from tests and
small test applications?

No, it is important to use the application as well to get a sense of what needs
improving.

In [...](https://ronjeffries.com/articles/-y023/python/-o110/110/), Ron writes

> And it seems to me that with a video-oriented game, we always wind up needing
> to watch it run, both for confidence and because, every now and then, we
> discover a problem.

## Summary

Those are my reflections on using TDD for a game so far. Do you have similar
experiences? Please let me know.

See you in the next episode!
