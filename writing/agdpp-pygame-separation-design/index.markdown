---
title: 'DRAFT: Separating pygame completely from the rest of the game'
date: 2023-04-14
tags: agdpp,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this episode we reflect on our current design. We see something that bothers
us and talk about it and how to fix it.

## The problem

Right now our game is split up into two main classes: the game and the game
loop. The game contains the logic of our game, while the game loop is
responsible for setting up pygame and calling our game on every frame.

Almost all references to pygame are contained in the game loop class. Our game
knows almost nothing about pygame. Almost. And that bothers me.

Let's have a look at the test for our game:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">I draw an animated circle until the user closes the window.</span>

<span class="sd">&gt;&gt;&gt; loop = GameLoop.create_null(</span>
<span class="sd">...     events=[</span>
<span class="sd">...         [],</span>
<span class="sd">...         [],</span>
<span class="sd">...         [pygame.event.Event(pygame.QUIT)],</span>
<span class="sd">...     ]</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; events = loop.track_events()</span>
<span class="sd">&gt;&gt;&gt; Game(loop).run()</span>
<span class="sd">&gt;&gt;&gt; events</span>
<span class="sd">PYGAME_INIT =&gt;</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 50</span>
<span class="sd">CLEAR_SCREEN =&gt;</span>
<span class="sd">DRAW_CIRCLE =&gt;</span>
<span class="sd">    x: 51</span>
<span class="sd">PYGAME_QUIT =&gt;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We can see references to pygame in two places. First when we create the quit
event. We create an instance of a pygame event and pass that to the null
version of the game loop. Later in the events that we assert on, there are
event names mentioning pygame (`PYGAME_INIT` and `PYGAME_QUIT`).

Why does this bother me?

One purpose of introducing the game loop class was to separate pygame code from
our game. One reason to do that is that our game becomes easier to test. And if
it's easier to test, it suggests that the design is also better.

But some details of pygame are leaking out.

If `GameLoop` were instead called `PygameGameLoop`, I would be more fine with
this. But I think the design would be more clear if the game didn't know
anything at all about pygame.

Our game loop uses the [infrastructure
wrapper](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#infrastructure-wrappers)
pattern. One purpose of that pattern is to isolate and contain infrastructure
code so that the user of it can be provided an interface that is optimal for
its consumption. In our case we want to design our game loop to fit exactly
what our game needs. And the loop should encapsulate all the details of how to
make that happen (using pygame).

Our game now needs to know that the pygame quit event is fired when the user
closes the window. But wouldn't it be more clear if that could be expressed in
the code something like this instead?

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">is_user_closed_window</span><span class="p">():</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">loop</span><span class="o">.</span><span class="n">quit</span><span class="p">()</span>
</pre></div>
</div></div>
Above, the game does not need to know about pygame and can directly express the
idea that if the user closes the window, the game loop should be quit.

Enough talking, let's see if we can fix this.

* Create Event wrapper class
* Add is_user_close_window
* Pass wrapper to tick and modify tick
* Test pass
* Add static create_user_close_window
* Make events pygame independent

* Kind of nice and better
* Not quite sure what I feel about event wrapper though

* Is this separation needed if we only plan on using pygame?

* I reset
* Smaller steps
    * Like separation of pygame
    * Let's fix one thing at a time
    * Starting with loop events mentioning pygame

* Designing in the beginning is easier, but it is harder to see smells. If you
  go too far in the wrong direction, you can always recover with refactoring,
  but the sooner you do it the easier. (Ron, Chet pair programming video.)
