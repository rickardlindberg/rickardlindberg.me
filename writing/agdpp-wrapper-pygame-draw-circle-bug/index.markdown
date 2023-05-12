---
title: 'DRAFT: A case for the infrastructure wrapper'
date: 2023-05-12
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Arrow</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">loop</span><span class="p">):</span>
        <span class="n">v</span> <span class="o">=</span> <span class="n">Point</span><span class="o">.</span><span class="n">from_angle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">angle</span> <span class="o">+</span> <span class="mi">180</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">10</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">v</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="mi">20</span><span class="p">)),</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">15</span><span class="p">)</span>
        <span class="n">loop</span><span class="o">.</span><span class="n">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">v</span><span class="o">.</span><span class="n">times</span><span class="p">(</span><span class="mi">40</span><span class="p">)),</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;blue&quot;</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">20</span><span class="p">)</span>
</pre></div>
</div></div>
`draw_circle` above is part of the infrastructure wrapper. In code that we
control. It in turn makes calls to Pygame like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">position</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">40</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;red&quot;</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span>
            <span class="n">color</span><span class="p">,</span>
            <span class="p">(</span><span class="nb">int</span><span class="p">(</span><span class="n">position</span><span class="o">.</span><span class="n">x</span><span class="p">),</span> <span class="nb">int</span><span class="p">(</span><span class="n">position</span><span class="o">.</span><span class="n">y</span><span class="p">)),</span>
            <span class="n">radius</span>
        <span class="p">)</span>
</pre></div>
</div></div>
So we are actually only calling Pygame's `draw_circle` in one place in our
code.

We fix it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameLoop</span><span class="p">(</span><span class="n">Observable</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">draw_circle</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">position</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">40</span><span class="p">,</span> <span class="n">color</span><span class="o">=</span><span class="s2">&quot;red&quot;</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">if</span> <span class="n">position</span><span class="o">.</span><span class="n">x</span> <span class="o">&gt;=</span> <span class="mi">0</span><span class="p">:</span>
            <span class="c1"># https://github.com/pygame/pygame/issues/3778</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">pygame</span><span class="o">.</span><span class="n">draw</span><span class="o">.</span><span class="n">circle</span><span class="p">(</span>
                <span class="bp">self</span><span class="o">.</span><span class="n">screen</span><span class="p">,</span>
                <span class="n">color</span><span class="p">,</span>
                <span class="p">(</span><span class="nb">int</span><span class="p">(</span><span class="n">position</span><span class="o">.</span><span class="n">x</span><span class="p">),</span> <span class="nb">int</span><span class="p">(</span><span class="n">position</span><span class="o">.</span><span class="n">y</span><span class="p">)),</span>
                <span class="n">radius</span>
            <span class="p">)</span>
</pre></div>
</div></div>
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
