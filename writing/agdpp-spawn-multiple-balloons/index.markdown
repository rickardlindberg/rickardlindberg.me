---
title: Spawn multiple balloons
date: 2023-06-17
tags: agdpp
agdpp: true
---

We [previously](/writing/agdpp-game-over/index.html) had a story about balloons
moving downwards. We scratched that because other stories were more important
for the first version of the balloon shooter. With those stories done, I think
a more realistic balloon spawning and movement pattern is the most valuable
thing we can work on.

## Video version

The video version of this episode:

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/unYD_bPyadc" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
</center>

## Code review

Let's review our code and look at how balloons are managed.

Our game scene has a sprite group for balloons which by default contains only
one balloon:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">balloons</span><span class="o">=</span><span class="p">[(</span><span class="mi">50</span><span class="p">,</span> <span class="mi">50</span><span class="p">)],</span> <span class="o">...</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">([</span>
            <span class="n">Balloon</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">))</span> <span class="k">for</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span> <span class="ow">in</span> <span class="n">balloons</span>
        <span class="p">]))</span>
</pre></div>
</div></div>
This sprite group is modified in the `update` method if an arrows hits a
balloon:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GameScene</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="o">...</span>
        <span class="k">for</span> <span class="n">arrow</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">flying_arrows</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
            <span class="o">...</span>
            <span class="k">for</span> <span class="n">balloon</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
                <span class="k">if</span> <span class="n">arrow</span><span class="o">.</span><span class="n">hits_baloon</span><span class="p">(</span><span class="n">balloon</span><span class="p">):</span>
                    <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">balloon</span><span class="p">)</span>
                    <span class="bp">self</span><span class="o">.</span><span class="n">balloons</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Balloon</span><span class="p">(</span><span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">50</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">50</span><span class="p">)))</span>
                    <span class="o">...</span>
</pre></div>
</div></div>
So the hit balloon is removed, and a new one is added.

How do balloons move?

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloon</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">position</span><span class="p">,</span> <span class="n">radius</span><span class="o">=</span><span class="mi">40</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="n">position</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">radius</span> <span class="o">=</span> <span class="n">radius</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="k">if</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">x</span> <span class="o">&gt;</span> <span class="mi">1200</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">50</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">position</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">position</span><span class="o">.</span><span class="n">move</span><span class="p">(</span><span class="n">dx</span><span class="o">=</span><span class="n">dt</span><span class="o">*</span><span class="mf">0.3</span><span class="p">)</span>
</pre></div>
</div></div>
They move from left to right and wrap around at x=1200.

## Strategy

To be able to write more isolated tests for balloon behavior, I want to start
with a few refactorings. I want to extract a `Balloons` class which contains
most logic related to balloons. Then I want to write tests for new behavior.
This is also known as make the change easy, then make the easy change.

We begin by creating the class and using it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-        self.balloons = self.add(SpriteGroup([</span>
<span class="gd">-            Balloon(Point(x=x, y=y)) for (x, y) in balloons</span>
<span class="gd">-        ]))</span>
<span class="gi">+        self.balloons = self.add(Balloons(balloons))</span>
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">positions</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="p">[</span>
            <span class="n">Balloon</span><span class="p">(</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="n">y</span><span class="p">))</span> <span class="k">for</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span> <span class="ow">in</span> <span class="n">positions</span>
        <span class="p">])</span>
</pre></div>
</div></div>
We continue to move some behavior into this new class:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_balloon_hit_by_arrow</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">arrow</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">balloon</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">():</span>
            <span class="k">if</span> <span class="n">arrow</span><span class="o">.</span><span class="n">hits_baloon</span><span class="p">(</span><span class="n">balloon</span><span class="p">):</span>
                <span class="k">return</span> <span class="n">balloon</span>

    <span class="k">def</span> <span class="nf">spawn_new</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">Balloon</span><span class="p">(</span><span class="n">position</span><span class="o">=</span><span class="n">Point</span><span class="p">(</span><span class="n">x</span><span class="o">=</span><span class="mi">50</span><span class="p">,</span> <span class="n">y</span><span class="o">=</span><span class="mi">50</span><span class="p">)))</span>
</pre></div>
</div></div>
With that in place, we can simplify the update code like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-            for balloon in self.balloons.get_sprites():</span>
<span class="gd">-                if arrow.hits_baloon(balloon):</span>
<span class="gd">-                    self.balloons.remove(balloon)</span>
<span class="gd">-                    self.balloons.add(Balloon(position=Point(x=50, y=50)))</span>
<span class="gd">-                    self.score.add(1)</span>
<span class="gi">+            hit_balloon = self.balloons.get_balloon_hit_by_arrow(arrow)</span>
<span class="gi">+            if hit_balloon:</span>
<span class="gi">+                self.balloons.remove(hit_balloon)</span>
<span class="gi">+                self.balloons.spawn_new()</span>
<span class="gi">+                self.score.add(1)</span>
</pre></div>
</div></div>
There is probably some more functionality that we can move into the new
balloons class, but let's stop here for now and focus on the new behavior.

(If you want to see this refactoring happening in smaller steps and in real
time, check out the video version.)

## Stories

Here is some new behavior that we would like to have:

* balloons move downwards
* balloons appear at different x positions
* multiple balloons are in the air at the same time

Let's start with the first one and write a test for the new movement pattern:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    &gt;&gt;&gt; balloons = Balloons([(50, 50)])</span>
<span class="sd">    &gt;&gt;&gt; balloons.get_sprites()[0].get_position()</span>
<span class="sd">    (50, 50)</span>
<span class="sd">    &gt;&gt;&gt; balloons.update(5)</span>
<span class="sd">    &gt;&gt;&gt; x, y = balloons.get_sprites()[0].get_position()</span>
<span class="sd">    &gt;&gt;&gt; x</span>
<span class="sd">    50</span>
<span class="sd">    &gt;&gt;&gt; y &gt; 50</span>
<span class="sd">    True</span>
<span class="sd">    &quot;&quot;&quot;</span>

    <span class="o">...</span>
</pre></div>
</div></div>
First we make sure that the first balloon in the sprite group is at the initial
position that we gave it. Then we assert that it has moved downward after an
update.

To make this test pass, we make this change:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span> class Balloon:

     ...

     def update(self, dt):
<span class="gd">-       if self.position.x &gt; 1200:</span>
<span class="gd">-           self.position = self.position.set(x=50)</span>
<span class="gd">-       else:</span>
<span class="gd">-           self.position = self.position.move(dx=dt*0.3)</span>
<span class="gi">+       self.position = self.position.move(dy=dt*self.speed)</span>
</pre></div>
</div></div>
We only needed to modify the `Balloon` class. Does that mean that we should
put the test in this class instead? I don't know. For now, I think it's nice if
we can keep all balloon related tests in the same place.

If we run the game now, a single balloon will move downwards and then disappear
at the bottom of the screen. That's no fun. No more balloon to shoot down.
Let's fix that.

Here is a test that checks that there are always 3 balloons in the air:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; balloons = Balloons([(50, 50)], space)</span>
<span class="sd">&gt;&gt;&gt; len(balloons.get_sprites())</span>
<span class="sd">1</span>
<span class="sd">&gt;&gt;&gt; balloons.update(5)</span>
<span class="sd">&gt;&gt;&gt; len(balloons.get_sprites())</span>
<span class="sd">3</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We make this pass by adding spawn logic in the update method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Balloons</span><span class="p">(</span><span class="n">SpriteGroup</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">):</span>
        <span class="n">SpriteGroup</span><span class="o">.</span><span class="n">update</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">dt</span><span class="p">)</span>
        <span class="k">while</span> <span class="nb">len</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">get_sprites</span><span class="p">())</span> <span class="o">&lt;</span> <span class="mi">3</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">spawn_new</span><span class="p">()</span>
</pre></div>
</div></div>
As long as we shoot down balloons, new ones will be spawned. But if we miss
three balloons, they will continue to move downwards outside the screen, and no
new balloons will be spawned. Let's work on removing balloons outside the
screen.

Here is a test describing this behavior:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; space = OutsideScreenSpace(500, 500)</span>
<span class="sd">&gt;&gt;&gt; balloons = Balloons([(1000, 1000)], space)</span>
<span class="sd">&gt;&gt;&gt; (balloon,) = balloons.get_sprites()</span>
<span class="sd">&gt;&gt;&gt; balloons.update(5)</span>
<span class="sd">&gt;&gt;&gt; balloon in balloons.get_sprites()</span>
<span class="sd">False</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The idea is that we place a balloon outside the screen. Then we call update and
make sure that it is no longer in the sprite group.

We have used the `OutsideScreenSpace` before to remove arrows that are outside
the screen. When we add it here to `Balloons` we have to update all
instantiations of it to include it. Once that is done, we make the test pass
like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span> class Balloons(SpriteGroup):

     ...

     def update(self, dt):
         SpriteGroup.update(self, dt)
<span class="gi">+        for balloon in self.get_sprites():</span>
<span class="gi">+            if self.space.hits(Point(*balloon.get_position()), 10):</span>
<span class="gi">+                self.remove(balloon)</span>
         while len(self.get_sprites()) &lt; 3:
             self.spawn_new()
</pre></div>
</div></div>
The game now plays without problems, however, it's a little boring that
balloons are always spawned at the same position.

We modify the spawning code like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span> class Balloons(SpriteGroup):

     ...

     def spawn_new(self):
<span class="gd">-        self.add(Balloon(position=Point(x=50, y=50)))</span>
<span class="gi">+        self.add(Balloon(position=Point(x=self.space.get_random_x(50), y=50)))</span>
</pre></div>
</div></div>
And add the corresponding method in `OutsideScreenSpace`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">OutsideScreenSpace</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">get_random_x</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">margin</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">random</span><span class="o">.</span><span class="n">randint</span><span class="p">(</span><span class="n">margin</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">width</span><span class="o">-</span><span class="n">margin</span><span class="o">*</span><span class="mi">2</span><span class="p">)</span>
</pre></div>
</div></div>
We don't write any tests for this. Why? I guess because I feel confident that
this will work. And maybe because testing random is not straight forward.
Perhaps we should add a test for `get_random_x` that checks that the x we get
back is within the width minus margin. We make a note of that.

Here is what the game looks like now:

<p>
<center>
![Multiple balloons.](multiple-balloons.png)
</center>
</p>

We have accomplished what we set out to do. I think the game is a little more
fun to play now. Success!

## Summary

We began by doing some refactoring to make the new behavior easy to add.
It was easy to add and it went smoothly. However, after adding new
functionality and working with an area of the code, we have probably noticed
things that can improve. We might even have ignored it to focus on adding the
new behavior.

What I like to do in those situations is to take a break and come back and
review the code a little later.

This time I came up with many small changes to improve the clarity of the code.
Here are some examples from that session:

* [Replace (x, y) with position in Arrow and Balloon.](https://github.com/rickardlindberg/agdpp/commit/782cda7032896b15d89058b0fe3bc4ccbb54da8c)
* [Move balloon space hit check to balloon where radius can be used for better hit test.](https://github.com/rickardlindberg/agdpp/commit/7d6c884d727bef96b1efcc524e4c8956cfd41c72)
* [Replace OutsideScreenSpace with a more generic Rectangle.](https://github.com/rickardlindberg/agdpp/commit/fe4b477a7ad4c89a56af58dceac84cfc100b2f8f)
* [Make spawning unaware of where its region is (no hard coded y=50).](https://github.com/rickardlindberg/agdpp/commit/c4f8ff4924f1ee4a83ece0d0360ed44e512af194)
* [No need to spawn balloon when shot down since it happens anyway.](https://github.com/rickardlindberg/agdpp/commit/7faddca9ad82c1bde8d6ea0d00bbbc8cfb5d6fbf)
* [Balloons positions are points instead of tuples.](https://github.com/rickardlindberg/agdpp/commit/19e51efe110e81e398a5f9a7401608f870afb4ff)
* [Clean up shooting arrow tests.](https://github.com/rickardlindberg/agdpp/commit/18a9a5af49966f9b2c7e8841495687181e7fedfb)

Making these tiny improvements feels so good. They are all quite small changes,
but they make a huge impact. Ok, maybe not huge, but the improvements compound.
The point is that if you keep making tiny improvements, the code base gets
easier and easier to work with.

See you in the next episode!
