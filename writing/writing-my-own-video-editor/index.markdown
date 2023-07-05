---
title: 'DRAFT: Writing my own video editor'
date: 2023-07-05
tags: rlvideo,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

On May 28 I
[wrote](https://hachyderm.io/@rickardlindberg/110447282439624451) this:

> Got the urge to write my own video editor. Tired of kdenlive's instability.
> And I don't need something that advanced. Reading a bit about the MLT
> framework makes me think that it might actually be possible to do in a
> reasonable time.
>
> Sometimes I feel bad for starting more projects than i finish. On the other
> hand, every project I do teach me something. And I do this (believe it or
> not) for my enjoyment.

It happens to me from time to time. I get an idea for something I want to
build. Sometimes the urge goes away. This time it didn't. Here is the story so
far.

## Why write a video editor?

I like to build things. In particular I like to build things that I have a use
for myself.

Today, I use [Kdenlive](https://kdenlive.org/en/) when I edit various videos
that I make. The program has served me well. However, every time I work with
it, I get a little frustrated. It often crashes on me. It often feels slow.
There are certain things I want to do that I don't know how.

The normal way of solving these problems I think would include

* Buy a faster computer
* Try the latest version of Kdenlive (would require me to upgrade Fedora
  version as well)
* Learn Kdenlive better

But I am a programmer, and I like to build things. So from that point of view,
the obvious solution to my problems is to build my own video editor
specifically for my needs.

## More ideas

On June 16 I sketched the following in my notebook:

<p>
<center>
![Initial sketch of a timeline.](sketch-initial.png)
</center>
</p>

I wanted to think about how to represent clips on the timeline.  This sketch
also told me that the urge had not gone away.

## Researching MLT

Writing a video editor seems like a daunting task. The only reason that I think
will make it possible is the [MLT](https://www.mltframework.org/). From their
website:

> MLT is an open source multimedia framework, designed and developed for
> television broadcasting. It provides a toolkit for broadcasters, video
> editors, media players, transcoders, web streamers and many more types of
> applications.

So a lot of the heavy lifting of a video editor can be done by MLT. (That is my
guess anyway.) What I can focus on is writing a nice frontend for it.

Instead of speculating, I did some spikes to learn how to use MLT from Python.

Here is one example how to put two clips next to each other on a timeline and
preview the result:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">import</span> <span class="nn">time</span>
<span class="kn">import</span> <span class="nn">mlt</span>

<span class="n">mlt</span><span class="o">.</span><span class="n">Factory</span><span class="p">()</span><span class="o">.</span><span class="n">init</span><span class="p">()</span>
<span class="n">profile</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Profile</span><span class="p">()</span>
<span class="n">playlist</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Playlist</span><span class="p">()</span>
<span class="n">playlist</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="s2">&quot;VID_20230611_120041.mp4&quot;</span><span class="p">))</span>
<span class="n">playlist</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="s2">&quot;VID_20230611_115932.mp4&quot;</span><span class="p">))</span>
<span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="s2">&quot;sdl&quot;</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;rescale&quot;</span><span class="p">,</span> <span class="s2">&quot;none&quot;</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">playlist</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
<span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
    <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mi">1</span><span class="p">)</span>
</pre></div>
</div></div>
More examples of my spikes can be found
[here](https://github.com/rickardlindberg/rlvideo/blob/91dd25a0d39cbe25e8ce85157115d023b4d2c78c/spikes/mlt_hello_world.py).

To help me do the spikes, I used the following resources:

* [MLT Framework Design](https://www.mltframework.org/docs/framework/): A good
  introduction to how MLT works.

* [Python
  examples](https://github.com/mltframework/mlt/tree/master/src/swig/python):
  Examples how to use MLT from Python.

* [Flowblade](https://github.com/jliljebl/flowblade): Another video editor that
  is written in Python and uses MLT.

* [MLT API documentation](https://www.mltframework.org/doxygen/annotated.html):
  The C API documentation. Translating this to Python has been mostly straight
  forward.

* Wrap my head around basic primitives and how to map that to what I want
    * Use my structure for clips
    * Turn that into mlt primitives

* Can overlap clips. Only one visible track. Editor figures out what tracks to
  put things on in the background.

## Timeline representation

* I think I know enough. Next I want to explore how I would like to organize
  clips and work with a timeline.

28th of June:

<p>
<center>
![Sketch of a timeline visualization of overlap.](sketch-timeline-visualization.png)
</center>
</p>

30th of June:

<p>
<center>
![Sketch of a timeline splits.](sketch-split-sections.png)
</center>
</p>

## Putting it together

3rd of July:

<p>
<center>
![Current look of application.](current-status.png)
</center>
</p>

* rlvideo

    * spike MLT june 28
    * Timeline.get_groups (first version)

* Embed SDL window (got idea from flowblade source code)

## Struggles

## Future
