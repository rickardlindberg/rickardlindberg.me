---
title: Writing my own video editor
date: 2023-07-04
tags: rlvideo,draft
---

On May 28 I wrote
[this](https://hachyderm.io/@rickardlindberg/110447282439624451):

> Got the urge to write my own video editor. Tired of kdenlive's instability.
> And I don't need something that advanced. Reading a bit about the MLT
> framework makes me think that it might actually be possible to do in a
> reasonable time.
>
> Sometimes I feel bad for starting more projects than i finish. On the other
> hand, every project I do teach me something. And I do this (believe it or
> not) for my enjoyment.

It happens to me from time to time. I get an idea for something that I want to
build. Sometimes the urge goes away. This time it didn't.

## Why write a video editor?

I like to build things. In particular I like to build things that I have a use
for myself.

Currently, I use [Kdenlive](https://kdenlive.org/en/) as my video editor.  It
has served me well. However, every time I work with it, I get a little
frustrated. It often crashes on me, it often feels slow, and there are certain
things that I want to do that I don't know how.

The normal way of solving these problems I think would include

* Try the latest version of Kdenlive (would require me to upgrade Fedora
  version as well)
* Buy a faster computer
* Learn Kdenlive better

But I can program, and I like to build things. So from that point of view, the
obvious solution to my problem is to build my own video editor specifically for
my needs.

## More ideas

On June 16 I sketched the following in my notebook:

<p>
<center>
![Initial sketch of a timeline.](sketch-initial.png)
</center>
</p>

I wanted to think about how to represent clips on a timeline in my ideal
video editor. This sketch also told me that the urge had not gone away.

## Researching MLT

Writing a video editor seems like a daunting task. The only reason I think it
will be possible is with help from [MLT](https://www.mltframework.org/). From
their website:

> MLT is an open source multimedia framework, designed and developed for
> television broadcasting. It provides a toolkit for broadcasters, video
> editors, media players, transcoders, web streamers and many more types of
> applications.

So a lot of the heavy lifting of a video editor can be done by MLT. That is my
guess and hope anyway. What I can focus on is writing a nice frontend for it.

Instead of speculating, I did some spikes to learn how to use MLT from Python.

Here is one example how to put two clips next to each other on a timeline and
preview the result:

$:output:python:
import time
import mlt

mlt.Factory().init()
profile = mlt.Profile()
playlist = mlt.Playlist()
playlist.append(mlt.Producer(profile, "VID_20230611_120041.mp4"))
playlist.append(mlt.Producer(profile, "VID_20230611_115932.mp4"))
consumer = mlt.Consumer(profile, "sdl")
consumer.set("rescale", "none")
consumer.connect(playlist)
consumer.start()
while consumer.is_stopped() == 0:
    time.sleep(1)
$:END

More examples from my spikes can be found
[here](https://github.com/rickardlindberg/rlvideo/blob/91dd25a0d39cbe25e8ce85157115d023b4d2c78c/spikes/mlt_hello_world.py).

To help me do the spikes, I used the following resources:

* [MLT Framework Design](https://www.mltframework.org/docs/framework/): A good
  introduction to how MLT works.

* [Python
  examples](https://github.com/mltframework/mlt/tree/master/src/swig/python):
  Examples how to use MLT from Python. They are quite limited, but gives you a
  good starting point.

* [MLT API documentation](https://www.mltframework.org/doxygen/annotated.html):
  The C API documentation. Translating this to Python has been mostly straight
  forward.

* [Flowblade](https://github.com/jliljebl/flowblade): Another video editor that
  is written in Python and MLT. So far, I've used it mainly to figure out how
  to embed the video preview window in an gui application.

## Design idea

By doing the spikes, I have a basic understanding how to use MLT and how it
could be used to build a video editor.

The design I have in mind for the video editor looks something like this:

$:output:python:
class Timeline:

    def __init__(self):
        self.clips = []

    def add(self, clip):
        self.clips.add(clip)

    def to_mlt_producer(self, ...):
        ...

    def draw(self, ...):
        ...
$:END

I want to use my own data structures for representing clips on a timeline. I
think that will give me a design which is clean and easy to work with. I can
design those structures to be good for the kinds of operations that I want to
perform.

However, somehow that structure must be turned into an MLT producer. That is
what `to_mlt_producer` is for. Transforming from my world into the MLT world.
When we have an MLT producer, we can preview the composition and render a final
result.

## Timeline representation

So what representation of clips is best? It depends on how it's going to be
used. How do I want to work with clips on a timeline in my ideal video editor?

On June 28 I sketch this:

<p>
<center>
![Sketch of a timeline visualization of overlap.](sketch-timeline-visualization.png)
</center>
</p>

And on June 30 I sketch this:

<p>
<center>
![Sketch of a timeline splits.](sketch-split-sections.png)
</center>
</p>

I don't think that I want to use multiple tracks in my timeline. There should
only be one track. When clips overlap, multiple tracks might be created in the
background, but it shouldn't be visible to the user.

The sketches help me figure this out.

I think we can have one structure will all the clips and their positions. Then
we can split those up into sections. One type of section has no overlaps, and
the other do. Overlaps must be handled differently. They both render
differently (stacked on top of eachother) and they must produce multiple MLT
tracks in the background.

I work on this section splitting code and end up with this:

$:output:python:
"""
>>> a = Source("A").create_cut(0, 20).at(0)
>>> b = Source("b").create_cut(0, 20).at(10)
>>> cuts = Cuts()
>>> cuts = cuts.add(a)
>>> cuts = cuts.add(b)
>>> cuts.split_into_sections().to_ascii_canvas()
|<-A0------|--A10---->|--b10---->|
|          |<-b0------|          |
>>> cuts.modify(b, lambda cut: cut.move(1)).split_into_sections().to_ascii_canvas()
|<-A0-------|--A11--->|--b9------>|
|           |<-b0-----|           |
"""
$:END

## Putting it together

$:output:python:
class Section:

    ...

    def to_mlt_producer(self, profile):
        if len(self.section_cuts) == 1:
            return self.section_cuts[0].cut.to_mlt_producer(profile)
        else:
            tractor = mlt.Tractor()
            for section_cut in self.section_cuts:
                tractor.insert_track(
                    section_cut.cut.to_mlt_producer(profile),
                    0
                )
            for clip_index in reversed(range(len(self.section_cuts))):
                if clip_index > 0:
                    transition = mlt.Transition(profile, "luma")
                    transition.set("in", 0)
                    transition.set("out", self.section_cuts[clip_index].cut.length-1)
                    tractor.plant_transition(transition, clip_index, clip_index-1)
            return tractor

    def draw(self, context, height, x_factor, rectangle_map):
        sub_height = height // len(self.section_cuts)
        rest = height % len(self.section_cuts)
        y = 0
        for index, section_cut in enumerate(self.section_cuts):
            if rest:
                rest -= 1
                h = sub_height + 1
            else:
                h = sub_height
            section_cut.draw(context, y, h, x_factor, rectangle_map)
            y += h
$:END

3rd of July:

<p>
<center>
![Current look of application.](current-status.png)
</center>
</p>

* Embed SDL window (got idea from flowblade source code)

## Struggles

## Future
