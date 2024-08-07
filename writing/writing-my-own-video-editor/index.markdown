---
title: Writing my own video editor
date: 2023-07-28
tags: rlvideo
---

On May 28 I write
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
build. Sometimes the urge goes away. This time it doesn't.

## Why write a video editor?

I like to build things. In particular I like to build things that I have a use
for myself.

Currently, I use [Kdenlive](https://kdenlive.org/en/) as my video editor.  It
has served me well. However, every time I work with it, I get a little
frustrated. It often crashes on me, it often feels slow, and there are certain
things that I want to do that I don't know how.

The normal way of solving those problems I think would include

* Trying the latest version of Kdenlive (would require me to upgrade Fedora
  version as well)
* Buying a faster computer
* Learning Kdenlive better

But I can program, and I like to build things. So from that point of view, the
obvious solution is to build my own video editor specifically for my needs.

Even if it ends up being unusable as my video editor, I will have had a good
time working on it and most likely learned a thing or two.

## More ideas

On June 16 I sketch the following in my notebook:

<p>
<center>
![Initial sketch of a timeline.](sketch-initial.png)
</center>
</p>

I think about how to represent clips on a timeline in my ideal video editor.
This sketch also tells me that the urge has not gone away.

## Researching MLT

Writing a video editor seems like a daunting task. The only reason that I think
it will be possible is with help from [MLT](https://www.mltframework.org/).
From their website:

> MLT is an open source multimedia framework, designed and developed for
> television broadcasting. It provides a toolkit for broadcasters, video
> editors, media players, transcoders, web streamers and many more types of
> applications.

So a lot of the heavy lifting of a video editor can be done by MLT. That is my
guess and hope anyway. What I can focus on is writing a nice frontend for it.

Instead of speculating, I do some spikes to learn how to use MLT from Python.

Here is one example how to put two clips next to each other on a timeline and
preview the result:

```python
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
```

More examples from my spikes can be found
[here](https://github.com/rickardlindberg/rlvideo/blob/91dd25a0d39cbe25e8ce85157115d023b4d2c78c/spikes/mlt_hello_world.py).

To help me do the spikes, I use the following resources:

* [MLT Framework Design](https://www.mltframework.org/docs/framework/): A good
  introduction to how MLT works.

* [Python
  examples](https://github.com/mltframework/mlt/tree/master/src/swig/python):
  Examples how to use MLT from Python. They are quite limited, but give you a
  good starting point.

* [MLT API documentation](https://www.mltframework.org/doxygen/annotated.html):
  The C API documentation. Translating this to Python has been mostly straight
  forward.

* [Flowblade](https://github.com/jliljebl/flowblade): Another video editor that
  is written in Python and MLT.

## Design idea

By doing the spikes, I have a basic understanding of how to use MLT and how it
could be used to build a video editor.

The design I have in mind for the video editor looks something like this:

```python
class Timeline:

    def __init__(self):
        self.clips = []

    def add(self, clip):
        self.clips.add(clip)

    def to_mlt_producer(self, ...):
        ...

    def draw(self, ...):
        ...
```

That is, I want to use custom data structures for representing clips on a
timeline. I think that will give us a design which is clean and easy to work
with. We can design those structures to be good for the kinds of operations
that we want to perform.

However, somehow those structures must be turned into an MLT producer. That is
what `to_mlt_producer` is for. Transforming from our world into the MLT world.
When we have an MLT producer, we can preview the composition and render a final
result. But all the edit operations will be done on our custom data structures.

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

I don't think that I want to have multiple tracks in the timeline. There should
only be one track. When clips overlap, multiple tracks might be created in the
background, but the user should not need to create tracks manually.

The sketches help me figure this out.

I think that we can have one structure with all the clips and their positions.
Then we can split those up into sections. One type of section has no overlaps,
and the other do. Overlaps must be handled differently. They both render
differently (stacked on top of each other) and they must produce multiple MLT
tracks in the background.

I work on this section splitting code and end up with this:

```python
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
```

The `to_ascii_canvas` is only used in tests to give me faster feedback on the
splitting code. It also documents quite nicely what the timeline would look
like in different situations.

## Putting it together

I spend quite some time getting the splitting of cuts to work. Even before I
know if this design will work out. (Not very smart.) We know if it will work
out when we put everything together.

I first try to put everything together in a Pygame application, but it gives me
all kinds of problems, so I decide to try GTK instead.

The application has two parts: one that shows the timeline, and one that shows
the preview window.

The timeline is created something like this:

```python
def timeline_draw(widget, context):
    sections.draw(context, ...)
timeline = Gtk.DrawingArea()
timeline.connect("draw", timeline_draw)
```

It hooks up the draw event and lets the sections (created by
`split_into_sections`) do all the drawing.

The preview window is created something like this:

```python
preview = Gtk.DrawingArea()
os.putenv("SDL_WINDOWID", str(preview.get_window().get_xid()))
consumer = mlt.Consumer(profile, "sdl")
consumer.start()
consumer.connect(sections.to_mlt_producer(profile))
```

It connects the SDL consumer to the preview window and the producer (created by
`to_mlt_producer`) to the consumer. I learn how to make the SDL consumer draw
its output inside a GTK window by looking at the Flowblade source code.

I get a basic version of `Sections.draw` and `Sections.to_mlt_producer` and end
up with this on July  3:

<p>
<center>
![Current look of application.](current-status.png)
</center>
</p>

## Future

At this point we have a sort of proof of concept of the design. We can now

* Programmaticlly load clips into a timeline data structure.
* This structure can draw itself onto a Cairo context which we can use to
  render it inside a GTK application.
* This structure can also generate an MLT producer which we can use to preview
  the composition using the SDL consumer and have the output shown in a window
  in our GTK application via `SDL_WINDOWID`.

One thing that I worry about with this design is performance. Every time we
modify the timeline, we have to generate a new sections object and from that
generate a new MLT producer. That might take time. My hope and guess is that we
can do smart things to get good enough performance. But it is worth looking
into to quite soon to ensure that this design will hold even for larger
projects.

There for sure are many, many more details to flesh out before we have a
functioning video editor. But I'm quite pleased that we have gotten this far in
this quite short amount of time.

You can find the source code on
[GitHub](https://github.com/rickardlindberg/rlvideo).
