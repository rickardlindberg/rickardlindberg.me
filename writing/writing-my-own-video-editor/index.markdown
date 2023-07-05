---
title: 'DRAFT: Writing my own video editor'
date: 2023-07-05
tags: rlvideo,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

## TODO

16th of June:

<p>
<center>
![Initial sketch of a timeline.](sketch-initial.png)
</center>
</p>

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

3rd of July:

<p>
<center>
![Current look of application.](current-status.png)
</center>
</p>

* rlvideo

    * spike MLT june 28
    * Timeline.get_groups (first version)

May 28, 2023:

> Got the urge to write my own video editor. Tired of kdenlive's instability.
> And I don't need something that advanced. Reading a bit about the MLT
> framework makes me think that it might actually be possible to do in a
> reasonable time.
>
> Sometimes I feel bad for starting more projects than i finish. On the other
> hand, every project I do teach me something. And I do this (believe it or
> not) for my enjoyment.

* Can overlap clips. Only one visible track. Editor figures out what tracks to
  put things on in the background.

June 26, 2023:

* Found https://github.com/jliljebl/flowblade
    * Written in Python
    * Good reference

* Example Python:
  https://github.com/mltframework/mlt/blob/master/src/swig/python/play.py

* My hello world

    ```
    def hello1():
        mlt.Factory().init()
        profile = mlt.Profile()
        producer = mlt.Producer(profile, "VID_20230611_115932.mp4")
        consumer = mlt.Consumer(profile, "sdl")
        consumer.set("rescale", "none")
        consumer.connect(producer)
        consumer.start()
        while consumer.is_stopped() == 0:
            time.sleep(1)

    def hello2():
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

* Tutorial: https://www.mltframework.org/docs/framework/

* Wrap my head around basic primitives and how to map that to what I want
    * Use my structure for clips
    * Turn that into mlt primitives

* Embed SDL window (got idea from flowblade source code)

* How to use in app: https://www.mltframework.org/docs/opengl/

* I think I know enough. Next I want to explore how I would like to organize
  clips and work with a timeline.
