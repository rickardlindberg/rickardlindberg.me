---
title: 'DevLog 005: MLT proxy hell'
date: 2023-07-31
tags: devlog,rlvideo,mlt
devlog: true
---

I want to use the [video editor](/projects/rlvideo/index.html) to edit footage
that I have shot this summer. It starts out well, gives me a lot of problems,
and resolves in the end.

## A promising start

To load all clips that I have, I try this command:

```
$ rlvideo a6400/* hero8/*
```

It takes a while to load all the clips. This is expected. When we load a clip
we need to figure out its length so that we can correctly place it on the
timeline. This is a one time cost when adding new clips. And I have shot many
clips this summer:

```
$ ls a6400/* hero8/* | wc -l
270
```

I patiently wait.

After a while the GUI pops up and proxy clips start to render in the
background. Meanwhile the GUI is quite snappy and we can start to make edits
right away.

Aside from the lack of progress bar when loading clips, the application works
as intended.

I figure it will take a while to render all proxy clips, so I leave it open and
go do something else for a while.

<p>
<center>
![Loading clips.](loading.png)
</center>
</p>

## Crashes

Then I hear that the fan stops making noises. Already done I think? Hmm. Where
did my application go? The window is closed, and so is the terminal from which
I opened it. How is that even possible? Reading the `dmesg` output, the
application seems to have segfaulted.

I spend many hours trying to figure out what is going on. What is particularly
annoying is that you have to wait a long time to reproduce it. The segfault
does not happen right away.

Eventually I narrow down the problem to proxy generation. At least I think so.
If I comment out generation of proxy clips, I can load many clips without a
crash.

In an earlier version of the program, we generated proxy clips using FFmpeg.
Then we switched over to using MLT. I got the idea that you can do it with
MLT from [Flowblade](http://jliljebl.github.io/flowblade/). It also made it
easier to show progress in the GUI.

When we made the switch, I noticed that something happened to the colors of the
proxy clips. They seemed to look a little bleaker than the original. I don't
recall having this problem when we generated proxies using FFmpeg.

Odd looking colors and segfaults. I think it's time to go back to generating
proxy clips using FFmpeg.

## Proxies with FFmpeg

Here is what I'm trying right now.

Instead of this:

```python
def load_proxy(self, profile, proxy_spec, progress):
    producer = self.create_producer(profile, self.path)
    checksum = md5(self.path)
    proxy_path = proxy_spec.get_path(checksum)
    proxy_tmp_path = proxy_spec.get_tmp_path(checksum)
    if not os.path.exists(proxy_path):
        proxy_spec.ensure_dir()
        p = mlt.Profile()
        p.from_producer(producer)
        proxy_spec.adjust_profile(p)
        producer = mlt.Producer(p, self.path)
        consumer = mlt.Consumer(p, "avformat")
        consumer.set("target", proxy_tmp_path)
        proxy_spec.adjust_consumer(consumer)
        run_consumer(consumer, producer, progress)
        self.create_producer(profile, proxy_tmp_path)
        os.rename(proxy_tmp_path, proxy_path)
    return self.create_producer(profile, proxy_path)
```

We do this:

```python
def load_proxy(self, profile, proxy_spec, progress):
    producer = self.create_producer(profile, self.path)
    checksum = md5(self.path)
    proxy_path = proxy_spec.get_path(checksum)
    proxy_tmp_path = proxy_spec.get_tmp_path(checksum)
    if not os.path.exists(proxy_path):
        proxy_spec.ensure_dir()
        subprocess.check_call([
            "ffmpeg",
            "-y",
            "-i", self.path,
            "-vf", "yadif,scale=960:540",
            "-q:v", "3",
            "-vcodec", "mjpeg",
            "-acodec", "pcm_s16le",
            proxy_tmp_path
        ])
        os.rename(proxy_tmp_path, proxy_path)
    return self.create_producer(profile, proxy_path)
```

We can can let the `proxy_spec` set the FFmpeg options. The above is just an
experiment to see if this works better.

So far, it looks promising. It has been working for a couple of hours and the
proxy clips look better than before.

I'm gonna make some dinner and we'll see the status later on.

## After dinner

All proxy clips rendered successfully. The editor is still alive and I can
scrub all the clips. Nice!

## Summary

Generating proxies using MLT always felt a little awkward. All we want to do is
to scale the clip, encode it using a seek-friendly format, and leave everything
else as is. With MLT we had to fiddle around with different profiles to make
sure FPS was preserved and recreate producers with different profiles. It never
felt like the proper solution.

Doing the conversion using FFmpeg is much more straight forward. There are two
objections that I can have to that solution:

1. It calls an external process
2. We lost the call to `progress`

I browsed the web for solutions to the progress problem. And there seems to be
many solutions for that. We can probably figure out one that works for us. And
to be honest, right now, progress in the GUI is not the most important thing.
Right now, FFmpeg outputs some statistics to the terminal, so we could have a
look there for some kind of progress.

When it comes to calling external processes, I'm not sure what I think. I know
I don't have a problem with it. Why do I object then? Honestly, I don't know.
Something tells me that it is a little ugly.

When I started this project, I thought it would only be possible to do with the
help of MLT. If the Python MLT bindings keep giving me segfaults and other hard
times, will the project fail? Maybe. So what is my strategy? I think I will try
to isolate the MLT code as much as possible and use other tools where possible
(FFmpeg for proxy generation for example). If I manage to isolate the core of a
video editor that is not depending on MLT, then perhaps I can also make it work
with another video library if there is one. It will be an interesting exercise.
