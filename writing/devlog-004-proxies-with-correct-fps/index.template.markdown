---
title: 'DevLog 004: Proxies with correct FPS'
date: 2023-07-30
tags: devlog,rlvideo,mlt
devlog: true
---

In this episode we will continue work on the [video
editor](/projects/rlvideo/index.html). I have some footage that I would like to
edit. Wouldn't it be cool if I can do that in my own video editor? I will use
that as a guide for the development. What is stopping me from using my video
editor today? Fix that and move on to the next thing.

In this episode we will fix an issue with proxy clips sometimes having the
incorrect FPS.

## Why DevLogs?

I do them for various reasons. Here are the ones that I can think of now.

* I think there is value in documenting the work that I do.

* People reading these DevLogs might pick up something that I do and
  incorporate into their workflow.

* Clear thinking is clear writing and vice versa. Writing helps me think more
  clearly about topics. Sometimes, by writing about a problem, I think I can
  reach a solution faster even though writing takes time.

* I want to practice writing.

## The problem with proxies and FPS

For most of my videos, I use a frame rate of 25. That is 25 frames per second
(FPS). I have coded that as a default in the video editor.

However, sometimes I shoot footage in a higher frame rate and slow it down in
post.

Here is an example:

$:output:shell:
$ ffprobe GX010802.MP4 2>&1 | grep fps
  Stream #0:0(eng): Video: hevc (Main) (hvc1 / 0x31637668), yuvj420p(pc, bt709), 2704x1520 [SAR 1:1 DAR 169:95], 97187 kb/s, 100 fps, 100 tbr, 90k tbn, 100 tbc (default)
$:END

You can see there in the middle that is says `100 fps`.

When I drop this clip on a 25 FPS timeline, only every 4th frame will be used
from that clip and the rest are discarded. However, if I slow down the clip to
25% speed, the runtime will be 4 times longer and all the frames will be used.

The problem is that the video editor uses proxy clips for preview. A proxy clip
is typically a lower resolution version of the original clip to allow real time
editing on a slower computer.

And proxy clips are currently generated with the same frame rate as the project
(which is 25).

Here is what `ffprobe` says about our proxy clip:

$:output:shell:
$ ffprobe /tmp/de63dcd626503cbde6f3da76b0af3e8c.mkv 2>&1 | grep fps
  Stream #0:0: Video: mjpeg (Baseline), yuvj420p(pc, bt470bg/bt709/bt709), 960x540 [SAR 1:1 DAR 16:9], 25 fps, 25 tbr, 1k tbn, 1k tbc (default)
$:END

## Proxy generation today

With some details removed, here is how proxies are generated today:

$:output:python:
producer = mlt.Producer(profile, CLIP_PATH)

consumer = mlt.Consumer(proxy_profile, "avformat")
consumer.set("target", PROXY_PATH)
consumer.set("vcodec", "mjpeg")
consumer.set("acodec", "pcm_s16le")
consumer.set("qscale", "3")

consumer.connect(producer)
consumer.start()
while consumer.is_stopped() == 0:
    time.sleep(0.5)
$:END

The `profile` and the `proxy_profile` differ only in that the `proxy_profile`
has a lower resolution (width x height). They are otherwise identical.

What we need to do is to get the profile for a consumer and only change the
size of it. We want the profile FPS to be the FPS of the clip.

## Sidetracked

As I start writing some code, I notice something odd in the output of the
tests:

$:output:text:
.............................[matroska,webm @ 0x5599e1354fc0] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the 'analyzeduration' (0) and 'probesize' (5000000) options
[matroska,webm @ 0x5599e0f51540] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the 'analyzeduration' (0) and 'probesize' (5000000) options
....................
----------------------------------------------------------------------
Ran 49 tests in 2.001s
$:END

I `git stash` my current changes and see that the output is still there.

I increase the verbosity of the test runner to figure out which test is causing
the output and I get this:

$:output:text:
Doctest: rlvideolib.domain.project.Project ... [matroska,webm @ 0x56544c299700] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the 'analyzeduration' (0) and 'probesize' (5000000) options
[matroska,webm @ 0x56544c4061c0] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the 'analyzeduration' (0) and 'probesize' (5000000) options
$:END

The problem is that there is a sort of integration test that, when run,
generates proxy clips, and the output is not captured in the test and instead
redirected to the terminal.

I add the `capture_stdout_stderr` helper in the test, and the output now looks
clean.

$:output:python:
"""
>>> with capture_stdout_stderr():
...     with project.new_transaction() as transaction:
...         ...
"""
$:END

Let's commit:

$:output:text:
$ ./make.py commit -m 'Capture stdout/stderr in Project test to not clutter the test output.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.990s

OK
[main 9846016] Capture stdout/stderr in Project test to not clutter the test output.
 1 file changed, 4 insertions(+), 3 deletions(-)
$:END

Sometimes when I encounter a small problem when working on something, I prefer
to `git stash` my changes, fix the small problem, and then get back to what I
was working on with `git stash pop`.

If the problem turns out to be not so small, I might write a note about it
instead.

## Back to the problem

I create this function to get a native producer and profile:

$:output:python:
def mlt_producer_with_native_profile(path):
    """
    >>> _ = mlt.Factory().init()
    >>> producer, profile = mlt_producer_with_native_profile("resources/one.mp4")
    >>> profile.fps()
    25.0
    """
    profile = mlt.Profile()
    producer = mlt.Producer(profile, path)
    profile.from_producer(producer)
    # Re-open the producer with the new profile to ensure it gets all the
    # properties from it and does not retain properties from the old profile.
    producer = mlt.Producer(profile, path)
    return (producer, profile)
$:END

I don't have any clips in the resources folder that are other than 25 FPS, but
this at leas shows that my code doesn't crash.

I try to use it when generating proxies. The tests pass after my modification,
so I try to run the application with my 100 FPS test clip and get this:

$:output:text:
$ rm /tmp/*.mkv; rlvideo GX010802.MP4
...
  File "/home/rick/rlvideo/rlvideolib/domain/source.py", line 59, in load_proxy
    assert self.length == native_producer.get_playtime()
$:END

This reveals a problem to me. In the Python structures for a `Source` we store
its length. My intention was to store the number of frames in the file so that
we can check that we make valid cuts:

$:output:python:
class FileSource(namedtuple("FileSource", "id,path,length")):

    ...

    def create_cut(self, start, end):
        if start < 0 or end > self.length:
            raise ValueError("Invalid cut.")
        ...
$:END

But I think the `producer.get_playtime()` is not giving frames, but rather
frames at the current frame rate. A quick test confirms that this is the case:

$:output:python:
"""
>>> profile = mlt.Profile()
>>> profile.fps()
25.0
>>> producer = mlt.Producer(profile, "resources/one.mp4")
>>> producer.get_playtime()
15

>>> profile.set_frame_rate(50, 1)
>>> profile.fps()
50.0
>>> producer = mlt.Producer(profile, "resources/one.mp4")
>>> producer.get_playtime()
31
"""
$:END

What to do?

I think it's time for another `git stash` and clarify length.

## Clarify length

I want to rename `FileSource.length` to
`FileSource.number_of_frames_at_project_fps`. That is a really long name, but
it is more clear about what it represents. I value that more now.  After the
refactoring, I might uncover other issues.  Let's see.

$:output:text:
$ ./make.py commit -m 'Rename FileSource.length to FileSource.number_of_frames_at_project_fps.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.994s

OK
[main 8baae0a] Rename FileSource.length to FileSource.number_of_frames_at_project_fps.
 2 files changed, 10 insertions(+), 10 deletions(-)
$:END

The parameter is used in only one place outside `FileSource`:

$:output:python:
class Transaction:

    ...

    def add_clip(self, path, id=None):
        producer = mlt.Producer(self.project.profile, path)
        source = FileSource(id=id, path=path, number_of_frames_at_project_fps=producer.get_playtime())
        return self.add_source(source, source.number_of_frames_at_project_fps)
$:END

I find it a little unclear the connection between a producer, its playtime, and
the number of frames. Let's see if we can make a helper to clarify this:

$:output:python:
def add_clip(self, path, id=None):
    source = FileSource(
        id=id,
        path=path,
        number_of_frames_at_project_fps=FileInfo(
            path
        ).get_number_of_frames(self.project.profile)
    )
    return self.add_source(source, source.number_of_frames_at_project_fps)
$:END

And here is `FileInfo`:

$:output:python:
class FileInfo:

    def __init__(self, path):
        self.path = path

    def get_number_of_frames(self, profile):
        return mlt.Producer(profile, self.path).get_playtime()
$:END

This makes it a little more clear that the number of frames in a file depends
on the profile.

$:output:text:
$ ./make.py commit -m 'Extract FileInfo.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.982s

OK
[main fd89715] Extract FileInfo.
 1 file changed, 15 insertions(+), 2 deletions(-)
$:END

## Change project FPS after?

This brings up the question if we can change the project frame rate after we
have added some clips.

My guess is not.

I remember reading that you should never do this in Kdenlive. Then weird things
will happen.

I suppose we could try to re-calculate all positions and lengths when we change
the frame rate.  Or have the unit of measurement be time instead. But I think
that will be hard since that is not what MLT works with, and also, it make
sense to work in terms of frames.

I add a note in the source code about this and move on. This is probably fine.

## Back

I `git stash pop` my earlier changes. Because of the rename, I have to resolve
conflicts, but it goes well.

I then spot this:

$:output:python:
native_producer, native_profile = mlt_producer_with_native_profile(self.path)
assert self.number_of_frames_at_project_fps == native_producer.get_playtime()
$:END

With our new knowledge, this is obviously wrong. And the new name helps us see
that. The native profile has the FPS of the clip whereas the project profile
has the FPS of the project. Those might not be the same, so therefore the
assertion is not always going to work.

I see some more usages for `FileInfo`, so I yet again stash my changes and
update `FileInfo` to this:

$:output:python:
class FileInfo:

    def __init__(self, path):
        self.path = path

    def get_number_of_frames(self, profile):
        return self.get_mlt_producer(profile).get_playtime()

    def get_mlt_producer(self, profile):
        return mlt.Producer(profile, self.path)
$:END

Commit:

$:output:shell:
$ ./make.py commit -m 'Allow clearer code by extending FileInfo.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.996s

OK
[main acb6702] Allow clearer code by extending FileInfo.
 4 files changed, 18 insertions(+), 11 deletions(-)
 create mode 100644 rlvideolib/mlthelpers.py
$:END

## Break

I'm having a hard time reasoning about proxy generation code. I decide it's
time for a break.

## Another strategy

So far we have made no actual progress on improving proxy generation, but we
have cleaned up the code in related areas and gained some new knowledge.

Since I was not able to fix the proxy generation in a small step, I decide to
change approach and take much smaller steps.

Let's see if we can refactor the `load_proxy` method and perhaps we might see
more clearly how to modify it.

For reference, this is what it looks now:

$:output:python:
def load_proxy(self, profile, proxy_profile, progress):
    producer = mlt.Producer(profile, self.path)
    assert self.number_of_frames_at_project_fps == producer.get_playtime()
    chechsum = md5(self.path)
    proxy_path = f"/tmp/{chechsum}.mkv"
    proxy_tmp_path = f"/tmp/{chechsum}.tmp.mkv"
    if not os.path.exists(proxy_path):
        consumer = mlt.Consumer(proxy_profile, "avformat")
        consumer.set("target", proxy_tmp_path)
        consumer.set("vcodec", "mjpeg")
        consumer.set("acodec", "pcm_s16le")
        consumer.set("qscale", "3")
        consumer.connect(producer)
        consumer.start()
        while consumer.is_stopped() == 0:
            progress(producer.position()/producer.get_playtime())
            time.sleep(0.5)
        os.rename(proxy_tmp_path, proxy_path)
    producer = mlt.Producer(profile, proxy_path)
    assert self.number_of_frames_at_project_fps == producer.get_playtime()
    return producer
$:END

Let's try to extract `get_file_info` (which can then also be used in another
place):

$:output:python:
def get_file_info(self, profile):
    file_info = FileInfo(self.path)
    assert self.number_of_frames_at_project_fps == file_info.get_number_of_frames(profile)
    return file_info
$:END

In `load_proxy` we can use it like this:

$:output:diff:
-        producer = mlt.Producer(profile, self.path)
-        assert self.number_of_frames_at_project_fps == producer.get_playtime()
+        file_info = self.get_file_info(profile)
         chechsum = md5(self.path)
         proxy_path = f"/tmp/{chechsum}.mkv"
         proxy_tmp_path = f"/tmp/{chechsum}.tmp.mkv"
         if not os.path.exists(proxy_path):
+            producer = file_info.get_mlt_producer(profile)
             consumer = mlt.Consumer(proxy_profile, "avformat")
             consumer.set("target", proxy_tmp_path)
             consumer.set("vcodec", "mjpeg")
$:END

This ensures that the file has the same length as we have recorded.

Let's extract `run_consumer`:

$:output:python:
def run_consumer(consumer, producer, progress):
    consumer.connect(producer)
    consumer.start()
    while consumer.is_stopped() == 0:
        progress(producer.position()/producer.get_playtime())
        time.sleep(0.5)
$:END

I forgot to commit last refactoring. Let's do that now:

$:output:text:
$ ./make.py commit -m 'Extract get_file_info and run_consumer.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.983s

OK
[main 49df31e] Extract get_file_info and run_consumer.
 2 files changed, 20 insertions(+), 11 deletions(-)
$:END

There is a test for proxy generation, but it does not run fully if the proxy
file already exists. I add a testing flag that we can set to True in tests. I'm
not sure I like this, but it will help us when refactoring this method:

$:output:python:
def load_proxy(self, profile, proxy_profile, progress, testing=False):
    ...
    proxy_tmp_path = f"/tmp/{chechsum}.tmp.mkv"
    if not os.path.exists(proxy_path) or testing:
        producer = file_info.get_mlt_producer(profile)
        ...
$:END

## Break

I keep trying to clean up the proxy loading code but I just can't seem to find
the right abstractions. Furthermore, I get segfaults and all kinds of strange
behavior from MLT. This demotivates me. I force myself to take a break.

## Revert

Since MLT is giving me all kinds of weird behavior, I think that perhaps the
`get_file_info` abstraction was wrong. Maybe it creates more trouble at the
moment. Let's see if we can inline some of it instead.

We get this:

$:output:diff:
@@ -65,12 +65,11 @@ class FileSource(namedtuple("FileSource", "id,path,number_of_frames_at_project_f
         """
         # TODO: generate proxy with same profile as source clip (same colorspace, etc,
         # but with smaller size)
-        file_info = self.get_file_info(profile)
+        producer = self.validate_producer(mlt.Producer(profile, self.path))
         chechsum = md5(self.path)
         proxy_path = f"/tmp/{chechsum}.mkv"
         proxy_tmp_path = f"/tmp/{chechsum}.tmp.mkv"
         if not os.path.exists(proxy_path) or testing:
-            producer = file_info.get_mlt_producer(profile)
             consumer = mlt.Consumer(proxy_profile, "avformat")
             consumer.set("target", proxy_tmp_path)
             consumer.set("vcodec", "mjpeg")
@@ -78,14 +77,11 @@ class FileSource(namedtuple("FileSource", "id,path,number_of_frames_at_project_f
             consumer.set("qscale", "3")
             run_consumer(consumer, producer, progress)
             os.rename(proxy_tmp_path, proxy_path)
-        producer = mlt.Producer(profile, proxy_path)
-        assert self.number_of_frames_at_project_fps == producer.get_playtime()
-        return producer
+        return self.validate_producer(mlt.Producer(profile, proxy_path))
$:END

Where `validate_producer` is this:

$:output:python:
def validate_producer(self, producer):
    assert producer.get_playtime() == self.number_of_frames_at_project_fps
    return producer
$:END

Commit:

$:output:text:
$ ./make.py commit -m 'Inline some of FileInfo.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 2.493s

OK
[main c47ea68] Inline some of FileInfo.
 2 files changed, 7 insertions(+), 14 deletions(-)
$:END

Then finally I can make this relatively small change:

$:output:diff:
@@ -70,7 +70,12 @@ class FileSource(namedtuple("FileSource", "id,path,number_of_frames_at_project_f
         proxy_path = f"/tmp/{chechsum}.mkv"
         proxy_tmp_path = f"/tmp/{chechsum}.tmp.mkv"
         if not os.path.exists(proxy_path) or testing:
-            consumer = mlt.Consumer(proxy_profile, "avformat")
+            p = mlt.Profile()
+            p.from_producer(producer)
+            p.set_width(proxy_profile.width())
+            p.set_height(proxy_profile.height())
+            producer = mlt.Producer(p, self.path)
+            consumer = mlt.Consumer(p, "avformat")
             consumer.set("target", proxy_tmp_path)
             consumer.set("vcodec", "mjpeg")
             consumer.set("acodec", "pcm_s16le")
$:END

With this, proxy clips now retain their FPS:

$:output:text:
$ ffprobe /tmp/de63dcd626503cbde6f3da76b0af3e8c.mkv 2>&1 | grep fps
  Stream #0:0: Video: mjpeg (Baseline), yuvj420p(pc, bt470bg/bt709/bt709), 960x540 [SAR 1:1 DAR 16:9], 100 fps, 100 tbr, 1k tbn, 1k tbc (default)
$:END

It seems to work fine in the application as well.

Let's commit this:

$:output:text:
$ ./make.py commit -m 'Produce proxy clips with native profile to preserve FPS.'
.................................................
----------------------------------------------------------------------
Ran 49 tests in 2.560s

OK
[main b69cfb7] Produce proxy clips with native profile to preserve FPS.
 1 file changed, 6 insertions(+), 3 deletions(-)
$:END

## Summary

This session turned out to be rather painful. Every time I do something that
involves MLT, things get painful. That tells med to isolate as much of the MLT
code as possible. It also tells me that I need to learn MLT better to
understand issues.

But segfaults worry me a bit. When working in Python, we should really not be
getting segfaults. Is there something wrong in the Python binding for MLT?

I'm sure we have to revisit proxy generation at some point. But I'm done for
now.
