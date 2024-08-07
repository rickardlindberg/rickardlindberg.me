---
title: 'DevLog 006: Adding the concept of a clip'
date: 2023-07-31
tags: devlog,rlvideo
devlog: true
---

When working on the [video editor](/projects/rlvideo/index.html) and writing
about it, I keep talking about clips. But there is nothing called a clip in the
source code. Today I will explore the idea of adding that as a concept and see
what kind of functionality it will attract.

## Finishing FFmpeg proxy generation

Here is the cleaned up diff from yesterday's work on moving back to FFmpeg for
proxy generation:

```diff
@@ -70,15 +70,16 @@ class FileSource(namedtuple("FileSource", "id,path,number_of_frames_at_project_f
         proxy_tmp_path = proxy_spec.get_tmp_path(checksum)
         if not os.path.exists(proxy_path):
             proxy_spec.ensure_dir()
-            p = mlt.Profile()
-            p.from_producer(producer)
-            proxy_spec.adjust_profile(p)
-            producer = mlt.Producer(p, self.path)
-            consumer = mlt.Consumer(p, "avformat")
-            consumer.set("target", proxy_tmp_path)
-            proxy_spec.adjust_consumer(consumer)
-            run_consumer(consumer, producer, progress)
-            self.create_producer(profile, proxy_tmp_path)
+            subprocess.check_call([
+                "ffmpeg",
+                "-y",
+                "-i", self.path,
+                "-vf", "yadif,scale=960:540",
+                "-q:v", "3",
+                "-vcodec", "mjpeg",
+                "-acodec", "pcm_s16le",
+                proxy_tmp_path
+            ])
             os.rename(proxy_tmp_path, proxy_path)
         return self.create_producer(profile, proxy_path)
```

Let's commit that and see how we can refactor in this area.

```text
$ ./make.py commit -m 'Use FFmpeg for proxy generation.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.959s

OK
[main 078c9f2] Use FFmpeg for proxy generation.
 1 file changed, 10 insertions(+), 9 deletions(-)
```

## The start of Clip

The proxy generation code has this line:

```python
checksum = md5(self.path)
```

Where `md5` is a top level function.

Let's extract a `Clip` class and put the `md5` method there. My idea is that a
clip represents a file on disk that we can load into a `FileSource` and display
on our timeline.

Here it is:

```python
class Clip:

    def __init__(self, path):
        self.path = path

    def md5(self):
        return subprocess.check_output(["md5sum", self.path])[:32].decode("ascii")
```

We can see again that we use an external program, `md5sum`, to calculate the
md5. I think I did that because the Python module for calculating md5 did not
have a convenient function for calculating the sum on large files. As I noted
previously, I'm fine with this. We should probably add a test though to make
sure it works with the external program.

Anyway, the proxy code can now be written like this:

```python
checksum = Clip(self.path).md5()
```

Perfect! Time to commit:

```text
$ ./make.py commit -m 'Extract Clip and move the md5 function to it.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.966s

OK
[main 6488c05] Extract Clip and move the md5 function to it.
 3 files changed, 12 insertions(+), 4 deletions(-)
 create mode 100644 rlvideolib/domain/clip.py
```

## Revising mlthelpers

When working on proxies before, we extracted this module:

```python
rlvideolib.mlthelpers
```

First of all, its `run_consumer` function is no longer used when we generate
proxies with FFmpeg. Let's remove it.

```text
$ ./make.py commit -m 'Remove rlvideolib.mlthelpers.run_consumer since it is no longer used.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.974s

OK
[main aff41ef] Remove rlvideolib.mlthelpers.run_consumer since it is no longer used.
 2 files changed, 9 deletions(-)
```

The only thing left now is this class:

```python
class FileInfo:

    def __init__(self, path):
        self.path = path

    def get_number_of_frames(self, profile):
        return mlt.Producer(profile, self.path).get_playtime()
```

This looks a lot like it can be merged into our new `Clip`. Let's see where it
is used.

I find an unused import of it and remove it.

```text
$ ./make.py commit -m 'Remove unused import of FileInfo.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.967s

OK
[main dda14cf] Remove unused import of FileInfo.
 1 file changed, 1 deletion(-)
```

I wonder if there is a tool that can automatically remove unused imports. Then
we can include it in our test runner perhaps? Along with an auto formatter?

Anyway, the only other use is here:

```python
class Transaction:

    ...

    def add_clip(self, path, id=None):
        source = FileSource(
            id=id,
            path=path,
            number_of_frames_at_project_fps=FileInfo(
                path
            ).get_number_of_frames(self.project.profile)
        )
        return self.add_source(source, source.number_of_frames_at_project_fps)
```

Here we are actually talking about adding a clip. I had forgotten about that.
It would make more sense to create a `Clip` then instead of a `FileInfo`. Let's
add `get_number_of_frames` to `Clip` and then we can get rid of `FileInfo` and
`rlvideolib.mlthelpers` completely.

```text
$ ./make.py commit -m 'Move get_number_of_frames to Clip.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.955s

OK
[main c13f4cb] Move get_number_of_frames to Clip.
 4 files changed, 7 insertions(+), 14 deletions(-)
 delete mode 100644 rlvideolib/mlthelpers.py
```

## Thinking about number of frames

We saw in an [earlier
devlog](/writing/devlog-004-proxies-with-correct-fps/index.html) that the
number of frames that we store is actually the number of frames at the current
project FPS. I dig into the MLT source code and see that the out point seems to
be calculated based on the FPS:

```c
mlt_position frames = (mlt_position) lrint(format->duration * mlt_profile_fps(profile)
                                           / AV_TIME_BASE);
if (mlt_properties_get_position(properties, "out") <= 0)
    mlt_properties_set_position(properties, "out", frames - 1);
if (mlt_properties_get_position(properties, "length") <= 0)
    mlt_properties_set_position(properties, "length", frames);
```

I wonder if we can do the same calculation so that we don't have to depend on
MLT for `get_number_of_frames`?

I'm not ready for that. Let's rewrite

```python
def get_number_of_frames(self, profile):
    return mlt.Producer(profile, self.path).get_playtime()
```

to this

```python
def calculate_length_at_fps(self, mlt_profile):
    return mlt.Producer(mlt_profile, self.path).get_playtime()
```

to clarify a bit more what it is actually doing.

With this change, I think that the previous rename we did of
`FileSource.length` to `FileSource.number_of_frames_at_project_fps` can be
reverted. I think it makes sense to talk about a length in terms of frames. If
we change the project FPS, all lengths have to be recalculated, and I think
this makes sense. With the new `calculate_length_at_fps` it is still clear that
this length depends on the FPS I think.

Let's commit:

```text
$ ./make.py commit -m 'Rename Clip..get_number_of_frames to Clip..calculate_length_at_fps.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.457s

OK
[main 94de3c1] Rename Clip..get_number_of_frames to Clip..calculate_length_at_fps.
 2 files changed, 3 insertions(+), 3 deletions(-)
```

```text
$ ./make.py commit -m 'Rename FileSource.number_of_frames_at_project_fps to FileSource.length.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.952s

OK
[main 5bca102] Rename FileSource.number_of_frames_at_project_fps to FileSource.length.
 2 files changed, 12 insertions(+), 12 deletions(-)
```

Now when `Transaction.add_clip` reads like this

```python
def add_clip(self, path, id=None):
    source = FileSource(
        id=id,
        path=path,
        length=Clip(
            path
        ).calculate_length_at_fps(mlt_profile=self.project.profile)
    )
    return self.add_source(source, source.length)
```

I don't think we need this comment anymore:

```python
# NOTE: The length depends on the FPS of the project. Once the first
# FileSource is added to the project, the FPS of the project can not be
# changed.
```

I think the code above makes it clear enough that the length depends on the FPS
of the project.

```text
$ ./make.py commit -m 'Remove note about length/FPS since this is clear enough in the code now.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.955s

OK
[main ab10ec9] Remove note about length/FPS since this is clear enough in the code now.
 1 file changed, 4 deletions(-)
```

Sometimes I feel bad for changing the code back and forth. But I don't think I
should feel bad. I think it's a good thing to always make the code express our
intentions as well as possible. If that means that names change back and forth,
that's ok.

## Clip proxies

What more functionality can the clip class attract? How about proxy generation?

A proxy clip is an alternative version of a clip. An alternative file on disk
that represents the same thing but in a format that is easier to work with.

It makes sense.

Let's extract it:

```python
class Clip:

    ...

    def generate_proxy(self, proxy_spec, progress):
        # TODO: call progress
        checksum = self.md5()
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
        return proxy_path
```

And `FileSource.load_proxy` can be reduced to this:

```python
def load_proxy(self, profile, proxy_spec, progress):
    return self.create_producer(
        profile,
        Clip(self.path).generate_proxy(proxy_spec, progress)
    )
```

This reads quite nicely I think. Let's commit:

```text
$ ./make.py commit -m 'Extract Clip.generate_proxy.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.929s

OK
[main cc85022] Extract Clip.generate_proxy.
 2 files changed, 25 insertions(+), 18 deletions(-)
```

## Proxy spec location

The `ProxySpec` class currently lives in `rlvideolib.domain.project`. I think a
much better place would be `rlvideolib.domain.clip`.

Let's move it over.

```text
$ ./make.py commit -m 'Move ProxySpec to rlvideolib.domain.clip.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.903s

OK
[main 36932f0] Move ProxySpec to rlvideolib.domain.clip.
 2 files changed, 50 insertions(+), 49 deletions(-)
```

## Proxy spec cleanup

The proxy spec has a method for setting properties on a consumer:

```python
def adjust_consumer(self, consumer):
    consumer.set("vcodec", self.vcodec)
    consumer.set("acodec", self.acodec)
    consumer.set("qscale", self.qscale)
```

Now when we use FFmpeg for proxy generation, this is no longer used.

But we did hardcode some values for FFmpeg, let's change `adjust_consumer` to
`get_ffmpeg_arguments`:

```python
def get_ffmpeg_arguments(self):
    return [
        "-vf", f"yadif,scale=-1:{self.height}",
        "-q:v", self.qscale,
        "-vcodec", self.vcodec,
        "-acodec", self.acodec,
    ]
```

And use it like this:

```python
def generate_proxy(self, proxy_spec, progress):

    ...

    subprocess.check_call(
        [
            "ffmpeg",
            "-y",
            "-i", self.path,
        ]
        +
        proxy_spec.get_ffmpeg_arguments()
        +
        [
            proxy_tmp_path
        ]
    )
```

Commit:

```text
$ ./make.py commit -m 'ProxySpec know how to generate FFmpeg arguments for conversion.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.412s

OK
[main 6579178] ProxySpec know how to generate FFmpeg arguments for conversion.
 1 file changed, 20 insertions(+), 14 deletions(-)
```

## Summary

I am quite happy with the refactoring that we did in this session. We finally
have a concept of a clip in the source code and it turned out that it attracted
a bit of functionality that previously was spread across the code.

The code base is now a little cleaner.

Why is that important?

Because clear thinking is clear writing and vice versa. It applies to code as
well. If we have expressed our thinking clear in code, thinking about it is
easier, and so making the next change is easier. And to think clearly about it,
we need to write about it, refactor it, until it expresses our ideas clearly.
It's a circle that probably never ends but perhaps slows down as our ideas and
the expression of them in code approach each other.
