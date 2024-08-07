---
title: 'DevLog 010: Debugging MLT/GTK segfault'
date: 2023-08-03
tags: devlog,rlvideo,mlt
devlog: true
---

I try to edit some footage with my [video
editor](/projects/rlvideo/index.html). Actually, it is footage from [DevLog
009](/writing/devlog-009-improve-timeline-scrubbing/index.html) that I hope to
put together. Everything is going quite well. After I add a
split-cut-at-playhead operation to the editor, in addition to the previously
added ripple delete, I am actually able to do some useful edits.

However, after a while I notice that a cut does not seem to render the correct
frame. I decide to restart the application, and then it happens. Segfault!

This time, the segfault reproduces consistently. I'm excited to debug this and
see how we can resolve it. I've got my cup of coffee, and I'm ready to go.

<p>
<center>
![Coffee.](coffee.png)
</center>
</p>

## GDB output

Because this is not the first time I see segfaults in this application, I have
added a command to run the application in GDB. Here is how to use it:

```
$ ~/rlvideo/make.py gdb devlog-009.rlvideo
...
Starting program: /usr/bin/python3 /home/rick/rlvideo/rlvideo.py devlog-009.rlvideo
...
Thread 1 "python3" received signal SIGSEGV, Segmentation fault.
..
(gdb) bt
#0  0x00007ffff7a64474 in pthread_mutex_lock () at /lib64/libpthread.so.0
#1  0x00007fffe96866af in XrmQGetResource () at /lib64/libX11.so.6
#2  0x00007fffe9667fca in XGetDefault () at /lib64/libX11.so.6
#3  0x00007fffe9a5ae8a in _cairo_xlib_surface_get_font_options () at /lib64/libcairo.so.2
...
```

## Analysis and ideas

The segfault seems to happen inside some Cairo drawing code. That is most
likely happening because GTK is trying to show a widget that tries to draw
itself. I think GTK calls were further down in the backrace.

I find it very unlikely that this can happen from the Python GTK bindings. My
suspicion is that this has something to do with MLT. Why? Because the segfault
only happens for some projects.

I know that many MLT calls return status codes that I never check. Perhaps I
should.

There is also a way to serialize an MLT producer to an XML file which can then
be played with `melt`. That way we can see if MLT has the same problems as we
are having given the same MLT producer.

This might be useful for other types of debugging as well.

Let's see if we can implement that XML export and see if `melt` segfaults as
well or if that works.

## Debugging MLT producers

I add this to the main function:

```python
if sys.argv[1:2] == ["--export-melt"]:
    path = sys.argv[2]
    print(f"Exporting {path}")
    project = Project.load(args=sys.argv[3:])
    consumer = mlt.Consumer(project.get_preview_profile(), "xml")
    consumer.set("resource", path)
    consumer.connect(project.get_preview_mlt_producer())
    consumer.start()
    while consumer.is_stopped() == 0:
        time.sleep(0.5)
    print("Done")
    return
```

We can run it like this:

```
$ ./make.py rundev --export-melt test.xml
Exporting test.xml
Done
```

Then we can feed it to `melt` like this:

```
$ mlt-melt test.xml
```

When I do, I get this:

```
[producer_xml] parse fatal: Input is not proper UTF-8, indicate encoding !
Bytes: 0xC0 0xF3 0x68 0x0E
	row: 3	col: 25
[producer_xml] parse fatal: invalid character in attribute value
...
```

There seems to be an encoding issue. I look at the file and see that the
profile description looks weird.

I fix it manually, and then get this:

```
$ mlt-melt test.xml
+-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
|1=-10| |2= -5| |3= -2| |4= -1| |5=  0| |6=  1| |7=  2| |8=  5| |9= 10|
+-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+ +-----+
+---------------------------------------------------------------------+
|               H = back 1 minute,  L = forward 1 minute              |
|                 h = previous frame,  l = next frame                 |
|           g = start of clip, j = next clip, k = previous clip       |
|                0 = restart, q = quit, space = play                  |
+---------------------------------------------------------------------+
Segmentation fault (core dumped)
```

Hmm. Now I'm not using the project that I had problems with. Now I'm just using
the default test project which works fine otherwise.

When I look closer at the profile in the XML file, other things seem off as
well. The width and height don't seem to be correct either.  I try to use the
project profile instead of the preview profile in the XML export code. This
works better. However, the player only shows a couple of frames where there
should be more. What is going on?

Then I notice this at the end of the XML file:

```xml
  <playlist id="playlist0">
    <entry producer="playlist1" in="" out=""/>
    <entry producer="producer4" in="0" out="0"/>
  </playlist>
</mlt>
```

The first item in the playlist, which is another playlist, seems to lack in and
out arguments. If I change to the following, the file plays ok:

```xml
    <entry producer="playlist1" in="0" out="43"/>
```

I print in and out points for all playlists that we create, and they seem to
have valid numbers. Time to dig into the MLT XML export code.

I find this:

```c
char *mlt_properties_get_time(mlt_properties self, const char *name, mlt_time_format format)
{
    mlt_profile profile = mlt_properties_get_data(self, "_profile", NULL);
    if (profile) {
        double fps = mlt_profile_fps(profile);
        mlt_property value = mlt_properties_find(self, name);
        property_list *list = self->local;
        return value == NULL ? NULL : mlt_property_get_time(value, format, fps, list->locale);
    }
    return NULL;
}
```

The `mlt_properties_get_time` functions seems to be used in the XML export. And
it seems to work only if there is a profile.

My playlists don't have profiles.

I add it like this:

```diff
diff --git a/rlvideolib/domain/section.py b/rlvideolib/domain/section.py
index 4c50d6d..78a0683 100644
--- a/rlvideolib/domain/section.py
+++ b/rlvideolib/domain/section.py
@@ -33,7 +33,7 @@ class Sections:
         return canvas

     def to_mlt_producer(self, profile, cache):
-        playlist = mlt.Playlist()
+        playlist = mlt.Playlist(profile)
         for section in self.sections:
             playlist.append(section.to_mlt_producer(profile, cache))
         assert playlist.get_playtime() == self.length
@@ -71,7 +71,7 @@ class PlaylistSection:
         return canvas

     def to_mlt_producer(self, profile, cache):
-        playlist = mlt.Playlist()
+        playlist = mlt.Playlist(profile)
         for part in self.parts:
             part.add_to_mlt_playlist(profile, cache, playlist)
         assert playlist.get_playtime() == self.length
```

Now the export works fine!

Let's export the XML file for the project that segfaults.

I examine the XML file and notice the same problem for `mlt.Tractor`. It is
also missing in and out arguments. I add profiles to those as well.

```
$ ./make.py commit -m 'Pass profile to mlt.Tractor so that XML export works properly with in/out points.'
...........................................................
----------------------------------------------------------------------
Ran 59 tests in 3.024s

OK
[main a5db808] Pass profile to mlt.Tractor so that XML export works properly with in/out points.
 1 file changed, 1 insertion(+), 1 deletion(-)
```

The export works fine and it plays fine in the `melt` player.

I think that the fixes we made for the XML export only affects the XML export.
But it is nice that we now have the ability to play our projects with `melt`. I
suspect it might come in handy in the future as well.

So there doesn't seem to be anything wrong with the producer that we create.
Melt can play it just fine. That is good news, I guess, but what to do next?

## Weird cuts

I mentioned in the beginning that the reason that I restarted the application
was that I thought a cut rendered the wrong frame.

I see this problem when playing the XML file with melt as well.

This is most likely something wrong in our code. However, it doesn't seem to
contribute to the segfault.

I add a TODO in the code in a place where I think the problem is. Let's deal
with that later. We are on the hunt for segfault reasons now.

## More ideas

We have concluded that the producer that we create is probably fine.

My suspicion is that there is something in the combination of MLT and GTK that
causes the segfault. MLT and GTK are running in the same process, so it might
be possible that they interfere with each other somehow. The backtrace got
segfaulted inside the pthread library. So perhaps this is also timing related.

Let's try a few things out.

## Removing the player

The thing that connects MLT and GTK is the player. We start an MLT SDL consumer
and have it display it's output in a GTK window.

I try to remove the player like this:

```python
#mlt_player = MltPlayer(self.project, preview.get_window().get_xid())
class MockPlayer:
    def position(self):
        return 0
mlt_player = MockPlayer()
```

And now the application starts!

But of course it doesn't work properly.

However, it tells me that there is something about this combination that causes
the segfault.

## Timing

I then try this:

```diff
diff --git a/rlvideolib/gui/gtk.py b/rlvideolib/gui/gtk.py
index cb13bef..3feaf87 100644
--- a/rlvideolib/gui/gtk.py
+++ b/rlvideolib/gui/gtk.py
@@ -160,6 +160,9 @@ class MltPlayer:
         # TODO: figure out why SDL consumer seems to produce brighter images (black -> grey)
         self.project = project
         os.putenv("SDL_WINDOWID", str(window_id))
+        GLib.idle_add(self.init_player)
+
+    def init_player(self):
         self.consumer = mlt.Consumer(self.project.get_preview_profile(), "sdl")
         self.consumer.start()
         self.producer = None
```

That is, I create the MLT consumer a little later, once GTK has had time to
start up a bit more.

And wow, this actually works!

I though about this idea because I had come across this comment in the
[Flowblade](https://github.com/jliljebl/flowblade) source code:

```python
# SDL 2 consumer needs to created after Gtk.main() has run enough for window to be visible
#if editorstate.get_sdl_version() == editorstate.SDL_2: # needs more state consideration still
#    print "SDL2 timeout launch"
#    global sdl2_timeout_id
#    sdl2_timeout_id = GLib.timeout_add(1500, create_sdl_2_consumer)
```

The comment was for SDL2, and we are using SDL1, but I thought it was worth a
try anyway.

Here is one reason that I think it is valuable documenting my work. I was able
to get an idea from Flowblade. From a comment written in the source code. That
was valuable to me. Maybe others will find similar value in what I write about.
Maybe.

## Solution too soon?

I try the `idle_add` solution a couple of times, and it seems like I was too
fast to declare victory. It seems like it still segfaults sometimes.

Then I try to take the SDL consumer out of the picture by replacing it with
this:

```python
class DummyConsumer:

    def disconnect_all_producers(self):
        print("Dummy disconnect")

    def connect(self, producer):
        print("Dummy connect")
```

And it still segfaults sometimes.

## Delay all MLT operations

I'm thinking that we need to delay all MLT operations until GTK is properly
initialized.

I try to get this to work, but I don't manage. The code is too tangled
together.

Many hours pass, and I don't seem to be making any progress.

## Overlap

I'm thinking that this segfault might have to do with the bug I talked about in
the beginning about the wrong frame being rendered.

I find the problem in the code, write a test that exposes the bug, and then fix
it.

That was good, but it did not resolve the segfault.

I keep scratching my head, thinking of things to try. Hours pass. Then I have a
breakthrough.

## Breakthrough

But some lucky guess, I find out that the segfault only happens when we have
overlapping clips in our project. I decide to comment out transitions (the code
that merges multiple, overlapping frames together), and suddenly, the
reproducible segfault goes away. The problem seems to be with the `qtblend`
transition. There is another one called `frei0r.cairoblend` which works as well
for our purposes. I switch to that one and write this comment in the code about
it.

```python
# 'qtblend' that was first used first seems to give problems
# when used in a GTK context. The application segfaults when
# started.
#
# Steps to reproduce:
#
# 1. ./make.py rundev foo.rlvideo resources/*mp4
#
# 2. Move a cut so that there is a overlap somewhere
#
# 3. ./make.py rundev foo.rlvideo
#
# Boom! Stacktrace:
#
#     (gdb) bt
#     #0  0x00007ffff7a64474 in pthread_mutex_lock () at /lib64/libpthread.so.0
#     #1  0x00007fffe96866af in XrmQGetResource () at /lib64/libX11.so.6
#     #2  0x00007fffe9667fca in XGetDefault () at /lib64/libX11.so.6
#     #3  0x00007fffe9a5ae8a in _cairo_xlib_surface_get_font_options () at /lib64/libcairo.so.2
#     ...
#
# frei0r.cairoblend seems to work better.
#
# TODO: How to fix this problem? Is qtblend just incompatible?
```

## Summary

I am extremely satisfied that we found the reason for the segfault and were
able to fix it.

In the process we also found a couple of other bugs that we fixed and added the
XML export for easier debugging.

After fixing the segfault I continue to edit. Unfortunately, I get other
segfaults now. This time not reproducible, but more random. I conclude that I
must learn better the internals of MLT to figure out what I'm doing wrong in
the Python code. And after the things I learned from this session, I'm more
prepared.
