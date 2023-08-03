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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ~/rlvideo/make.py gdb devlog-009.rlvideo
...
Starting program: /usr/bin/python3 /home/rick/rlvideo/rlvideo.py devlog-009.rlvideo
...
Thread 1 &quot;python3&quot; received signal SIGSEGV, Segmentation fault.
..
(gdb) bt
#0  0x00007ffff7a64474 in pthread_mutex_lock () at /lib64/libpthread.so.0
#1  0x00007fffe96866af in XrmQGetResource () at /lib64/libX11.so.6
#2  0x00007fffe9667fca in XGetDefault () at /lib64/libX11.so.6
#3  0x00007fffe9a5ae8a in _cairo_xlib_surface_get_font_options () at /lib64/libcairo.so.2
...
</pre></div>
</div></div>
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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">if</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:</span><span class="mi">2</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;--export-melt&quot;</span><span class="p">]:</span>
    <span class="n">path</span> <span class="o">=</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">2</span><span class="p">]</span>
    <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;Exporting </span><span class="si">{</span><span class="n">path</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">)</span>
    <span class="n">project</span> <span class="o">=</span> <span class="n">Project</span><span class="o">.</span><span class="n">load</span><span class="p">(</span><span class="n">args</span><span class="o">=</span><span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">3</span><span class="p">:])</span>
    <span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="n">project</span><span class="o">.</span><span class="n">get_preview_profile</span><span class="p">(),</span> <span class="s2">&quot;xml&quot;</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;resource&quot;</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">project</span><span class="o">.</span><span class="n">get_preview_mlt_producer</span><span class="p">())</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
    <span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
        <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;Done&quot;</span><span class="p">)</span>
    <span class="k">return</span>
</pre></div>
</div></div>
We can run it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py rundev --export-melt test.xml
Exporting test.xml
Done
</pre></div>
</div></div>
Then we can feed it to `melt` like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ mlt-melt test.xml
</pre></div>
</div></div>
When I do, I get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>[producer_xml] parse fatal: Input is not proper UTF-8, indicate encoding !
Bytes: 0xC0 0xF3 0x68 0x0E
	row: 3	col: 25
[producer_xml] parse fatal: invalid character in attribute value
...
</pre></div>
</div></div>
There seems to be an encoding issue. I look at the file and see that the
profile description looks weird.

I fix it manually, and then get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ mlt-melt test.xml
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
</pre></div>
</div></div>
Hmm. Now I'm not using the project that I had problems with. Now I'm just using
the default test project which works fine otherwise.

When I look closer at the profile in the XML file, other things seem off as
well. The width and height don't seem to be correct either.  I try to use the
project profile instead of the preview profile in the XML export code. This
works better. However, the player only shows a couple of frames where there
should be more. What is going on?

Then I notice this at the end of the XML file:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>  <span class="nt">&lt;playlist</span> <span class="na">id=</span><span class="s">&quot;playlist0&quot;</span><span class="nt">&gt;</span>
    <span class="nt">&lt;entry</span> <span class="na">producer=</span><span class="s">&quot;playlist1&quot;</span> <span class="na">in=</span><span class="s">&quot;&quot;</span> <span class="na">out=</span><span class="s">&quot;&quot;</span><span class="nt">/&gt;</span>
    <span class="nt">&lt;entry</span> <span class="na">producer=</span><span class="s">&quot;producer4&quot;</span> <span class="na">in=</span><span class="s">&quot;0&quot;</span> <span class="na">out=</span><span class="s">&quot;0&quot;</span><span class="nt">/&gt;</span>
  <span class="nt">&lt;/playlist&gt;</span>
<span class="nt">&lt;/mlt&gt;</span>
</pre></div>
</div></div>
The first item in the playlist, which is another playlist, seems to lack in and
out arguments. If I change to the following, the file plays ok:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>    <span class="nt">&lt;entry</span> <span class="na">producer=</span><span class="s">&quot;playlist1&quot;</span> <span class="na">in=</span><span class="s">&quot;0&quot;</span> <span class="na">out=</span><span class="s">&quot;43&quot;</span><span class="nt">/&gt;</span>
</pre></div>
</div></div>
I print in and out points for all playlists that we create, and they seem to
have valid numbers. Time to dig into the MLT XML export code.

I find this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kt">char</span> <span class="o">*</span><span class="nf">mlt_properties_get_time</span><span class="p">(</span><span class="n">mlt_properties</span> <span class="n">self</span><span class="p">,</span> <span class="k">const</span> <span class="kt">char</span> <span class="o">*</span><span class="n">name</span><span class="p">,</span> <span class="n">mlt_time_format</span> <span class="n">format</span><span class="p">)</span>
<span class="p">{</span>
    <span class="n">mlt_profile</span> <span class="n">profile</span> <span class="o">=</span> <span class="n">mlt_properties_get_data</span><span class="p">(</span><span class="n">self</span><span class="p">,</span> <span class="s">&quot;_profile&quot;</span><span class="p">,</span> <span class="nb">NULL</span><span class="p">);</span>
    <span class="k">if</span> <span class="p">(</span><span class="n">profile</span><span class="p">)</span> <span class="p">{</span>
        <span class="kt">double</span> <span class="n">fps</span> <span class="o">=</span> <span class="n">mlt_profile_fps</span><span class="p">(</span><span class="n">profile</span><span class="p">);</span>
        <span class="n">mlt_property</span> <span class="n">value</span> <span class="o">=</span> <span class="n">mlt_properties_find</span><span class="p">(</span><span class="n">self</span><span class="p">,</span> <span class="n">name</span><span class="p">);</span>
        <span class="n">property_list</span> <span class="o">*</span><span class="n">list</span> <span class="o">=</span> <span class="n">self</span><span class="o">-&gt;</span><span class="n">local</span><span class="p">;</span>
        <span class="k">return</span> <span class="n">value</span> <span class="o">==</span> <span class="nb">NULL</span> <span class="o">?</span> <span class="nb">NULL</span> <span class="o">:</span> <span class="n">mlt_property_get_time</span><span class="p">(</span><span class="n">value</span><span class="p">,</span> <span class="n">format</span><span class="p">,</span> <span class="n">fps</span><span class="p">,</span> <span class="n">list</span><span class="o">-&gt;</span><span class="n">locale</span><span class="p">);</span>
    <span class="p">}</span>
    <span class="k">return</span> <span class="nb">NULL</span><span class="p">;</span>
<span class="p">}</span>
</pre></div>
</div></div>
The `mlt_properties_get_time` functions seems to be used in the XML export. And
it seems to work only if there is a profile.

My playlists don't have profiles.

I add it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gh">diff --git a/rlvideolib/domain/section.py b/rlvideolib/domain/section.py</span>
<span class="gh">index 4c50d6d..78a0683 100644</span>
<span class="gd">--- a/rlvideolib/domain/section.py</span>
<span class="gi">+++ b/rlvideolib/domain/section.py</span>
<span class="gu">@@ -33,7 +33,7 @@ class Sections:</span>
         return canvas

     def to_mlt_producer(self, profile, cache):
<span class="gd">-        playlist = mlt.Playlist()</span>
<span class="gi">+        playlist = mlt.Playlist(profile)</span>
         for section in self.sections:
             playlist.append(section.to_mlt_producer(profile, cache))
         assert playlist.get_playtime() == self.length
<span class="gu">@@ -71,7 +71,7 @@ class PlaylistSection:</span>
         return canvas

     def to_mlt_producer(self, profile, cache):
<span class="gd">-        playlist = mlt.Playlist()</span>
<span class="gi">+        playlist = mlt.Playlist(profile)</span>
         for part in self.parts:
             part.add_to_mlt_playlist(profile, cache, playlist)
         assert playlist.get_playtime() == self.length
</pre></div>
</div></div>
Now the export works fine!

Let's export the XML file for the project that segfaults.

I examine the XML file and notice the same problem for `mlt.Tractor`. It is
also missing in and out arguments. I add profiles to those as well.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Pass profile to mlt.Tractor so that XML export works properly with in/out points.&#39;
...........................................................
----------------------------------------------------------------------
Ran 59 tests in 3.024s

OK
[main a5db808] Pass profile to mlt.Tractor so that XML export works properly with in/out points.
 1 file changed, 1 insertion(+), 1 deletion(-)
</pre></div>
</div></div>
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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1">#mlt_player = MltPlayer(self.project, preview.get_window().get_xid())</span>
<span class="k">class</span> <span class="nc">MockPlayer</span><span class="p">:</span>
    <span class="k">def</span> <span class="nf">position</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="mi">0</span>
<span class="n">mlt_player</span> <span class="o">=</span> <span class="n">MockPlayer</span><span class="p">()</span>
</pre></div>
</div></div>
And now the application starts!

But of course it doesn't work properly.

However, it tells me that there is something about this combination that causes
the segfault.

## Timing

I then try this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gh">diff --git a/rlvideolib/gui/gtk.py b/rlvideolib/gui/gtk.py</span>
<span class="gh">index cb13bef..3feaf87 100644</span>
<span class="gd">--- a/rlvideolib/gui/gtk.py</span>
<span class="gi">+++ b/rlvideolib/gui/gtk.py</span>
<span class="gu">@@ -160,6 +160,9 @@ class MltPlayer:</span>
         # TODO: figure out why SDL consumer seems to produce brighter images (black -&gt; grey)
         self.project = project
         os.putenv(&quot;SDL_WINDOWID&quot;, str(window_id))
<span class="gi">+        GLib.idle_add(self.init_player)</span>
<span class="gi">+</span>
<span class="gi">+    def init_player(self):</span>
         self.consumer = mlt.Consumer(self.project.get_preview_profile(), &quot;sdl&quot;)
         self.consumer.start()
         self.producer = None
</pre></div>
</div></div>
That is, I create the MLT consumer a little later, once GTK has had time to
start up a bit more.

And wow, this actually works!

I though about this idea because I had come across this comment in the
[Flowblade](https://github.com/jliljebl/flowblade) source code:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># SDL 2 consumer needs to created after Gtk.main() has run enough for window to be visible</span>
<span class="c1">#if editorstate.get_sdl_version() == editorstate.SDL_2: # needs more state consideration still</span>
<span class="c1">#    print &quot;SDL2 timeout launch&quot;</span>
<span class="c1">#    global sdl2_timeout_id</span>
<span class="c1">#    sdl2_timeout_id = GLib.timeout_add(1500, create_sdl_2_consumer)</span>
</pre></div>
</div></div>
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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">DummyConsumer</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">disconnect_all_producers</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;Dummy disconnect&quot;</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">connect</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">producer</span><span class="p">):</span>
        <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;Dummy connect&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># &#39;qtblend&#39; that was first used first seems to give problems</span>
<span class="c1"># when used in a GTK context. The application segfaults when</span>
<span class="c1"># started.</span>
<span class="c1">#</span>
<span class="c1"># Steps to reproduce:</span>
<span class="c1">#</span>
<span class="c1"># 1. ./make.py rundev foo.rlvideo resources/*mp4</span>
<span class="c1">#</span>
<span class="c1"># 2. Move a cut so that there is a overlap somewhere</span>
<span class="c1">#</span>
<span class="c1"># 3. ./make.py rundev foo.rlvideo</span>
<span class="c1">#</span>
<span class="c1"># Boom! Stacktrace:</span>
<span class="c1">#</span>
<span class="c1">#     (gdb) bt</span>
<span class="c1">#     #0  0x00007ffff7a64474 in pthread_mutex_lock () at /lib64/libpthread.so.0</span>
<span class="c1">#     #1  0x00007fffe96866af in XrmQGetResource () at /lib64/libX11.so.6</span>
<span class="c1">#     #2  0x00007fffe9667fca in XGetDefault () at /lib64/libX11.so.6</span>
<span class="c1">#     #3  0x00007fffe9a5ae8a in _cairo_xlib_surface_get_font_options () at /lib64/libcairo.so.2</span>
<span class="c1">#     ...</span>
<span class="c1">#</span>
<span class="c1"># frei0r.cairoblend seems to work better.</span>
<span class="c1">#</span>
<span class="c1"># TODO: How to fix this problem? Is qtblend just incompatible?</span>
</pre></div>
</div></div>
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
