---
title: How to get fast feedback on graphical code?
date: 2023-07-28
tags: rlvideo
---

I am working on [my own video
editor](/writing/writing-my-own-video-editor/index.html). It currently looks
like this:

<p>
<center>
![Current state of video editor.](rlvideo.png)
</center>
</p>

The bottom pane shows the timeline with all the clips. It is a
`Gtk.DrawingArea` where all the drawing is done using Cairo.

When I work on the drawing code, I want fast feedback on it. Does it look good?
Does it draw as intended?

Usually I use TDD for this kind of feedback, but graphical output it hard to
test.

In the beginning, I used to run the application after every change. This was
quite fast because the application is still small and I can start it up with a
sample timeline quite quickly. However, it still takes a few seconds and a
couple of keystrokes.

Then I came up with a much better workflow.

I added a (doc)test to my test suite that does something like this (some
details removed):

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)</span>
<span class="sd">&gt;&gt;&gt; context = cairo.Context(surface)</span>
<span class="sd">&gt;&gt;&gt; project = Project.new()</span>
<span class="sd">&gt;&gt;&gt; with project.new_transaction() as transaction:</span>
<span class="sd">...     _ = transaction.add_text_clip(&quot;hello&quot;, length=30)</span>
<span class="sd">...     x = transaction.add_text_clip(&quot;world&quot;, length=35)</span>
<span class="sd">...     _ = transaction.add_text_clip(&quot;end&quot;, length=20)</span>
<span class="sd">...     _ = transaction.add_text_clip(&quot;end&quot;, length=20)</span>
<span class="sd">...     transaction.modify(x, lambda cut: cut.move(-10))</span>
<span class="sd">&gt;&gt;&gt; timeline = Timeline(project)</span>
<span class="sd">&gt;&gt;&gt; timeline.draw_cairo(</span>
<span class="sd">...     context=context,</span>
<span class="sd">...     playhead_position=40,</span>
<span class="sd">...     width=width,</span>
<span class="sd">...     height=height</span>
<span class="sd">... )</span>
<span class="sd">&gt;&gt;&gt; surface.write_to_png(&quot;timeline.png&quot;)</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
The `Timeline` class is what does the drawing of the timeline. Fortunately, it
is separated from the GTK component onto which it is drawn. This allows us to
create our own Cairo surface, let the timeline draw itself on that, and in the
end, write that surface to a file.

Every time we run the test suite (which I like to do automatically on every
save) we get a new `timeline.png` file where the example clips that we
populated in the test are drawn. If we open this file in the GNOME image viewer
(`eog timeline.png`) it will automatically reload the image when it changes.

In this workflow, this is my typical setup:

<p>
<center>
![Workflow setup.](workflow1.png)
</center>
</p>

I have my editor to the right, the automatic test suite runner in the top
left, and the timeline image in the bottom left.

In this example I draw two timelines with different zoom levels so that I can
quickly see how that looks.

Then I can make a change to some color for example, and within a second or two,
my test suite has automatically run and my desktop looks like this:

<p>
<center>
![Workflow after a change.](workflow2.png)
</center>
</p>

I can tweak numbers until I think it looks good and I never have to leave my
editor.
