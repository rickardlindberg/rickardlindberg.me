---
title: 'DevLog 003: Clarify GUI separation'
date: 2023-07-29
tags: devlog,rlvideo
devlog: true
---

In the [video editor](/projects/rlvideo/index.html), there is the idea that we
want to isolate the GTK code and have as few classes as possible depend on it.
However, this idea is not clearly expressed in the code. So new readers of the
code base will not necessarily understand that this separation is intentional
and something that we want to do.

In this episode I want to refactor the code to make that more clear.

## Current state

The current layout of the Python files looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>.
├── rlvideolib
│   ├── asciicanvas.py
│   ├── debug.py
│   ├── domain
│   │   ├── cut.py
│   │   ├── __init__.py
│   │   ├── project.py
│   │   ├── region.py
│   │   ├── section.py
│   │   └── source.py
│   ├── events.py
│   ├── graphics
│   │   ├── __init__.py
│   │   └── rectangle.py
│   ├── __init__.py
│   ├── jobs.py
│   └── testing.py
└── rlvideo.py
</pre></div>
</div></div>
There is the "main" file `rlvideo.py` and the `rlvideolib` package.

The main file is sort of the default place where new things go that don't fit
anywhere else. It currently has a mix of classes with different areas of
responsibility:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FakeGui</span><span class="p">:</span>
<span class="k">class</span> <span class="nc">GtkGui</span><span class="p">:</span>
<span class="k">class</span> <span class="nc">MenuItem</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;MenuItem&quot;</span><span class="p">,</span> <span class="s2">&quot;label,action&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">App</span><span class="p">:</span>
<span class="k">class</span> <span class="nc">MltPlayer</span><span class="p">:</span>
<span class="k">class</span> <span class="nc">Timeline</span><span class="p">:</span>
<span class="k">class</span> <span class="nc">Scrollbar</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;Scrollbar&quot;</span><span class="p">,</span> <span class="s2">&quot;content_length,one_length_in_pixels,ui_size,content_desired_start&quot;</span><span class="p">)):</span>
</pre></div>
</div></div>
Some of these classes deal with GTK. Others with GUI code that does not depend
on GTK directly.

I would like to create a new `rlvideolib.gui` package that has three modules:

* gtk
* generic
* testing

## Testing

I extract a new testing module like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ mkdir rlvideolib/gui
$ touch rlvideolib/gui/__init__.py
$ touch rlvideolib/gui/testing.py
</pre></div>
</div></div>
Then I move the `FakeGui` class to that module and also rename it to `TestGui`
as I think that is a more descriptive name. I also make sure to import it from
`rlvideo.py`.

Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Extract rlvideolib.gui.testing.&#39;</span>
................................................
----------------------------------------------------------------------
Ran <span class="m">48</span> tests <span class="k">in</span> <span class="m">1</span>.931s

OK
<span class="o">[</span>main 91b63c2<span class="o">]</span> Extract rlvideolib.gui.testing.
 <span class="m">4</span> files changed, <span class="m">14</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">12</span> deletions<span class="o">(</span>-<span class="o">)</span>
 create mode <span class="m">100644</span> rlvideolib/gui/__init__.py
 create mode <span class="m">100644</span> rlvideolib/gui/testing.py
</pre></div>
</div></div>
## Generic

Let's do the same thing for generic GUI code:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ touch rlvideolib/gui/generic.py
</pre></div>
</div></div>
I move over the following classes:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Timeline</span><span class="p">:</span>
<span class="k">class</span> <span class="nc">Scrollbar</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;Scrollbar&quot;</span><span class="p">,</span> <span class="s2">&quot;content_length,one_length_in_pixels,ui_size,content_desired_start&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">MenuItem</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;MenuItem&quot;</span><span class="p">,</span> <span class="s2">&quot;label,action&quot;</span><span class="p">)):</span>
</pre></div>
</div></div>
If we look at the imports for the generic GUI module, we see this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">from</span> <span class="nn">collections</span> <span class="kn">import</span> <span class="n">namedtuple</span>

<span class="kn">import</span> <span class="nn">cairo</span>
<span class="kn">import</span> <span class="nn">mlt</span>

<span class="kn">from</span> <span class="nn">rlvideolib.debug</span> <span class="kn">import</span> <span class="n">timeit</span>
<span class="kn">from</span> <span class="nn">rlvideolib.domain.project</span> <span class="kn">import</span> <span class="n">Project</span>
<span class="kn">from</span> <span class="nn">rlvideolib.graphics.rectangle</span> <span class="kn">import</span> <span class="n">RectangleMap</span>
<span class="kn">from</span> <span class="nn">rlvideolib.graphics.rectangle</span> <span class="kn">import</span> <span class="n">Rectangle</span>
<span class="kn">from</span> <span class="nn">rlvideolib.events</span> <span class="kn">import</span> <span class="n">Event</span>
<span class="kn">from</span> <span class="nn">rlvideolib.domain.region</span> <span class="kn">import</span> <span class="n">Region</span>
<span class="kn">from</span> <span class="nn">rlvideolib.gui.testing</span> <span class="kn">import</span> <span class="n">TestGui</span>
<span class="kn">from</span> <span class="nn">rlvideolib.domain.cut</span> <span class="kn">import</span> <span class="n">Cut</span>
</pre></div>
</div></div>
We can see that it depends on Cairo. That is ok. The drawing of generic GUI
components is done with Cairo. I don't see as big a reason to abstract that
compared to GTK. Cairo can also most likely be used with other GUI frameworks.
The important thing here is that there is no import of GTK.

Let's commit this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Extract timelinelib.gui.generic.&#39;</span>
................................................
----------------------------------------------------------------------
Ran <span class="m">48</span> tests <span class="k">in</span> <span class="m">1</span>.937s

OK
<span class="o">[</span>main e392173<span class="o">]</span> Extract timelinelib.gui.generic.
 <span class="m">3</span> files changed, <span class="m">220</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">819</span> deletions<span class="o">(</span>-<span class="o">)</span>
 rewrite rlvideo.py <span class="o">(</span><span class="m">64</span>%<span class="o">)</span>
 copy rlvideo.py <span class="o">=</span>&gt; rlvideolib/gui/generic.py <span class="o">(</span><span class="m">65</span>%<span class="o">)</span>
</pre></div>
</div></div>
We should probably also run the application to see that I didn't mess up
anything major. I get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Traceback (most recent call last):
  File &quot;/home/rick/rlvideo/rlvideo.py&quot;, line 213, in &lt;module&gt;
    App().run()
  File &quot;/home/rick/rlvideo/rlvideo.py&quot;, line 156, in run
    self.timeline = Timeline(project=self.project, player=mlt_player)
NameError: name &#39;Timeline&#39; is not defined
</pre></div>
</div></div>
Woopsie.

I add the missing import to `rlvideo.py`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">from</span> <span class="nn">rlvideolib.gui.generic</span> <span class="kn">import</span> <span class="n">Timeline</span>
</pre></div>
</div></div>
If we want to catch this error in the test suite, we must run the whole
application which incudes starting the GTK main loop.

I'm thinking that we can separate the construction of the GUI from the actual
main loop, something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">App</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">init</span><span class="p">()</span>
        <span class="n">Gtk</span><span class="o">.</span><span class="n">main</span><span class="p">()</span>

    <span class="o">...</span>
</pre></div>
</div></div>
And then we can run `App().init()` in a test to make sure that construction of
the GUI works.

I try this, but run into all kinds of issues.

I decide to leave this as is for now.

## GUI

All classes that are left in `rlvideo.py` are now related to GTK. Let's move
them to its own module, leaving only this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">from</span> <span class="nn">rlvideolib.gui.gtk</span> <span class="kn">import</span> <span class="n">App</span>

<span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="n">App</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">()</span>
</pre></div>
</div></div>
Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Extract timelinelib.gui.gtk.&#39;
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.941s

OK
[main c4702cc] Extract timelinelib.gui.gtk.
 3 files changed, 5 insertions(+), 217 deletions(-)
 rewrite rlvideo.py (99%)
 copy rlvideo.py =&gt; rlvideolib/gui/gtk.py (99%)
</pre></div>
</div></div>
## Summary

The new structure looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">rlvideolib</span><span class="o">/</span><span class="n">gui</span><span class="o">/</span><span class="n">testing</span><span class="o">.</span><span class="n">py</span>
<span class="mi">1</span><span class="p">:</span><span class="k">class</span> <span class="nc">TestGui</span><span class="p">:</span>

<span class="n">rlvideolib</span><span class="o">/</span><span class="n">gui</span><span class="o">/</span><span class="n">generic</span><span class="o">.</span><span class="n">py</span>
<span class="mi">17</span><span class="p">:</span><span class="k">class</span> <span class="nc">Timeline</span><span class="p">:</span>
<span class="mi">312</span><span class="p">:</span><span class="k">class</span> <span class="nc">Scrollbar</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;Scrollbar&quot;</span><span class="p">,</span> <span class="s2">&quot;content_length,one_length_in_pixels,ui_size,content_desired_start&quot;</span><span class="p">)):</span>
<span class="mi">400</span><span class="p">:</span><span class="k">class</span> <span class="nc">MenuItem</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;MenuItem&quot;</span><span class="p">,</span> <span class="s2">&quot;label,action&quot;</span><span class="p">)):</span>

<span class="n">rlvideolib</span><span class="o">/</span><span class="n">gui</span><span class="o">/</span><span class="n">gtk</span><span class="o">.</span><span class="n">py</span>
<span class="mi">16</span><span class="p">:</span><span class="k">class</span> <span class="nc">GtkGui</span><span class="p">:</span>
<span class="mi">41</span><span class="p">:</span><span class="k">class</span> <span class="nc">App</span><span class="p">:</span>
<span class="mi">163</span><span class="p">:</span><span class="k">class</span> <span class="nc">MltPlayer</span><span class="p">:</span>
</pre></div>
</div></div>
I think this structure tells the reader that there is a clear separation
between GTK related GUI code and generic GUI code and that this separation is
intentional.

The `MltPlayer` that we see in the GTK module has only very little to do with
GTK. Most of it just works with an MLT producer. GTK is needed for the
embedding of the video display from the MLT consumer inside a GTK window.

I think part of `MltPlayer` should probably be extracted to a new class and be
put in the generic GUI module or somewhere else. This refactoring to separate
the different GUI modules revealed that to me. I find that is often the case.
You make one refactoring to clarify something and you discover something else
that is unclear.
