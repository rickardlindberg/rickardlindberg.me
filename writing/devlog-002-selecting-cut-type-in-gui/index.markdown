---
title: 'DevLog 002: Change mix strategy for cuts in GUI'
date: 2023-07-29
tags: devlog,rlvideo
devlog: true
---

In the [previous devlog](/writing/devlog-001-jcut-lcut/index.html) we worked on
adding the concept of a cut type to a clip in the [video
editor](/projects/rlvideo/index.html). That is, how should two overlapping
clips be mixed together? Which one should be on top?  Should it be a
[J-cut](https://en.wikipedia.org/wiki/J_cut)?

I've since added support for two types of cuts: over and under. So we can
(programmatically) set this property on clips and they will render in the
correct order.

The default cut is under so that later clips will be mixed under the previous
clips:

<p>
<center>
![Default under cut.](under.png)
</center>
</p>

There is not yet a way to change this default from the GUI, so that's what we
will work on in this episode.

## Aside: clips and cuts

When writing about this and when looking at the source code, I'm a bit confused
by the terminology. I write about clips och cuts, but in the source code there
is no concept of a clip, only a cut and a source.

A source represents a file on disk or some generator of frames. A cut
represents a region of a source.

A cut has a property called `cut` which is how to mix this cut with the
previous cut on the timeline. That confuses things further.

Let's rename it to `mix_strategy` to lessen the confusion.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Rename Cut.cut to Cut.mix_strategy.&#39;</span>
...............................................
----------------------------------------------------------------------
Ran <span class="m">47</span> tests <span class="k">in</span> <span class="m">1</span>.918s

OK
<span class="o">[</span>main 425909a<span class="o">]</span> Rename Cut.cut to Cut.mix_strategy.
 <span class="m">1</span> file changed, <span class="m">11</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">11</span> deletions<span class="o">(</span>-<span class="o">)</span>
</pre></div>
</div></div>
That's better.

## Two possible ways

I can think of two possible ways to set the mix strategy for a cut in the GUI.
Either we can right click on a cut and have a context menu pop up where we can
select the mix strategy. Or we can select a clip and have a "set mix strategy"
operation applied to the selected clip.

Currently, there is no concept of a selected clip. You can't select anything.
But there is a concept of clicking and dragging a clip. Therefore I think a
context menu is easier to get started with.

## Reviewing mouse click code

Let's look at how click and drag is handled today.

I see this GTK event is connected:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">timeline</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;button-press-event&quot;</span><span class="p">,</span> <span class="n">timeline_button</span><span class="p">)</span>
</pre></div>
</div></div>
And `timeline_button` is defined like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">timeline_button</span><span class="p">(</span><span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">mouse_down</span><span class="p">(</span><span class="o">*</span><span class="n">timeline</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
        <span class="n">main_window</span><span class="p">,</span>
        <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
        <span class="n">event</span><span class="o">.</span><span class="n">y</span>
    <span class="p">))</span>
</pre></div>
</div></div>
This code does not seem to distinguish between left and right mouse button
click. Interesting.

Does that mean that we can move a cut on the timeline by clicking and dragging
with the right mouse button? I try it in the application. And it indeed works.
That was really not my intention. Let's see if we can fix that.

I add a debug print to see what properties of the event might indicate the
button:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">timeline_button</span><span class="p">(</span><span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
    <span class="nb">print</span><span class="p">(</span><span class="nb">dir</span><span class="p">(</span><span class="n">event</span><span class="p">))</span>
    <span class="o">...</span>
</pre></div>
</div></div>
I find this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>[..., &#39;button&#39;, ..., &#39;get_button&#39;, ...]
</pre></div>
</div></div>
Let's try this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">timeline_button</span><span class="p">(</span><span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
    <span class="nb">print</span><span class="p">(</span><span class="n">event</span><span class="o">.</span><span class="n">button</span><span class="p">)</span>
</pre></div>
</div></div>
When I press the left button, it prints 1, and when I press the right button,
it prints 3. There must be some constants for these. I search the GTK
documentation and find
[this](https://docs.gtk.org/gdk3/struct.EventButton.html):

> The button which was pressed or released, numbered from 1 to 5. Normally
> button 1 is the left mouse button, 2 is the middle button, and 3 is the right
> button.

Maybe there are no constants?

Let's codify our new knowledge like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">timeline_button</span><span class="p">(</span><span class="n">widget</span><span class="p">,</span> <span class="n">event</span><span class="p">):</span>
    <span class="k">if</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">1</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">left_mouse_down</span><span class="p">(</span><span class="o">*</span><span class="n">timeline</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
            <span class="n">main_window</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">y</span>
        <span class="p">))</span>
    <span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">3</span><span class="p">:</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">right_mouse_down</span><span class="p">(</span><span class="o">*</span><span class="n">timeline</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
            <span class="n">main_window</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
            <span class="n">event</span><span class="o">.</span><span class="n">y</span>
        <span class="p">))</span>
</pre></div>
</div></div>
We rename the previous `mouse_down` to `left_mouse_down` and add a new empty
method for `right_mouse_down`.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Timeline receives both left and right mouse down events.&#39;</span>
...............................................
----------------------------------------------------------------------
Ran <span class="m">47</span> tests <span class="k">in</span> <span class="m">1</span>.923s

OK
<span class="o">[</span>main 0fc6fb1<span class="o">]</span> Timeline receives both left and right mouse down events.
 <span class="m">1</span> file changed, <span class="m">17</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">7</span> deletions<span class="o">(</span>-<span class="o">)</span>
</pre></div>
</div></div>
## Review

It's a little unnclear to me what the translation of coordinates are doing. I
think the coordinates received in the event are relative to the whole GTK
window and the timeline expects coordinates relative to itself.

I don't really want to focus on this now, but I add a TODO in the code that I
should clarify this.

In this project I've tried to keep my "backlog" in the source code in the form
of TODO comments. Some I will probably never get back to, and others will serve
as a reminder. But so far I kind of like this approach.

## Separation of timeline and GTK

The timeline component is unaware of GTK. So when it receives the right mouse
down event, it can find the cut that we clicked on, but it doesn't have the
ability to show a context menu, because it needs to use GTK for that.

This separation is intentional. I've tried to isolate GTK code to the
outermost layer to keep the inner layers free from those details and make them
easier to test.

But this presents a problem now.

The only solution that comes to mind if we want to maintain this separation is
to create some kind of abstraction. Something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GtkGuiAbstraction</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">show_context_menu</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">generic_menu_description</span><span class="p">):</span>
        <span class="c1"># Create GTK context menu from generic_menu_description</span>
</pre></div>
</div></div>
And then pass that to the timeline so that it can do something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">gui</span><span class="o">.</span><span class="n">show_context_menu</span><span class="p">([</span>
        <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;over&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="k">lambda</span><span class="p">:</span> <span class="o">...</span><span class="p">),</span>
        <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;under&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="k">lambda</span><span class="p">:</span> <span class="o">...</span><span class="p">),</span>
    <span class="p">])</span>
</pre></div>
</div></div>
I'm not sure what I think about this. On the one hand it feels like a complex
extra layer. On the other hand I really want to isolate GTK code. My experience
tells me that GUI code can easily leak in to every part of the application and
it just makes everything more messy.

I will try to create the simplest possible solution of this design and see what
it feels like.

## GTK GUI abstraction

Let's start with a test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">GtkGui</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">show_context_menu</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">menu</span><span class="p">):</span>
        <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">        &gt;&gt;&gt; GtkGui().show_context_menu([</span>
<span class="sd">        ...     MenuItem(label=&quot;over&quot;, action=lambda: print(&quot;over&quot;)),</span>
<span class="sd">        ...     MenuItem(label=&quot;under&quot;, action=lambda: print(&quot;under&quot;)),</span>
<span class="sd">        ... ])</span>
<span class="sd">        &quot;&quot;&quot;</span>
</pre></div>
</div></div>
This fails with

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>NameError: name &#39;MenuItem&#39; is not defined
</pre></div>
</div></div>
I define `MenuItem` and we are green:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">MenuItem</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;MenuItem&quot;</span><span class="p">,</span> <span class="s2">&quot;label,action&quot;</span><span class="p">)):</span>
    <span class="k">pass</span>
</pre></div>
</div></div>
Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;The start of GtkGui and its show_context_menu method.&#39;</span>
................................................
----------------------------------------------------------------------
Ran <span class="m">48</span> tests <span class="k">in</span> <span class="m">1</span>.923s

OK
<span class="o">[</span>main e64b93e<span class="o">]</span> The start of GtkGui and its show_context_menu method.
 <span class="m">1</span> file changed, <span class="m">13</span> insertions<span class="o">(</span>+<span class="o">)</span>
</pre></div>
</div></div>
The test so far does not assert anything. It just checks that the code does not
crash. But that is enough to experiment with the GTK API. Let's try to create
the menu and the test will tell is if we use the GKT API wrong.

I try this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">menu</span> <span class="o">=</span> <span class="n">gtk</span><span class="o">.</span><span class="n">Menu</span><span class="p">()</span>
</pre></div>
</div></div>
Test immediately fails:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>NameError: name &#39;gtk&#39; is not defined
</pre></div>
</div></div>
Ah, it should be `Gtk` I see in the imports at the top of the file. Thank you
test.

I search the web for examples how to show a context menu in GTK. After a bit of
reading and trying, I end up with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">show_context_menu</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">menu</span><span class="p">):</span>
    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    &gt;&gt;&gt; event = namedtuple(&quot;FakeEvent&quot;, &quot;button,time&quot;)(3, 0)</span>
<span class="sd">    &gt;&gt;&gt; GtkGui(event).show_context_menu([</span>
<span class="sd">    ...     MenuItem(label=&quot;over&quot;, action=lambda: print(&quot;over&quot;)),</span>
<span class="sd">    ...     MenuItem(label=&quot;under&quot;, action=lambda: print(&quot;under&quot;)),</span>
<span class="sd">    ... ])</span>
<span class="sd">    &quot;&quot;&quot;</span>
    <span class="k">def</span> <span class="nf">create_gtk_handler</span><span class="p">(</span><span class="n">menu_item</span><span class="p">):</span>
        <span class="k">def</span> <span class="nf">handler</span><span class="p">(</span><span class="n">widget</span><span class="p">):</span>
            <span class="n">menu_item</span><span class="o">.</span><span class="n">action</span><span class="p">()</span>
        <span class="k">return</span> <span class="n">handler</span>
    <span class="n">gtk_menu</span> <span class="o">=</span> <span class="n">Gtk</span><span class="o">.</span><span class="n">Menu</span><span class="p">()</span>
    <span class="k">for</span> <span class="n">menu_item</span> <span class="ow">in</span> <span class="n">menu</span><span class="p">:</span>
        <span class="n">gtk_menu_item</span> <span class="o">=</span> <span class="n">Gtk</span><span class="o">.</span><span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="n">menu_item</span><span class="o">.</span><span class="n">label</span><span class="p">)</span>
        <span class="n">gtk_menu_item</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="s2">&quot;activate&quot;</span><span class="p">,</span> <span class="n">create_gtk_handler</span><span class="p">(</span><span class="n">menu_item</span><span class="p">))</span>
        <span class="n">gtk_menu_item</span><span class="o">.</span><span class="n">show</span><span class="p">()</span>
        <span class="n">gtk_menu</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">gtk_menu_item</span><span class="p">)</span>
    <span class="n">gtk_menu</span><span class="o">.</span><span class="n">popup</span><span class="p">(</span><span class="kc">None</span><span class="p">,</span> <span class="kc">None</span><span class="p">,</span> <span class="kc">None</span><span class="p">,</span> <span class="kc">None</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">button</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">event</span><span class="o">.</span><span class="n">time</span><span class="p">)</span>
</pre></div>
</div></div>
The `Gtk.Menu` seems to need an event to show itself according to examples. So
I pass that along to `GtkGui` and use a fake one in the test. The event handler
looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
    <span class="n">gui</span><span class="o">.</span><span class="n">show_context_menu</span><span class="p">([</span>
        <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;over&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="k">lambda</span><span class="p">:</span> <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;over&quot;</span><span class="p">)),</span>
        <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;under&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="k">lambda</span><span class="p">:</span> <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;under&quot;</span><span class="p">)),</span>
    <span class="p">])</span>
</pre></div>
</div></div>
I decide to pass the `gui` along in the method call. That way, it can more
easily be constructed with the right click event in the outer layer:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">elif</span> <span class="n">event</span><span class="o">.</span><span class="n">button</span> <span class="o">==</span> <span class="mi">3</span><span class="p">:</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">timeline</span><span class="o">.</span><span class="n">right_mouse_down</span><span class="p">(</span><span class="o">*</span><span class="n">timeline</span><span class="o">.</span><span class="n">translate_coordinates</span><span class="p">(</span>
        <span class="n">main_window</span><span class="p">,</span>
        <span class="n">event</span><span class="o">.</span><span class="n">x</span><span class="p">,</span>
        <span class="n">event</span><span class="o">.</span><span class="n">y</span>
    <span class="p">),</span> <span class="n">GtkGui</span><span class="p">(</span><span class="n">event</span><span class="p">))</span>
</pre></div>
</div></div>
This works fine and when I click the menu items, the corresponding text is
shown in the console:

<p>
<center>
![Context menu popup.](popup.png)
</center>
</p>

Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Show a context menu when right clicking in timeline.&#39;</span>
................................................
----------------------------------------------------------------------
Ran <span class="m">48</span> tests <span class="k">in</span> <span class="m">1</span>.954s

OK
<span class="o">[</span>main <span class="m">4201621</span><span class="o">]</span> Show a context menu when right clicking <span class="k">in</span> timeline.
 <span class="m">1</span> file changed, <span class="m">22</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">4</span> deletions<span class="o">(</span>-<span class="o">)</span>
</pre></div>
</div></div>
## Modify cut

I continue to modify `right_mouse_down` to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
    <span class="n">cut</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">cut</span><span class="p">,</span> <span class="n">Cut</span><span class="p">):</span>
        <span class="n">gui</span><span class="o">.</span><span class="n">show_context_menu</span><span class="p">([</span>
            <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;over&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="k">lambda</span><span class="p">:</span> <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;over&quot;</span><span class="p">)),</span>
            <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;under&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="k">lambda</span><span class="p">:</span> <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;under&quot;</span><span class="p">)),</span>
        <span class="p">])</span>
</pre></div>
</div></div>
I want to show the context menu only if we right click on a cut. We can use
the rectangle map for that.

All tests pass, but when I try this in the application, it fails with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>NameError: name &#39;Cut&#39; is not defined
</pre></div>
</div></div>
I should have started with a test. Let's move a little slower.

I add this line in a larger timeline test where we have some objects setup:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; timeline.right_mouse_down(5, 25, FakeGui(click_context_menu=&quot;over&quot;))</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And here comes one benefit of the GUI abstraction: easier testing. In the test
we can pass a `FakeGui` that will simulate that we click a context menu item.
We implement it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FakeGui</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">click_context_menu</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">click_context_menu</span> <span class="o">=</span> <span class="n">click_context_menu</span>

    <span class="k">def</span> <span class="nf">show_context_menu</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">menu</span><span class="p">):</span>
        <span class="k">for</span> <span class="n">menu_item</span> <span class="ow">in</span> <span class="n">menu</span><span class="p">:</span>
            <span class="k">if</span> <span class="n">menu</span><span class="o">.</span><span class="n">label</span> <span class="o">==</span> <span class="bp">self</span><span class="o">.</span><span class="n">click_context_menu</span><span class="p">:</span>
                <span class="n">menu</span><span class="o">.</span><span class="n">action</span><span class="p">()</span>
                <span class="k">return</span>
</pre></div>
</div></div>
Now we get the same error about `Cut` not being defined. But this time, we get
it when running the test suite. Success!

I import `Cut` and get an error that 'list' object has no attribute 'label'.
Ah. I made a mistake in the fake GUI. `label` and `action` should be accessed
on the item, not the menu.

The current context menu just prints its label, so to make the test pass, let's
assert on that:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; timeline.right_mouse_down(5, 25, FakeGui(click_context_menu=&quot;over&quot;))</span>
<span class="sd">over</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
And we are back to green. Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Test right clicking a cut.&#39;</span>
................................................
----------------------------------------------------------------------
Ran <span class="m">48</span> tests <span class="k">in</span> <span class="m">1</span>.957s

OK
<span class="o">[</span>main 7bf3e14<span class="o">]</span> Test right clicking a cut.
 <span class="m">1</span> file changed, <span class="m">33</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">7</span> deletions<span class="o">(</span>-<span class="o">)</span>
</pre></div>
</div></div>
But we don't want to print the menu item label. We want to change the
`mix_strategy` of the clicked cut. Let's assert on that instead:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; timeline.right_mouse_down(5, 25, FakeGui(click_context_menu=&quot;over&quot;))</span>
<span class="sd">&gt;&gt;&gt; timeline.get_cut(cut_id).mix_strategy</span>
<span class="sd">&#39;over&#39;</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
This fails because `right_mouse_down` still prints its label. I remove the
print and it now fails because `Timeline.get_cut` is not defined. I add it and
get the correct failure:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Failed example:
    timeline.get_cut(cut_id).mix_strategy
Differences (ndiff with -expected +actual):
    - &#39;over&#39;
    + &#39;under&#39;
</pre></div>
</div></div>
The original mix strategy is `over` and this test should have changed it to
`under`, but it didn't. Let's fix that. As I try to get this test to pass, I
get many test failures. The failures guide me what to do next. This method is
not defined. Define it. This name does not exist. Fix spell error. I eventually
end up with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">right_mouse_down</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">gui</span><span class="p">):</span>
    <span class="n">cut</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rectangle_map</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">)</span>
    <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">cut</span><span class="p">,</span> <span class="n">Cut</span><span class="p">):</span>
        <span class="k">def</span> <span class="nf">mix_strategy_updater</span><span class="p">(</span><span class="n">value</span><span class="p">):</span>
            <span class="k">def</span> <span class="nf">update</span><span class="p">():</span>
                <span class="k">with</span> <span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">new_transaction</span><span class="p">()</span> <span class="k">as</span> <span class="n">transaction</span><span class="p">:</span>
                    <span class="n">transaction</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">cut</span><span class="o">.</span><span class="n">id</span><span class="p">,</span> <span class="k">lambda</span> <span class="n">cut</span><span class="p">:</span>
                        <span class="n">cut</span><span class="o">.</span><span class="n">with_mix_strategy</span><span class="p">(</span><span class="n">value</span><span class="p">))</span>
            <span class="k">return</span> <span class="n">update</span>
        <span class="n">gui</span><span class="o">.</span><span class="n">show_context_menu</span><span class="p">([</span>
            <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;over&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="n">mix_strategy_updater</span><span class="p">(</span><span class="s2">&quot;over&quot;</span><span class="p">)),</span>
            <span class="n">MenuItem</span><span class="p">(</span><span class="n">label</span><span class="o">=</span><span class="s2">&quot;under&quot;</span><span class="p">,</span> <span class="n">action</span><span class="o">=</span><span class="n">mix_strategy_updater</span><span class="p">(</span><span class="s2">&quot;under&quot;</span><span class="p">)),</span>
        <span class="p">])</span>
</pre></div>
</div></div>
This makes the test pass and also works beautifully in the application. Let's
commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Can change mix strategy of clip with context menu.&#39;</span>
................................................
----------------------------------------------------------------------
Ran <span class="m">48</span> tests <span class="k">in</span> <span class="m">1</span>.956s

OK
<span class="o">[</span>main 776171a<span class="o">]</span> Can change mix strategy of clip with context menu.
 <span class="m">3</span> files changed, <span class="m">26</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">4</span> deletions<span class="o">(</span>-<span class="o">)</span>
</pre></div>
</div></div>
## Summary

We now have a way to change the mix strategy of a cut in the GUI. The
application is a little more useful now.

Working with third party frameworks, like GTK, I find often slows you down. You
need to learn the details of it and it is often difficult to write tests.
Therefore I'm quite happy with the abstraction that we created. I want to keep
as many classes as possible away from messy GTK code.
