---
title: 'DevLog 001: J-cuts and L-cuts in my video editor?'
date: 2023-07-28
tags: devlog,rlvideo
devlog: true
---

## About DevLogs

DevLogs is an experiment to try to document development that I do on various
projects. I will try to write what is going on in my head as I do various
development tasks.

## Today's problem

In my video editor, there is a problem with overlapping clips. How they overlap
appears to be almost random.

In this edit, the `two.mp4` clip is rendered below:

<p>
<center>
![Alternative text.](edit1.png)
</center>
</p>

If we edit the `one.mp4` clip, then the two switch order.

<p>
<center>
![Alternative text.](edit2.png)
</center>
</p>

So the order depends on the modification times of clips.

That is not very good.

## Plan

My idea for how to solve this is that each clip can specify how it should be
cut into the previous one.

I imagine a library of cuts such as:

* over
* under
* j-cut
* l-cut
* overlay (with priority)
* background (with priority)

To make progress on this, we can probably assume a default cut (maybe under)
and make sure it works. Then we can extend the library of cuts.

## Writing a test

The relevant code for this is mostly here:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">extract_mix_section</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">region</span><span class="p">):</span>
    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    &gt;&gt;&gt; cuts = Cuts.from_list([</span>
<span class="sd">    ...     Cut.test_instance(name=&quot;A&quot;, start=0, end=8, position=1),</span>
<span class="sd">    ...     Cut.test_instance(name=&quot;B&quot;, start=0, end=8, position=5),</span>
<span class="sd">    ... ])</span>
<span class="sd">    &gt;&gt;&gt; cuts.to_ascii_canvas()</span>
<span class="sd">    | &lt;-A0---&gt;    |</span>
<span class="sd">    |     &lt;-B0---&gt;|</span>
<span class="sd">    &gt;&gt;&gt; cuts.extract_mix_section(Region(start=0, end=15)).to_ascii_canvas()</span>
<span class="sd">    %&lt;-A0---&gt;%%%%%%</span>
<span class="sd">    %%%%%&lt;-B0---&gt;%%</span>
<span class="sd">    &quot;&quot;&quot;</span>
    <span class="c1"># TODO: sort based on cut (j-cut, l-cut, overlay, background).</span>
    <span class="n">playlists</span> <span class="o">=</span> <span class="p">[]</span>
    <span class="k">for</span> <span class="n">cut</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">create_cut</span><span class="p">(</span><span class="n">region</span><span class="p">)</span><span class="o">.</span><span class="n">cut_map</span><span class="o">.</span><span class="n">values</span><span class="p">():</span>
        <span class="n">playlists</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">Cuts</span><span class="o">.</span><span class="n">empty</span><span class="p">()</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">cut</span><span class="p">)</span><span class="o">.</span><span class="n">extract_playlist_section</span><span class="p">(</span><span class="n">region</span><span class="p">))</span>
    <span class="k">return</span> <span class="n">MixSection</span><span class="p">(</span><span class="n">length</span><span class="o">=</span><span class="n">region</span><span class="o">.</span><span class="n">length</span><span class="p">,</span> <span class="n">playlists</span><span class="o">=</span><span class="n">playlists</span><span class="p">)</span>
</pre></div>
</div></div>
Let's write a test that shows the current behavior first:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; region = Region(start=0, end=15)</span>
<span class="sd">&gt;&gt;&gt; a_cut = Cut.test_instance(name=&quot;A&quot;, start=0, end=8, position=1)</span>
<span class="sd">&gt;&gt;&gt; b_cut = Cut.test_instance(name=&quot;B&quot;, start=0, end=8, position=5)</span>

<span class="sd">&gt;&gt;&gt; Cuts.from_list([</span>
<span class="sd">...     a_cut,</span>
<span class="sd">...     b_cut,</span>
<span class="sd">... ]).extract_mix_section(region).to_ascii_canvas()</span>
<span class="sd">%&lt;-A0---&gt;%%%%%%</span>
<span class="sd">%%%%%&lt;-B0---&gt;%%</span>

<span class="sd">&gt;&gt;&gt; Cuts.from_list([</span>
<span class="sd">...     b_cut,</span>
<span class="sd">...     a_cut,</span>
<span class="sd">... ]).extract_mix_section(region).to_ascii_canvas()</span>
<span class="sd">%%%%%&lt;-B0---&gt;%%</span>
<span class="sd">%&lt;-A0---&gt;%%%%%%</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
So depending on the order in which cuts are added, they mix differently. This
is exactly the behavior that we want to change. We want them to mix the same no
matter what order they are added in.

Let's commit this as a baseline:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Pinpoint current behavior of mixing cuts.&#39;</span>
</pre></div>
</div></div>
The `./make.py commit` script is a wrapper around `git commit` that also runs
the tests and makes sure there are no untracked files. A real handy tool to
make sure we don't commit "bad" code.

## Cut the same

I modify the test above to assert that the same mix is created even when the
cuts are in the reverse order.

I make it pass by sorting the clips by start time, making "later" clips appear
below which would be the equivalent of having the cut on the second b clip set
to "under":

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">sort_cuts</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cuts</span><span class="p">):</span>
    <span class="k">return</span> <span class="nb">sorted</span><span class="p">(</span><span class="n">cuts</span><span class="p">,</span> <span class="n">key</span><span class="o">=</span><span class="k">lambda</span> <span class="n">cut</span><span class="p">:</span> <span class="n">cut</span><span class="o">.</span><span class="n">get_source_cut</span><span class="p">()</span><span class="o">.</span><span class="n">start</span><span class="p">)</span>
</pre></div>
</div></div>
That works as intended in the application as well. Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Always mix cuts below previous cut.&#39;</span>
</pre></div>
</div></div>
## Clips with the same start

If two cuts have the same start, the sorting will have no effect and we still
have the same problem.

First of all, I don't think this scenario will be that common in a real
situation.

I'm thinking we can handle it with a warning. If such situation appears, the
user has to resolve the issue by hinting how the mix should be done.

We can also take the end position into account. Then only cuts with the same
start and end has the problem. In which case the user must explicitly tell
which should be on top somehow.

## Summary

We made progress towards mixing cuts in a better way. Next step in this area I
think is to allow the type of cut to be specified per clip and take that into
account when sorting cuts. I think it should be quite easy to write tests for
this and we should be able to move steady and carefully using TDD.
