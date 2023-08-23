---
title: 'DevLog 012: Investigating export crash'
date: 2023-08-23
tags: devlog,rlvideo
devlog: true
---

I have managed to edit some footage using my own [video
editor](/projects/rlvideo/index.html). When I tried to export it, it took
forever and eventually crashed. In this DevLog, we will investigate why that
might be.

## How export works

When we press the export button, the following code is run:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Project</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">export</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">path</span> <span class="o">=</span> <span class="s2">&quot;export.mp4&quot;</span>
        <span class="n">producer</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">split_into_sections</span><span class="p">()</span><span class="o">.</span><span class="n">to_mlt_producer</span><span class="p">(</span>
            <span class="n">profile</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span>
            <span class="n">cache</span><span class="o">=</span><span class="n">ExportSourceLoader</span><span class="p">(</span><span class="n">profile</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span> <span class="n">project</span><span class="o">=</span><span class="bp">self</span><span class="p">)</span>
        <span class="p">)</span>
        <span class="k">def</span> <span class="nf">work</span><span class="p">(</span><span class="n">progress</span><span class="p">):</span>
            <span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span> <span class="s2">&quot;avformat&quot;</span><span class="p">)</span>
            <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;target&quot;</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
            <span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
            <span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
            <span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
                <span class="n">progress</span><span class="p">(</span><span class="n">producer</span><span class="o">.</span><span class="n">position</span><span class="p">()</span><span class="o">/</span><span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">())</span>
                <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">background_worker</span><span class="o">.</span><span class="n">add</span><span class="p">(</span>
            <span class="sa">f</span><span class="s2">&quot;Exporting </span><span class="si">{</span><span class="n">path</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">,</span>
            <span class="k">lambda</span> <span class="n">result</span><span class="p">:</span> <span class="kc">None</span><span class="p">,</span>
            <span class="n">work</span><span class="p">,</span>
        <span class="p">)</span>
</pre></div>
</div></div>
It creates an MLT producer with the real clips, and not the proxy clips. The
`work` function is called in a thread, and this code does the actual export:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span> <span class="s2">&quot;avformat&quot;</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;target&quot;</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
<span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
    <span class="n">progress</span><span class="p">(</span><span class="n">producer</span><span class="o">.</span><span class="n">position</span><span class="p">()</span><span class="o">/</span><span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">())</span>
    <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
</pre></div>
</div></div>
As I remember, this is the code that takes forever and eventually crash. I also
think its memory consumption steadily increase.

## The test

There is not much Python code in here. Just the loop that queries the consumer.
So my guess is that something in MLT consumes memory and eventually crashes. We
had a similar problem, I think, before when we created proxies using MLT in
this way. On the other hand, it seems unlikely that MLT would crash when
exporting a "small" project.

What I want to try today is to export my project as an MLT XML file and try to
render it using melt. It should do roughly the same thing as my Python code,
but will avoid using the Python binding for MLT.

If there is something wrong with MLT, which I doubt, the export will fail here
as well. If not, well, then I don't know what is wrong, but we can at least
rule out MLT (core).

## The test

We have this code that enables us to export MLT XML:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">if</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:</span><span class="mi">2</span><span class="p">]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;--export-melt&quot;</span><span class="p">]:</span>
    <span class="n">path</span> <span class="o">=</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">2</span><span class="p">]</span>
    <span class="nb">print</span><span class="p">(</span><span class="sa">f</span><span class="s2">&quot;Exporting </span><span class="si">{</span><span class="n">path</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">)</span>
    <span class="n">project</span> <span class="o">=</span> <span class="n">Project</span><span class="o">.</span><span class="n">load</span><span class="p">(</span><span class="n">args</span><span class="o">=</span><span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">3</span><span class="p">:])</span>
    <span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="n">project</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span> <span class="s2">&quot;xml&quot;</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;resource&quot;</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">project</span><span class="o">.</span><span class="n">get_preview_mlt_producer</span><span class="p">())</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
    <span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
        <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
    <span class="nb">print</span><span class="p">(</span><span class="s2">&quot;Done&quot;</span><span class="p">)</span>
    <span class="k">return</span>
</pre></div>
</div></div>
However, it creates the preview MLT producer which uses the proxy clips.

Since this is just a test, not intended to be committed, I modify this code to
instead create an MLT producer with the real clips.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="kn">from</span> <span class="nn">rlvideolib.domain.project</span> <span class="kn">import</span> <span class="n">ExportSourceLoader</span>
<span class="n">producer</span> <span class="o">=</span> <span class="n">project</span><span class="o">.</span><span class="n">split_into_sections</span><span class="p">()</span><span class="o">.</span><span class="n">to_mlt_producer</span><span class="p">(</span>
    <span class="n">profile</span><span class="o">=</span><span class="n">project</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span>
    <span class="n">cache</span><span class="o">=</span><span class="n">ExportSourceLoader</span><span class="p">(</span><span class="n">profile</span><span class="o">=</span><span class="n">project</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span> <span class="n">project</span><span class="o">=</span><span class="n">project</span><span class="p">)</span>
<span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
</pre></div>
</div></div>
Now we can export the XML like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ rlvideo --export-melt test.xml devlog-009.rlvideo 
Exporting test.xml
...
Done
</pre></div>
</div></div>
I verify that the XML file has references to the real clips. It does.  Perfect!

We can now do the equivalent export with this command:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>mlt-melt test.xml -consumer avformat target=export.mp4
</pre></div>
</div></div>
And now, it's just to wait and see what happens.

## A few minutes later

The memory consumption seems to be quite stable. Unless there is a memory leak,
this is what I expect. If the memory consumption keeps increasing for every
frame that is exported, that would mean that you can only export longer videos
by getting more memory. That does not seem right.

I should probably also verify that the export in the application keeps
increasing memory consumption. If it does, then there might be a memory leak in
the Python binding for MLT. Or I might use the binding incorrectly.

Using threads (which is used in the export) has also been problematic. I've
experienced that the Python threads interfere with the MLT threads. I'm don't
understand the problem fully, it's just a feeling. So that might be something
to look into. Try the export with threading disabled.

## A few hours later

I might have mistaken. The memory consumption seems to keep increasing.
However, the export finish without crashing and the final result looks fine.

## Summary

It seems that MLT consumes more and more memory the longer the exported video.
To confirm this, I should probably do some more precise measures. Maybe using
something like [psrecord](https://github.com/astrofrog/psrecord)? However,
memory consumption might not be problematic in itself. Perhaps it allocates
more memory to speed things up, but will not allocated more than what is
available. Perhaps the crash that I experienced before was not related to
memory.

We have learned something today, and this knowledge will make us better
prepared for the future.

Here are a few things I think of as possible next steps in this area:

* Measure memory consumption properly
* Compare memory consumption from MLT and rlvideo
* Try disabling threading in rlvideo
* "Optimize" the generated producer. It has many unnecessary tracks which I
  think will slow rendering down. (Should measure this to confirm.)

We'll see if we work on any of these the next time or something else.
