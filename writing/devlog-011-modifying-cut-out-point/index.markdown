---
title: 'DevLog 011: Modifying cut out point'
date: 2023-08-06
tags: devlog,rlvideo
devlog: true
---

I've added a few more timeline edit operations to the [video
editor](/projects/rlvideo/index.html). For example, it is now possible to
change the speed of a cut with ctrl+drag on the right hand side and modify the
in point with drag on the left hand side.

<p>
<center>
![Move right.](move-right.png)
</center>
</p>

However, changing the out point of a cut by dragging the right hand side does
not yet work. It prints the following in the console:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>TODO: implement move_right!
</pre></div>
</div></div>
It is a bit trickier to get working than changing the in point as we will see
in a second.

## The call chain

Here is roughly what happens when you drag the right hand side of a cut:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">transaction</span> <span class="o">=</span> <span class="n">project</span><span class="o">.</span><span class="n">new_transaction</span><span class="p">()</span>
<span class="n">transaction</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">cut_id</span><span class="p">,</span> <span class="k">lambda</span> <span class="n">cut</span><span class="p">:</span> <span class="n">cut</span><span class="o">.</span><span class="n">move_right</span><span class="p">(</span><span class="n">delta</span><span class="p">))</span>
<span class="n">transaction</span><span class="o">.</span><span class="n">commit</span><span class="p">()</span>
</pre></div>
</div></div>
Here is `Transaction.modify`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">modify</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">):</span>
    <span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">set_project_data</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">project_data</span><span class="o">.</span><span class="n">modify_cut</span><span class="p">(</span><span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">))</span>
</pre></div>
</div></div>
Here is `ProjectData.modify_cut`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">modify_cut</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">):</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">cuts</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">cuts</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">))</span>
</pre></div>
</div></div>
Here is `Cuts.modify`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">modify</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="n">old_cut</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">cut_map</span><span class="p">[</span><span class="n">cut_id</span><span class="p">]</span>
    <span class="n">new_cut</span> <span class="o">=</span> <span class="n">fn</span><span class="p">(</span><span class="n">old_cut</span><span class="p">)</span>
    <span class="n">new_cuts</span> <span class="o">=</span> <span class="nb">dict</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">cut_map</span><span class="p">)</span>
    <span class="n">new_cuts</span><span class="p">[</span><span class="n">cut_id</span><span class="p">]</span> <span class="o">=</span> <span class="n">new_cut</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span>
        <span class="n">cut_map</span><span class="o">=</span><span class="n">new_cuts</span><span class="p">,</span>
        <span class="n">region_to_cuts</span><span class="o">=...</span><span class="p">,</span>
    <span class="p">)</span>
</pre></div>
</div></div>
And it is here that the lambda gets called to modify the cut.

The problem is that when we modify the out point, we can't place it outside the
length of the source. And the cut itself does not know how long the source is.
It just has a source id where it can be looked up, but only in the
`ProjectData` structure, which is two levels above.

## Data structure consistency

Let's have a look at the data structures and what they contain:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">ProjectData</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;ProjectData&quot;</span><span class="p">,</span> <span class="s2">&quot;sources,cuts&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">Sources</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;Sources&quot;</span><span class="p">,</span> <span class="s2">&quot;id_to_source&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">FileSource</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;FileSource&quot;</span><span class="p">,</span> <span class="s2">&quot;id,path,length&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">TextSource</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;TextSource&quot;</span><span class="p">,</span> <span class="s2">&quot;id,text&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">Cuts</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;Cuts&quot;</span><span class="p">,</span> <span class="s2">&quot;cut_map,region_to_cuts,region_group_size&quot;</span><span class="p">)):</span>
<span class="k">class</span> <span class="nc">Cut</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;Cut&quot;</span><span class="p">,</span> <span class="s2">&quot;source,in_out,position,id,mix_strategy,volume,speed&quot;</span><span class="p">)):</span>
</pre></div>
</div></div>
Put in a more hierarchical format:

* ProjectData
    * sources (Sources)
    * cuts (Cuts)
* Sources
    * id_to_source (id -> source)
* Source
    * FileSource
    * TextSource
* Cuts
    * cut_map (id -> Cut)
    * region_to_cuts
    * region_group_size
* Cut
    * source (id)
    * in_out
    * position
    * id
    * mix_strategy
    * volume
    * speed

To make sure that a cut's out point does not exceed the length of the source,
we have to make the check in ProjectData since that is the only structure that
has both the source information and the cut information.

## Modify cut

Let's have a look at `ProjectData.modify_cut` again:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">modify_cut</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">):</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">cuts</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">cuts</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">))</span>
</pre></div>
</div></div>
How about if we did something like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">modify_cut</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">):</span>
    <span class="k">def</span> <span class="nf">wrapper</span><span class="p">(</span><span class="n">cut</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">sources</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">cut</span><span class="o">.</span><span class="n">source</span><span class="p">)</span><span class="o">.</span><span class="n">limit_out_point</span><span class="p">(</span><span class="n">fn</span><span class="p">(</span><span class="n">cut</span><span class="p">))</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">cuts</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">cuts</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">cut_id</span><span class="p">,</span> <span class="n">wrapper</span><span class="p">))</span>
</pre></div>
</div></div>
That is, we let the original lambda  modify the out point beyond the length of
the source. Then in the wrapper above we get the source of the clip and have it
adjust the out point to not exceed the length.

I think this will actually work.

## Reflections

When first thinking about this problem I had a much more complicated solution
in mind. I was annoyed that the cut itself did not know about the maximum
length. I was thinking that `Cut.modify` somehow has to be passed a length so
that it could do the limiting itself.

Then I started writing about it, and I thought that each data structure should
be responsible for validating itself. Since a cut has no information about
length, it is ok to specify any length. But when a cut is put into a
`ProjectData` and is associated with a source, the validation must happen.

This makes a lot of sense to me, and I feel like a made a breakthrough.

You could argue that the design of the data structure is wrong. Perhaps a cut
should have more information about its source so that it can do more
validation.

But when it looks as it does, I think this will be fine.

Let's see if we can test this.

## Testing limiting out point

Here is the test that I come up with:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">A cut&#39;s out point is adjusted if going outside the limit:</span>

<span class="sd">&gt;&gt;&gt; data = ProjectData.empty()</span>
<span class="sd">&gt;&gt;&gt; data = data.add_source(FileSource(id=&quot;source_a&quot;, path=&quot;a.mp4&quot;, length=5))</span>
<span class="sd">&gt;&gt;&gt; data = data.add_cut(Cut.test_instance(name=&quot;source_a&quot;, start=0, end=3, id=&quot;cut_a&quot;))</span>
<span class="sd">&gt;&gt;&gt; data = data.modify_cut(&quot;cut_a&quot;, lambda cut: cut.move_right(10))</span>
<span class="sd">&gt;&gt;&gt; data.get_cut(&quot;cut_a&quot;).in_out</span>
<span class="sd">Region(start=0, end=5)</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
We create project data with one source and one cut. The source is of length 5
and the cut is of length 3. We can extend it two more frames before we have
reached the end of the source.

Then we modify the cut by trying to extend it by 10 frames.

Then we assert that the end point is limited to 5.

This fails with this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Failed example:
    data = data.modify_cut(&quot;cut_a&quot;, lambda cut: cut.move_right(10))
Differences (ndiff with -expected +actual):
    + TODO: implement move_right!
</pre></div>
</div></div>
I implement `Cut.move_right` like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">move_right</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">amount</span><span class="p">):</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span>
        <span class="n">in_out</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">in_out</span><span class="o">.</span><span class="n">move_end</span><span class="p">(</span><span class="n">amount</span><span class="p">),</span>
    <span class="p">)</span>
</pre></div>
</div></div>
Then we get this failure:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Failed example:
    data.get_cut(&quot;cut_a&quot;).in_out
Differences (ndiff with -expected +actual):
    - Region(start=0, end=5)
    ?                     ^
    + Region(start=0, end=13)
    ?                     ^^
</pre></div>
</div></div>
I expected this. We don't do any limiting yet.

Let's modify `ProjectData.modify_cut` to what we had in mind. I write this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">modify_cut</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut_id</span><span class="p">,</span> <span class="n">fn</span><span class="p">):</span>
    <span class="k">def</span> <span class="nf">wrapper</span><span class="p">(</span><span class="n">cut</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_source</span><span class="p">(</span><span class="n">cut</span><span class="o">.</span><span class="n">source</span><span class="o">.</span><span class="n">source_id</span><span class="p">)</span><span class="o">.</span><span class="n">limit_in_out</span><span class="p">(</span><span class="n">fn</span><span class="p">(</span><span class="n">cut</span><span class="p">))</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">cuts</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">cuts</span><span class="o">.</span><span class="n">modify</span><span class="p">(</span><span class="n">cut_id</span><span class="p">,</span> <span class="n">wrapper</span><span class="p">))</span>
</pre></div>
</div></div>
We now get this error:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>AttributeError: &#39;TextSource&#39; object has no attribute &#39;limit_in_out&#39;
</pre></div>
</div></div>
This is also to be expected. Now we need to implement `limit_in_out` on every
type of source. At the moment those are `TextSource` and `FileSource`. Let's
see if we have coverage for both. We get a failure for `TextSource` now, so
let's start there.

A text source does not have a length. It is infinite. So `limit_in_out` just
becomes this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">limit_in_out</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut</span><span class="p">):</span>
    <span class="k">return</span> <span class="n">cut</span>
</pre></div>
</div></div>
Now we get the same error for the file source.

I implement `FileSource.limit_in_out` like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">limit_in_out</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut</span><span class="p">):</span>
    <span class="k">return</span> <span class="n">cut</span><span class="o">.</span><span class="n">limit_out</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">length</span><span class="p">)</span>
</pre></div>
</div></div>
The test now complains about this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>AttributeError: &#39;Cut&#39; object has no attribute &#39;limit_out&#39;
</pre></div>
</div></div>
I implement it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">limit_out</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">max_out</span><span class="p">):</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">in_out</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">in_out</span><span class="o">.</span><span class="n">limit_end</span><span class="p">(</span><span class="n">max_out</span><span class="p">))</span>
</pre></div>
</div></div>
And `Region.limit_end` like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>    <span class="k">def</span> <span class="nf">limit_end</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">max_end</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">end</span><span class="o">=</span><span class="nb">min</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">end</span><span class="p">,</span> <span class="n">max_end</span><span class="p">))</span>
</pre></div>
</div></div>
And wow, that actually works.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;ProjectData.modify_cut ensures that in_out is withing source limit.&#39;
...................................................................
----------------------------------------------------------------------
Ran 67 tests in 3.925s

OK
[main a9eb857] ProjectData.modify_cut ensures that in_out is withing source limit.
 4 files changed, 29 insertions(+), 3 deletions(-)
</pre></div>
</div></div>
## Improving design

Right above `modify_cut` I see `add_cut` which also has a TODO:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">add_cut</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut</span><span class="p">):</span>
    <span class="c1"># TODO: assert that source id exists (even for json loading)</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">cuts</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">cuts</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="n">cut</span><span class="p">))</span>
</pre></div>
</div></div>
Now that we have touched this area of the code, let's have a closer look if we
can make something cleaner with our new insights.

The `add_cut` could probably also benefit from having the in and out points
limited.

However, it is not used by the JSON loading mechanism.

I move the JSON loading part of the comment to here:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="nd">@staticmethod</span>
<span class="k">def</span> <span class="nf">from_json</span><span class="p">(</span><span class="n">json</span><span class="p">):</span>
    <span class="c1"># TODO: validate the cuts point to valid sources and that they have</span>
    <span class="c1"># valid in/out points.</span>
    <span class="k">return</span> <span class="n">ProjectData</span><span class="p">(</span>
        <span class="n">sources</span><span class="o">=</span><span class="n">Sources</span><span class="o">.</span><span class="n">from_json</span><span class="p">(</span><span class="n">json</span><span class="p">[</span><span class="s2">&quot;sources&quot;</span><span class="p">]),</span>
        <span class="n">cuts</span><span class="o">=</span><span class="n">Cuts</span><span class="o">.</span><span class="n">from_json</span><span class="p">(</span><span class="n">json</span><span class="p">[</span><span class="s2">&quot;cuts&quot;</span><span class="p">])</span>
    <span class="p">)</span>
</pre></div>
</div></div>
I'm not sure that we want to adjust cuts that are invalid. We could remove cuts
that don't have a corresponding source, and we could adjust in and out points
of cuts with valid sources. But that would change the project. So a load +
save will save something else without the user having done any changes. Unless
manually modified, a JSON export should never have these problems. So
validation should be ok. But I said **should**. If we make a mistake somewhere,
we could export invalid JSON. So a load that fixes bad input it probably a good
idea. However, in such cases the user should probably be informed about the
changes made and a backup file with the old contents should probably be
written. I think this work is for a later time. Not really prioritized now.

Let's go back to `ProjectData.add_cut`. It is only used when the user actively
adds a cut somehow. At that point the cut does not exists yet, and if we modify
the in and out points, there is no obvious change.

Let's modify it guided by this test:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">In/Out is modified according to source:</span>

<span class="sd">&gt;&gt;&gt; ProjectData.empty(</span>
<span class="sd">... ).add_source(</span>
<span class="sd">...     FileSource(id=&quot;source_a&quot;, path=&quot;a.mp4&quot;, length=5)</span>
<span class="sd">... ).add_cut(</span>
<span class="sd">...     Cut.test_instance(name=&quot;source_a&quot;, start=0, end=10, id=&quot;cut_a&quot;)</span>
<span class="sd">... ).get_cut(&quot;cut_a&quot;).in_out</span>
<span class="sd">Region(start=0, end=5)</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
It fails with this message:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Differences (ndiff with -expected +actual):
    - Region(start=0, end=5)
    ?                     ^
    + Region(start=0, end=10)
    ?                     ^^
</pre></div>
</div></div>
We fix it in a similar way to before:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">_replace</span><span class="p">(</span><span class="n">cuts</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">cuts</span><span class="o">.</span><span class="n">add</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">get_source</span><span class="p">(</span><span class="n">cut</span><span class="o">.</span><span class="n">source</span><span class="o">.</span><span class="n">source_id</span><span class="p">)</span><span class="o">.</span><span class="n">limit_in_out</span><span class="p">(</span><span class="n">cut</span><span class="p">)))</span>
</pre></div>
</div></div>
That passes all the tests.

## Speed issue

I also noticed an issue with the limiting for cuts that had a changed speed. I
modify `FileSource.limit_in_out` to take speed into account:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">limit_in_out</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">cut</span><span class="p">):</span>
    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    &gt;&gt;&gt; source = FileSource(id=&quot;source_a&quot;, path=&quot;a.mp4&quot;, length=5)</span>

<span class="sd">    &gt;&gt;&gt; source.limit_in_out(Cut.test_instance(start=0, end=10)).in_out</span>
<span class="sd">    Region(start=0, end=5)</span>

<span class="sd">    &gt;&gt;&gt; source.limit_in_out(Cut.test_instance(start=0, end=20, speed=0.5)).in_out</span>
<span class="sd">    Region(start=0, end=10)</span>
<span class="sd">    &quot;&quot;&quot;</span>
    <span class="k">return</span> <span class="n">cut</span><span class="o">.</span><span class="n">limit_out</span><span class="p">(</span><span class="nb">int</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">length</span><span class="o">/</span><span class="n">cut</span><span class="o">.</span><span class="n">speed</span><span class="p">))</span>
</pre></div>
</div></div>
## Summary

We added a feature to the application. It is now possible to move the out point
of a cut and it is properly limited to not exceed the length of the underlying
source.

I was surprised at how elegant the solution came out. The realisation that made
this possible was that validation should happen at the point where all data
exists. Each data entity validates itself. If parent attributes are needed for
the validation, do the validation higher up the hierarchy.

This also makes me wonder if the limit of in point should also be done by the
source. Right now the cut assumes that in point >= 0 is ok. It doesn't need to
know anything about the source. But it makes assumptions about the source. I
think this assumption is always correct, but I don't think it hurts to not
assume anything and let the source do the decision.

I will probably try that refactoring out. My suspicion is that the code base
will be a little cleaner then.

But not in this session. This is it for now. See you next time!
