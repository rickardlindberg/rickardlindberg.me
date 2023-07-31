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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gu">@@ -70,15 +70,16 @@ class FileSource(namedtuple(&quot;FileSource&quot;, &quot;id,path,number_of_frames_at_project_f</span>
         proxy_tmp_path = proxy_spec.get_tmp_path(checksum)
         if not os.path.exists(proxy_path):
             proxy_spec.ensure_dir()
<span class="gd">-            p = mlt.Profile()</span>
<span class="gd">-            p.from_producer(producer)</span>
<span class="gd">-            proxy_spec.adjust_profile(p)</span>
<span class="gd">-            producer = mlt.Producer(p, self.path)</span>
<span class="gd">-            consumer = mlt.Consumer(p, &quot;avformat&quot;)</span>
<span class="gd">-            consumer.set(&quot;target&quot;, proxy_tmp_path)</span>
<span class="gd">-            proxy_spec.adjust_consumer(consumer)</span>
<span class="gd">-            run_consumer(consumer, producer, progress)</span>
<span class="gd">-            self.create_producer(profile, proxy_tmp_path)</span>
<span class="gi">+            subprocess.check_call([</span>
<span class="gi">+                &quot;ffmpeg&quot;,</span>
<span class="gi">+                &quot;-y&quot;,</span>
<span class="gi">+                &quot;-i&quot;, self.path,</span>
<span class="gi">+                &quot;-vf&quot;, &quot;yadif,scale=960:540&quot;,</span>
<span class="gi">+                &quot;-q:v&quot;, &quot;3&quot;,</span>
<span class="gi">+                &quot;-vcodec&quot;, &quot;mjpeg&quot;,</span>
<span class="gi">+                &quot;-acodec&quot;, &quot;pcm_s16le&quot;,</span>
<span class="gi">+                proxy_tmp_path</span>
<span class="gi">+            ])</span>
             os.rename(proxy_tmp_path, proxy_path)
         return self.create_producer(profile, proxy_path)
</pre></div>
</div></div>
Let's commit that and see how we can refactor in this area.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Use FFmpeg for proxy generation.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.959s

OK
[main 078c9f2] Use FFmpeg for proxy generation.
 1 file changed, 10 insertions(+), 9 deletions(-)
</pre></div>
</div></div>
## The start of Clip

The proxy generation code has this line:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">checksum</span> <span class="o">=</span> <span class="n">md5</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span>
</pre></div>
</div></div>
Where `md5` is a top level function.

Let's extract a `Clip` class and put the `md5` method there. My idea is that a
clip represents a file on disk that we can load into a `FileSource` and display
on our timeline.

Here it is:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Clip</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">path</span> <span class="o">=</span> <span class="n">path</span>

    <span class="k">def</span> <span class="nf">md5</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">subprocess</span><span class="o">.</span><span class="n">check_output</span><span class="p">([</span><span class="s2">&quot;md5sum&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">])[:</span><span class="mi">32</span><span class="p">]</span><span class="o">.</span><span class="n">decode</span><span class="p">(</span><span class="s2">&quot;ascii&quot;</span><span class="p">)</span>
</pre></div>
</div></div>
We can see again that we use an external program, `md5sum`, to calculate the
md5. I think I did that because the Python module for calculating md5 did not
have a convenient function for calculating the sum on large files. As I noted
previously, I'm fine with this. We should probably add a test though to make
sure it works with the external program.

Anyway, the proxy code can now be written like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">checksum</span> <span class="o">=</span> <span class="n">Clip</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span><span class="o">.</span><span class="n">md5</span><span class="p">()</span>
</pre></div>
</div></div>
Perfect! Time to commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Extract Clip and move the md5 function to it.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.966s

OK
[main 6488c05] Extract Clip and move the md5 function to it.
 3 files changed, 12 insertions(+), 4 deletions(-)
 create mode 100644 rlvideolib/domain/clip.py
</pre></div>
</div></div>
## Revising mlthelpers

When working on proxies before, we extracted this module:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">rlvideolib</span><span class="o">.</span><span class="n">mlthelpers</span>
</pre></div>
</div></div>
First of all, its `run_consumer` function is no longer used when we generate
proxies with FFmpeg. Let's remove it.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Remove rlvideolib.mlthelpers.run_consumer since it is no longer used.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.974s

OK
[main aff41ef] Remove rlvideolib.mlthelpers.run_consumer since it is no longer used.
 2 files changed, 9 deletions(-)
</pre></div>
</div></div>
The only thing left now is this class:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FileInfo</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">path</span> <span class="o">=</span> <span class="n">path</span>

    <span class="k">def</span> <span class="nf">get_number_of_frames</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
</pre></div>
</div></div>
This looks a lot like it can be merged into our new `Clip`. Let's see where it
is used.

I find an unused import of it and remove it.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Remove unused import of FileInfo.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.967s

OK
[main dda14cf] Remove unused import of FileInfo.
 1 file changed, 1 deletion(-)
</pre></div>
</div></div>
I wonder if there is a tool that can automatically remove unused imports. Then
we can include it in our test runner perhaps? Along with an auto formatter?

Anyway, the only other use is here:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Transaction</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">add_clip</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">,</span> <span class="nb">id</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
        <span class="n">source</span> <span class="o">=</span> <span class="n">FileSource</span><span class="p">(</span>
            <span class="nb">id</span><span class="o">=</span><span class="nb">id</span><span class="p">,</span>
            <span class="n">path</span><span class="o">=</span><span class="n">path</span><span class="p">,</span>
            <span class="n">number_of_frames_at_project_fps</span><span class="o">=</span><span class="n">FileInfo</span><span class="p">(</span>
                <span class="n">path</span>
            <span class="p">)</span><span class="o">.</span><span class="n">get_number_of_frames</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">profile</span><span class="p">)</span>
        <span class="p">)</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">add_source</span><span class="p">(</span><span class="n">source</span><span class="p">,</span> <span class="n">source</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span><span class="p">)</span>
</pre></div>
</div></div>
Here we are actually talking about adding a clip. I had forgotten about that.
It would make more sense to create a `Clip` then instead of a `FileInfo`. Let's
add `get_number_of_frames` to `Clip` and then we can get rid of `FileInfo` and
`rlvideolib.mlthelpers` completely.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Move get_number_of_frames to Clip.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.955s

OK
[main c13f4cb] Move get_number_of_frames to Clip.
 4 files changed, 7 insertions(+), 14 deletions(-)
 delete mode 100644 rlvideolib/mlthelpers.py
</pre></div>
</div></div>
## Thinking about number of frames

We saw in an [earlier
devlog](/writing/devlog-004-proxies-with-correct-fps/index.html) that the
number of frames that we store is actually the number of frames at the current
project FPS. I dig into the MLT source code and see that the out point seems to
be calculated based on the FPS:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">mlt_position</span> <span class="n">frames</span> <span class="o">=</span> <span class="p">(</span><span class="n">mlt_position</span><span class="p">)</span> <span class="n">lrint</span><span class="p">(</span><span class="n">format</span><span class="o">-&gt;</span><span class="n">duration</span> <span class="o">*</span> <span class="n">mlt_profile_fps</span><span class="p">(</span><span class="n">profile</span><span class="p">)</span>
                                           <span class="o">/</span> <span class="n">AV_TIME_BASE</span><span class="p">);</span>
<span class="k">if</span> <span class="p">(</span><span class="n">mlt_properties_get_position</span><span class="p">(</span><span class="n">properties</span><span class="p">,</span> <span class="s">&quot;out&quot;</span><span class="p">)</span> <span class="o">&lt;=</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">mlt_properties_set_position</span><span class="p">(</span><span class="n">properties</span><span class="p">,</span> <span class="s">&quot;out&quot;</span><span class="p">,</span> <span class="n">frames</span> <span class="o">-</span> <span class="mi">1</span><span class="p">);</span>
<span class="k">if</span> <span class="p">(</span><span class="n">mlt_properties_get_position</span><span class="p">(</span><span class="n">properties</span><span class="p">,</span> <span class="s">&quot;length&quot;</span><span class="p">)</span> <span class="o">&lt;=</span> <span class="mi">0</span><span class="p">)</span>
    <span class="n">mlt_properties_set_position</span><span class="p">(</span><span class="n">properties</span><span class="p">,</span> <span class="s">&quot;length&quot;</span><span class="p">,</span> <span class="n">frames</span><span class="p">);</span>
</pre></div>
</div></div>
I wonder if we can do the same calculation so that we don't have to depend on
MLT for `get_number_of_frames`?

I'm not ready for that. Let's rewrite

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">get_number_of_frames</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">):</span>
    <span class="k">return</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
</pre></div>
</div></div>
to this

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">calculate_length_at_fps</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">mlt_profile</span><span class="p">):</span>
    <span class="k">return</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">mlt_profile</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
</pre></div>
</div></div>
to clarify a bit more what it is actually doing.

With this change, I think that the previous rename we did of
`FileSource.length` to `FileSource.number_of_frames_at_project_fps` can be
reverted. I think it makes sense to talk about a length in terms of frames. If
we change the project FPS, all lengths have to be recalculated, and I think
this makes sense. With the new `calculate_length_at_fps` it is still clear that
this length depends on the FPS I think.

Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Rename Clip..get_number_of_frames to Clip..calculate_length_at_fps.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.457s

OK
[main 94de3c1] Rename Clip..get_number_of_frames to Clip..calculate_length_at_fps.
 2 files changed, 3 insertions(+), 3 deletions(-)
</pre></div>
</div></div>
<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Rename FileSource.number_of_frames_at_project_fps to FileSource.length.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.952s

OK
[main 5bca102] Rename FileSource.number_of_frames_at_project_fps to FileSource.length.
 2 files changed, 12 insertions(+), 12 deletions(-)
</pre></div>
</div></div>
Now when `Transaction.add_clip` reads like this

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">add_clip</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">,</span> <span class="nb">id</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
    <span class="n">source</span> <span class="o">=</span> <span class="n">FileSource</span><span class="p">(</span>
        <span class="nb">id</span><span class="o">=</span><span class="nb">id</span><span class="p">,</span>
        <span class="n">path</span><span class="o">=</span><span class="n">path</span><span class="p">,</span>
        <span class="n">length</span><span class="o">=</span><span class="n">Clip</span><span class="p">(</span>
            <span class="n">path</span>
        <span class="p">)</span><span class="o">.</span><span class="n">calculate_length_at_fps</span><span class="p">(</span><span class="n">mlt_profile</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">profile</span><span class="p">)</span>
    <span class="p">)</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">add_source</span><span class="p">(</span><span class="n">source</span><span class="p">,</span> <span class="n">source</span><span class="o">.</span><span class="n">length</span><span class="p">)</span>
</pre></div>
</div></div>
I don't think we need this comment anymore:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="c1"># NOTE: The length depends on the FPS of the project. Once the first</span>
<span class="c1"># FileSource is added to the project, the FPS of the project can not be</span>
<span class="c1"># changed.</span>
</pre></div>
</div></div>
I think the code above makes it clear enough that the length depends on the FPS
of the project.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Remove note about length/FPS since this is clear enough in the code now.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.955s

OK
[main ab10ec9] Remove note about length/FPS since this is clear enough in the code now.
 1 file changed, 4 deletions(-)
</pre></div>
</div></div>
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

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Clip</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">generate_proxy</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">proxy_spec</span><span class="p">,</span> <span class="n">progress</span><span class="p">):</span>
        <span class="c1"># TODO: call progress</span>
        <span class="n">checksum</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">md5</span><span class="p">()</span>
        <span class="n">proxy_path</span> <span class="o">=</span> <span class="n">proxy_spec</span><span class="o">.</span><span class="n">get_path</span><span class="p">(</span><span class="n">checksum</span><span class="p">)</span>
        <span class="n">proxy_tmp_path</span> <span class="o">=</span> <span class="n">proxy_spec</span><span class="o">.</span><span class="n">get_tmp_path</span><span class="p">(</span><span class="n">checksum</span><span class="p">)</span>
        <span class="k">if</span> <span class="ow">not</span> <span class="n">os</span><span class="o">.</span><span class="n">path</span><span class="o">.</span><span class="n">exists</span><span class="p">(</span><span class="n">proxy_path</span><span class="p">):</span>
            <span class="n">proxy_spec</span><span class="o">.</span><span class="n">ensure_dir</span><span class="p">()</span>
            <span class="n">subprocess</span><span class="o">.</span><span class="n">check_call</span><span class="p">([</span>
                <span class="s2">&quot;ffmpeg&quot;</span><span class="p">,</span>
                <span class="s2">&quot;-y&quot;</span><span class="p">,</span>
                <span class="s2">&quot;-i&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">,</span>
                <span class="s2">&quot;-vf&quot;</span><span class="p">,</span> <span class="s2">&quot;yadif,scale=960:540&quot;</span><span class="p">,</span>
                <span class="s2">&quot;-q:v&quot;</span><span class="p">,</span> <span class="s2">&quot;3&quot;</span><span class="p">,</span>
                <span class="s2">&quot;-vcodec&quot;</span><span class="p">,</span> <span class="s2">&quot;mjpeg&quot;</span><span class="p">,</span>
                <span class="s2">&quot;-acodec&quot;</span><span class="p">,</span> <span class="s2">&quot;pcm_s16le&quot;</span><span class="p">,</span>
                <span class="n">proxy_tmp_path</span>
            <span class="p">])</span>
            <span class="n">os</span><span class="o">.</span><span class="n">rename</span><span class="p">(</span><span class="n">proxy_tmp_path</span><span class="p">,</span> <span class="n">proxy_path</span><span class="p">)</span>
        <span class="k">return</span> <span class="n">proxy_path</span>
</pre></div>
</div></div>
And `FileSource.load_proxy` can be reduced to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">load_proxy</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">,</span> <span class="n">proxy_spec</span><span class="p">,</span> <span class="n">progress</span><span class="p">):</span>
    <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">create_producer</span><span class="p">(</span>
        <span class="n">profile</span><span class="p">,</span>
        <span class="n">Clip</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span><span class="o">.</span><span class="n">generate_proxy</span><span class="p">(</span><span class="n">proxy_spec</span><span class="p">,</span> <span class="n">progress</span><span class="p">)</span>
    <span class="p">)</span>
</pre></div>
</div></div>
This reads quite nicely I think. Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Extract Clip.generate_proxy.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.929s

OK
[main cc85022] Extract Clip.generate_proxy.
 2 files changed, 25 insertions(+), 18 deletions(-)
</pre></div>
</div></div>
## Proxy spec location

The `ProxySpec` class currently lives in `rlvideolib.domain.project`. I think a
much better place would be `rlvideolib.domain.clip`.

Let's move it over.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Move ProxySpec to rlvideolib.domain.clip.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 2.903s

OK
[main 36932f0] Move ProxySpec to rlvideolib.domain.clip.
 2 files changed, 50 insertions(+), 49 deletions(-)
</pre></div>
</div></div>
## Proxy spec cleanup

The proxy spec has a method for setting properties on a consumer:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">adjust_consumer</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">consumer</span><span class="p">):</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;vcodec&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">vcodec</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;acodec&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">acodec</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;qscale&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">qscale</span><span class="p">)</span>
</pre></div>
</div></div>
Now when we use FFmpeg for proxy generation, this is no longer used.

But we did hardcode some values for FFmpeg, let's change `adjust_consumer` to
`get_ffmpeg_arguments`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">get_ffmpeg_arguments</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
    <span class="k">return</span> <span class="p">[</span>
        <span class="s2">&quot;-vf&quot;</span><span class="p">,</span> <span class="sa">f</span><span class="s2">&quot;yadif,scale=-1:</span><span class="si">{</span><span class="bp">self</span><span class="o">.</span><span class="n">height</span><span class="si">}</span><span class="s2">&quot;</span><span class="p">,</span>
        <span class="s2">&quot;-q:v&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">qscale</span><span class="p">,</span>
        <span class="s2">&quot;-vcodec&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">vcodec</span><span class="p">,</span>
        <span class="s2">&quot;-acodec&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">acodec</span><span class="p">,</span>
    <span class="p">]</span>
</pre></div>
</div></div>
And use it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">generate_proxy</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">proxy_spec</span><span class="p">,</span> <span class="n">progress</span><span class="p">):</span>

    <span class="o">...</span>

    <span class="n">subprocess</span><span class="o">.</span><span class="n">check_call</span><span class="p">(</span>
        <span class="p">[</span>
            <span class="s2">&quot;ffmpeg&quot;</span><span class="p">,</span>
            <span class="s2">&quot;-y&quot;</span><span class="p">,</span>
            <span class="s2">&quot;-i&quot;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">,</span>
        <span class="p">]</span>
        <span class="o">+</span>
        <span class="n">proxy_spec</span><span class="o">.</span><span class="n">get_ffmpeg_arguments</span><span class="p">()</span>
        <span class="o">+</span>
        <span class="p">[</span>
            <span class="n">proxy_tmp_path</span>
        <span class="p">]</span>
    <span class="p">)</span>
</pre></div>
</div></div>
Commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;ProxySpec know how to generate FFmpeg arguments for conversion.&#39;
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.412s

OK
[main 6579178] ProxySpec know how to generate FFmpeg arguments for conversion.
 1 file changed, 20 insertions(+), 14 deletions(-)
</pre></div>
</div></div>
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
