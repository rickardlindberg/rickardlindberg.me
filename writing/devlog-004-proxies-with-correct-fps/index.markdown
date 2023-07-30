---
title: 'DevLog 004: Proxies with correct FPS'
date: 2023-07-30
tags: devlog
devlog: true
---

In this episode we will continue work on the [video
editor](/projects/rlvideo/index.html). I have some footage that I would like to
edit. Wouldn't it be cool if I can do that in my own video editor? I will use
that as a guide for the development. What is stopping me from using my video
editor today? Fix that and move on to the next thing.

In this episode we will fix an issue with proxy clips sometimes having the
incorrect FPS.

## Why DevLogs?

I do them for various reasons. Here are the ones that I can think of now.

* I think there is value in documenting the work that I do.

* People reading these DevLogs might pick up something that I do and
  incorporate into their workflow.

* Clear thinking is clear writing and vice versa. Writing helps me think more
  clearly about topics. Sometimes, by writing about a problem, I think I can
  reach a solution faster even though writing takes time.

* I want to practice writing.

## The problem with proxies and FPS

For most of my videos, I use a frame rate of 25. That is 25 frames per second
(FPS). I have coded that as a default in the video editor.

However, sometimes I shoot footage in a higher frame rate and slow it down in
post.

Here is an example:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ffprobe GX010802.MP4 <span class="m">2</span>&gt;<span class="p">&amp;</span><span class="m">1</span> <span class="p">|</span> grep fps
  Stream <span class="c1">#0:0(eng): Video: hevc (Main) (hvc1 / 0x31637668), yuvj420p(pc, bt709), 2704x1520 [SAR 1:1 DAR 169:95], 97187 kb/s, 100 fps, 100 tbr, 90k tbn, 100 tbc (default)</span>
</pre></div>
</div></div>
You can see there in the middle that is says `100 fps`.

When I drop this clip on a 25 FPS timeline, only every 4th frame will be used
from that clip and the rest are discarded. However, if I slow down the clip to
25% speed, the runtime will be 4 times longer and all the frames will be used.

The problem is that the video editor uses proxy clips for preview. A proxy clip
is typically a lower resolution version of the original clip to allow real time
editing on a slower computer.

And proxy clips are currently generated with the same frame rate as the project
(which is 25).

Here is what `ffprobe` says about our proxy clip:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ffprobe /tmp/de63dcd626503cbde6f3da76b0af3e8c.mkv <span class="m">2</span>&gt;<span class="p">&amp;</span><span class="m">1</span> <span class="p">|</span> grep fps
  Stream <span class="c1">#0:0: Video: mjpeg (Baseline), yuvj420p(pc, bt470bg/bt709/bt709), 960x540 [SAR 1:1 DAR 16:9], 25 fps, 25 tbr, 1k tbn, 1k tbc (default)</span>
</pre></div>
</div></div>
## Proxy generation today

With some details removed, here is how proxies are generated today:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">producer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="n">CLIP_PATH</span><span class="p">)</span>

<span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="n">proxy_profile</span><span class="p">,</span> <span class="s2">&quot;avformat&quot;</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;target&quot;</span><span class="p">,</span> <span class="n">PROXY_PATH</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;vcodec&quot;</span><span class="p">,</span> <span class="s2">&quot;mjpeg&quot;</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;acodec&quot;</span><span class="p">,</span> <span class="s2">&quot;pcm_s16le&quot;</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;qscale&quot;</span><span class="p">,</span> <span class="s2">&quot;3&quot;</span><span class="p">)</span>

<span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
<span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
<span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
    <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
</pre></div>
</div></div>
The `profile` and the `proxy_profile` differ only in that the `proxy_profile`
has a lower resolution (width x height). They are otherwise identical.

What we need to do is to get the profile for a consumer and only change the
size of it. We want the profile FPS to be the FPS of the clip.

## Sidetracked

As I start writing some code, I notice something odd in the output of the
tests:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>.............................[matroska,webm @ 0x5599e1354fc0] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the &#39;analyzeduration&#39; (0) and &#39;probesize&#39; (5000000) options
[matroska,webm @ 0x5599e0f51540] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the &#39;analyzeduration&#39; (0) and &#39;probesize&#39; (5000000) options
....................
----------------------------------------------------------------------
Ran 49 tests in 2.001s
</pre></div>
</div></div>
I `git stash` my current changes and see that the output is still there.

I increase the verbosity of the test runner to figure out which test is causing
the output and I get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>Doctest: rlvideolib.domain.project.Project ... [matroska,webm @ 0x56544c299700] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the &#39;analyzeduration&#39; (0) and &#39;probesize&#39; (5000000) options
[matroska,webm @ 0x56544c4061c0] Could not find codec parameters for stream 0 (Video: mjpeg, none(pc, bt470bg/bt470bg/smpte170m), 720x576): unspecified pixel format
Consider increasing the value for the &#39;analyzeduration&#39; (0) and &#39;probesize&#39; (5000000) options
</pre></div>
</div></div>
The problem is that there is a sort of integration test that, when run,
generates proxy clips, and the output is not captured in the test and instead
redirected to the terminal.

I add the `capture_stdout_stderr` helper in the test, and the output now looks
clean.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; with capture_stdout_stderr():</span>
<span class="sd">...     with project.new_transaction() as transaction:</span>
<span class="sd">...         ...</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
Let's commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Capture stdout/stderr in Project test to not clutter the test output.&#39;
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.990s

OK
[main 9846016] Capture stdout/stderr in Project test to not clutter the test output.
 1 file changed, 4 insertions(+), 3 deletions(-)
</pre></div>
</div></div>
Sometimes when I encounter a small problem when working on something, I prefer
to `git stash` my changes, fix the small problem, and then get back to what I
was working on with `git stash pop`.

If the problem turns out to be not so small, I might write a note about it
instead.

## Back to the problem

I create this function to get a native producer and profile:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">mlt_producer_with_native_profile</span><span class="p">(</span><span class="n">path</span><span class="p">):</span>
    <span class="sd">&quot;&quot;&quot;</span>
<span class="sd">    &gt;&gt;&gt; _ = mlt.Factory().init()</span>
<span class="sd">    &gt;&gt;&gt; producer, profile = mlt_producer_with_native_profile(&quot;resources/one.mp4&quot;)</span>
<span class="sd">    &gt;&gt;&gt; profile.fps()</span>
<span class="sd">    25.0</span>
<span class="sd">    &quot;&quot;&quot;</span>
    <span class="n">profile</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Profile</span><span class="p">()</span>
    <span class="n">producer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
    <span class="n">profile</span><span class="o">.</span><span class="n">from_producer</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
    <span class="c1"># Re-open the producer with the new profile to ensure it gets all the</span>
    <span class="c1"># properties from it and does not retain properties from the old profile.</span>
    <span class="n">producer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
    <span class="k">return</span> <span class="p">(</span><span class="n">producer</span><span class="p">,</span> <span class="n">profile</span><span class="p">)</span>
</pre></div>
</div></div>
I don't have any clips in the resources folder that are other than 25 FPS, but
this at leas shows that my code doesn't crash.

I try to use it when generating proxies. The tests pass after my modification,
so I try to run the application with my 100 FPS test clip and get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ rm /tmp/*.mkv; rlvideo GX010802.MP4
...
  File &quot;/home/rick/rlvideo/rlvideolib/domain/source.py&quot;, line 59, in load_proxy
    assert self.length == native_producer.get_playtime()
</pre></div>
</div></div>
This reveals a problem to me. In the Python structures for a `Source` we store
its length. My intention was to store the number of frames in the file so that
we can check that we make valid cuts:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FileSource</span><span class="p">(</span><span class="n">namedtuple</span><span class="p">(</span><span class="s2">&quot;FileSource&quot;</span><span class="p">,</span> <span class="s2">&quot;id,path,length&quot;</span><span class="p">)):</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">create_cut</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">start</span><span class="p">,</span> <span class="n">end</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">start</span> <span class="o">&lt;</span> <span class="mi">0</span> <span class="ow">or</span> <span class="n">end</span> <span class="o">&gt;</span> <span class="bp">self</span><span class="o">.</span><span class="n">length</span><span class="p">:</span>
            <span class="k">raise</span> <span class="ne">ValueError</span><span class="p">(</span><span class="s2">&quot;Invalid cut.&quot;</span><span class="p">)</span>
        <span class="o">...</span>
</pre></div>
</div></div>
But I think the `producer.get_playtime()` is not giving frames, but rather
frames at the current frame rate. A quick test confirms that this is the case:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="sd">&quot;&quot;&quot;</span>
<span class="sd">&gt;&gt;&gt; profile = mlt.Profile()</span>
<span class="sd">&gt;&gt;&gt; profile.fps()</span>
<span class="sd">25.0</span>
<span class="sd">&gt;&gt;&gt; producer = mlt.Producer(profile, &quot;resources/one.mp4&quot;)</span>
<span class="sd">&gt;&gt;&gt; producer.get_playtime()</span>
<span class="sd">15</span>

<span class="sd">&gt;&gt;&gt; profile.set_frame_rate(50, 1)</span>
<span class="sd">&gt;&gt;&gt; profile.fps()</span>
<span class="sd">50.0</span>
<span class="sd">&gt;&gt;&gt; producer = mlt.Producer(profile, &quot;resources/one.mp4&quot;)</span>
<span class="sd">&gt;&gt;&gt; producer.get_playtime()</span>
<span class="sd">31</span>
<span class="sd">&quot;&quot;&quot;</span>
</pre></div>
</div></div>
What to do?

I think it's time for another `git stash` and clarify length.

## Clarify length

I want to rename `FileSource.length` to
`FileSource.number_of_frames_at_project_fps`. That is a really long name, but
it is more clear about what it represents. I value that more now.  After the
refactoring, I might uncover other issues.  Let's see.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Rename FileSource.length to FileSource.number_of_frames_at_project_fps.&#39;
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.994s

OK
[main 8baae0a] Rename FileSource.length to FileSource.number_of_frames_at_project_fps.
 2 files changed, 10 insertions(+), 10 deletions(-)
</pre></div>
</div></div>
The parameter is used in only one place outside `FileSource`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Transaction</span><span class="p">:</span>

    <span class="o">...</span>

    <span class="k">def</span> <span class="nf">add_clip</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">,</span> <span class="nb">id</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
        <span class="n">producer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">project</span><span class="o">.</span><span class="n">profile</span><span class="p">,</span> <span class="n">path</span><span class="p">)</span>
        <span class="n">source</span> <span class="o">=</span> <span class="n">FileSource</span><span class="p">(</span><span class="nb">id</span><span class="o">=</span><span class="nb">id</span><span class="p">,</span> <span class="n">path</span><span class="o">=</span><span class="n">path</span><span class="p">,</span> <span class="n">number_of_frames_at_project_fps</span><span class="o">=</span><span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">())</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">add_source</span><span class="p">(</span><span class="n">source</span><span class="p">,</span> <span class="n">source</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span><span class="p">)</span>
</pre></div>
</div></div>
I find it a little unclear the connection between a producer, its playtime, and
the number of frames. Let's see if we can make a helper to clarify this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">add_clip</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">,</span> <span class="nb">id</span><span class="o">=</span><span class="kc">None</span><span class="p">):</span>
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
And here is `FileInfo`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FileInfo</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">path</span> <span class="o">=</span> <span class="n">path</span>

    <span class="k">def</span> <span class="nf">get_number_of_frames</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
</pre></div>
</div></div>
This makes it a little more clear that the number of frames in a file depends
on the profile.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Extract FileInfo.&#39;
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.982s

OK
[main fd89715] Extract FileInfo.
 1 file changed, 15 insertions(+), 2 deletions(-)
</pre></div>
</div></div>
## Change project FPS after?

This brings up the question if we can change the project frame rate after we
have added some clips.

My guess is not.

I remember reading that you should never do this in Kdenlive. Then weird things
will happen.

I suppose we could try to re-calculate all positions and lengths when we change
the frame rate.  Or have the unit of measurement be time instead. But I think
that will be hard since that is not what MLT works with, and also, it make
sense to work in terms of frames.

I add a note in the source code about this and move on. This is probably fine.

## Back

I `git stash pop` my earlier changes. Because of the rename, I have to resolve
conflicts, but it goes well.

I then spot this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="n">native_producer</span><span class="p">,</span> <span class="n">native_profile</span> <span class="o">=</span> <span class="n">mlt_producer_with_native_profile</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span>
<span class="k">assert</span> <span class="bp">self</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span> <span class="o">==</span> <span class="n">native_producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
</pre></div>
</div></div>
With our new knowledge, this is obviously wrong. And the new name helps us see
that. The native profile has the FPS of the clip whereas the project profile
has the FPS of the project. Those might not be the same, so therefore the
assertion is not always going to work.

I see some more usages for `FileInfo`, so I yet again stash my changes and
update `FileInfo` to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">FileInfo</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">path</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">path</span> <span class="o">=</span> <span class="n">path</span>

    <span class="k">def</span> <span class="nf">get_number_of_frames</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">):</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">get_mlt_producer</span><span class="p">(</span><span class="n">profile</span><span class="p">)</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">get_mlt_producer</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span>
</pre></div>
</div></div>
Commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m <span class="s1">&#39;Allow clearer code by extending FileInfo.&#39;</span>
.................................................
----------------------------------------------------------------------
Ran <span class="m">49</span> tests <span class="k">in</span> <span class="m">1</span>.996s

OK
<span class="o">[</span>main acb6702<span class="o">]</span> Allow clearer code by extending FileInfo.
 <span class="m">4</span> files changed, <span class="m">18</span> insertions<span class="o">(</span>+<span class="o">)</span>, <span class="m">11</span> deletions<span class="o">(</span>-<span class="o">)</span>
 create mode <span class="m">100644</span> rlvideolib/mlthelpers.py
</pre></div>
</div></div>
## Break

I'm having a hard time reasoning about proxy generation code. I decide it's
time for a break.

## Another strategy

So far we have made no actual progress on improving proxy generation, but we
have cleaned up the code in related areas and gained some new knowledge.

Since I was not able to fix the proxy generation in a small step, I decide to
change approach and take much smaller steps.

Let's see if we can refactor the `load_proxy` method and perhaps we might see
more clearly how to modify it.

For reference, this is what it looks now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">load_proxy</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">,</span> <span class="n">proxy_profile</span><span class="p">,</span> <span class="n">progress</span><span class="p">):</span>
    <span class="n">producer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span>
    <span class="k">assert</span> <span class="bp">self</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span> <span class="o">==</span> <span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
    <span class="n">chechsum</span> <span class="o">=</span> <span class="n">md5</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span>
    <span class="n">proxy_path</span> <span class="o">=</span> <span class="sa">f</span><span class="s2">&quot;/tmp/</span><span class="si">{</span><span class="n">chechsum</span><span class="si">}</span><span class="s2">.mkv&quot;</span>
    <span class="n">proxy_tmp_path</span> <span class="o">=</span> <span class="sa">f</span><span class="s2">&quot;/tmp/</span><span class="si">{</span><span class="n">chechsum</span><span class="si">}</span><span class="s2">.tmp.mkv&quot;</span>
    <span class="k">if</span> <span class="ow">not</span> <span class="n">os</span><span class="o">.</span><span class="n">path</span><span class="o">.</span><span class="n">exists</span><span class="p">(</span><span class="n">proxy_path</span><span class="p">):</span>
        <span class="n">consumer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Consumer</span><span class="p">(</span><span class="n">proxy_profile</span><span class="p">,</span> <span class="s2">&quot;avformat&quot;</span><span class="p">)</span>
        <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;target&quot;</span><span class="p">,</span> <span class="n">proxy_tmp_path</span><span class="p">)</span>
        <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;vcodec&quot;</span><span class="p">,</span> <span class="s2">&quot;mjpeg&quot;</span><span class="p">)</span>
        <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;acodec&quot;</span><span class="p">,</span> <span class="s2">&quot;pcm_s16le&quot;</span><span class="p">)</span>
        <span class="n">consumer</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="s2">&quot;qscale&quot;</span><span class="p">,</span> <span class="s2">&quot;3&quot;</span><span class="p">)</span>
        <span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
        <span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
        <span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
            <span class="n">progress</span><span class="p">(</span><span class="n">producer</span><span class="o">.</span><span class="n">position</span><span class="p">()</span><span class="o">/</span><span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">())</span>
            <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
        <span class="n">os</span><span class="o">.</span><span class="n">rename</span><span class="p">(</span><span class="n">proxy_tmp_path</span><span class="p">,</span> <span class="n">proxy_path</span><span class="p">)</span>
    <span class="n">producer</span> <span class="o">=</span> <span class="n">mlt</span><span class="o">.</span><span class="n">Producer</span><span class="p">(</span><span class="n">profile</span><span class="p">,</span> <span class="n">proxy_path</span><span class="p">)</span>
    <span class="k">assert</span> <span class="bp">self</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span> <span class="o">==</span> <span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span>
    <span class="k">return</span> <span class="n">producer</span>
</pre></div>
</div></div>
Let's try to extract `get_file_info` (which can then also be used in another
place):

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">get_file_info</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">):</span>
    <span class="n">file_info</span> <span class="o">=</span> <span class="n">FileInfo</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">path</span><span class="p">)</span>
    <span class="k">assert</span> <span class="bp">self</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span> <span class="o">==</span> <span class="n">file_info</span><span class="o">.</span><span class="n">get_number_of_frames</span><span class="p">(</span><span class="n">profile</span><span class="p">)</span>
    <span class="k">return</span> <span class="n">file_info</span>
</pre></div>
</div></div>
In `load_proxy` we can use it like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gd">-        producer = mlt.Producer(profile, self.path)</span>
<span class="gd">-        assert self.number_of_frames_at_project_fps == producer.get_playtime()</span>
<span class="gi">+        file_info = self.get_file_info(profile)</span>
         chechsum = md5(self.path)
         proxy_path = f&quot;/tmp/{chechsum}.mkv&quot;
         proxy_tmp_path = f&quot;/tmp/{chechsum}.tmp.mkv&quot;
         if not os.path.exists(proxy_path):
<span class="gi">+            producer = file_info.get_mlt_producer(profile)</span>
             consumer = mlt.Consumer(proxy_profile, &quot;avformat&quot;)
             consumer.set(&quot;target&quot;, proxy_tmp_path)
             consumer.set(&quot;vcodec&quot;, &quot;mjpeg&quot;)
</pre></div>
</div></div>
This ensures that the file has the same length as we have recorded.

Let's extract `run_consumer`:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">run_consumer</span><span class="p">(</span><span class="n">consumer</span><span class="p">,</span> <span class="n">producer</span><span class="p">,</span> <span class="n">progress</span><span class="p">):</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">connect</span><span class="p">(</span><span class="n">producer</span><span class="p">)</span>
    <span class="n">consumer</span><span class="o">.</span><span class="n">start</span><span class="p">()</span>
    <span class="k">while</span> <span class="n">consumer</span><span class="o">.</span><span class="n">is_stopped</span><span class="p">()</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
        <span class="n">progress</span><span class="p">(</span><span class="n">producer</span><span class="o">.</span><span class="n">position</span><span class="p">()</span><span class="o">/</span><span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">())</span>
        <span class="n">time</span><span class="o">.</span><span class="n">sleep</span><span class="p">(</span><span class="mf">0.5</span><span class="p">)</span>
</pre></div>
</div></div>
I forgot to commit last refactoring. Let's do that now:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Extract get_file_info and run_consumer.&#39;
.................................................
----------------------------------------------------------------------
Ran 49 tests in 1.983s

OK
[main 49df31e] Extract get_file_info and run_consumer.
 2 files changed, 20 insertions(+), 11 deletions(-)
</pre></div>
</div></div>
There is a test for proxy generation, but it does not run fully if the proxy
file already exists. I add a testing flag that we can set to True in tests. I'm
not sure I like this, but it will help us when refactoring this method:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">load_proxy</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">profile</span><span class="p">,</span> <span class="n">proxy_profile</span><span class="p">,</span> <span class="n">progress</span><span class="p">,</span> <span class="n">testing</span><span class="o">=</span><span class="kc">False</span><span class="p">):</span>
    <span class="o">...</span>
    <span class="n">proxy_tmp_path</span> <span class="o">=</span> <span class="sa">f</span><span class="s2">&quot;/tmp/</span><span class="si">{</span><span class="n">chechsum</span><span class="si">}</span><span class="s2">.tmp.mkv&quot;</span>
    <span class="k">if</span> <span class="ow">not</span> <span class="n">os</span><span class="o">.</span><span class="n">path</span><span class="o">.</span><span class="n">exists</span><span class="p">(</span><span class="n">proxy_path</span><span class="p">)</span> <span class="ow">or</span> <span class="n">testing</span><span class="p">:</span>
        <span class="n">producer</span> <span class="o">=</span> <span class="n">file_info</span><span class="o">.</span><span class="n">get_mlt_producer</span><span class="p">(</span><span class="n">profile</span><span class="p">)</span>
        <span class="o">...</span>
</pre></div>
</div></div>
## Break

I keep trying to clean up the proxy loading code but I just can't seem to find
the right abstractions. Furthermore, I get segfaults and all kinds of strange
behavior from MLT. This demotivates me. I force myself to take a break.

## Revert

Since MLT is giving me all kinds of weird behavior, I think that perhaps the
`get_file_info` abstraction was wrong. Maybe it creates more trouble at the
moment. Let's see if we can inline some of it instead.

We get this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gu">@@ -65,12 +65,11 @@ class FileSource(namedtuple(&quot;FileSource&quot;, &quot;id,path,number_of_frames_at_project_f</span>
         &quot;&quot;&quot;
         # TODO: generate proxy with same profile as source clip (same colorspace, etc,
         # but with smaller size)
<span class="gd">-        file_info = self.get_file_info(profile)</span>
<span class="gi">+        producer = self.validate_producer(mlt.Producer(profile, self.path))</span>
         chechsum = md5(self.path)
         proxy_path = f&quot;/tmp/{chechsum}.mkv&quot;
         proxy_tmp_path = f&quot;/tmp/{chechsum}.tmp.mkv&quot;
         if not os.path.exists(proxy_path) or testing:
<span class="gd">-            producer = file_info.get_mlt_producer(profile)</span>
             consumer = mlt.Consumer(proxy_profile, &quot;avformat&quot;)
             consumer.set(&quot;target&quot;, proxy_tmp_path)
             consumer.set(&quot;vcodec&quot;, &quot;mjpeg&quot;)
<span class="gu">@@ -78,14 +77,11 @@ class FileSource(namedtuple(&quot;FileSource&quot;, &quot;id,path,number_of_frames_at_project_f</span>
             consumer.set(&quot;qscale&quot;, &quot;3&quot;)
             run_consumer(consumer, producer, progress)
             os.rename(proxy_tmp_path, proxy_path)
<span class="gd">-        producer = mlt.Producer(profile, proxy_path)</span>
<span class="gd">-        assert self.number_of_frames_at_project_fps == producer.get_playtime()</span>
<span class="gd">-        return producer</span>
<span class="gi">+        return self.validate_producer(mlt.Producer(profile, proxy_path))</span>
</pre></div>
</div></div>
Where `validate_producer` is this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">validate_producer</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">producer</span><span class="p">):</span>
    <span class="k">assert</span> <span class="n">producer</span><span class="o">.</span><span class="n">get_playtime</span><span class="p">()</span> <span class="o">==</span> <span class="bp">self</span><span class="o">.</span><span class="n">number_of_frames_at_project_fps</span>
    <span class="k">return</span> <span class="n">producer</span>
</pre></div>
</div></div>
Commit:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Inline some of FileInfo.&#39;
.................................................
----------------------------------------------------------------------
Ran 49 tests in 2.493s

OK
[main c47ea68] Inline some of FileInfo.
 2 files changed, 7 insertions(+), 14 deletions(-)
</pre></div>
</div></div>
Then finally I can make this relatively small change:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="gu">@@ -70,7 +70,12 @@ class FileSource(namedtuple(&quot;FileSource&quot;, &quot;id,path,number_of_frames_at_project_f</span>
         proxy_path = f&quot;/tmp/{chechsum}.mkv&quot;
         proxy_tmp_path = f&quot;/tmp/{chechsum}.tmp.mkv&quot;
         if not os.path.exists(proxy_path) or testing:
<span class="gd">-            consumer = mlt.Consumer(proxy_profile, &quot;avformat&quot;)</span>
<span class="gi">+            p = mlt.Profile()</span>
<span class="gi">+            p.from_producer(producer)</span>
<span class="gi">+            p.set_width(proxy_profile.width())</span>
<span class="gi">+            p.set_height(proxy_profile.height())</span>
<span class="gi">+            producer = mlt.Producer(p, self.path)</span>
<span class="gi">+            consumer = mlt.Consumer(p, &quot;avformat&quot;)</span>
             consumer.set(&quot;target&quot;, proxy_tmp_path)
             consumer.set(&quot;vcodec&quot;, &quot;mjpeg&quot;)
             consumer.set(&quot;acodec&quot;, &quot;pcm_s16le&quot;)
</pre></div>
</div></div>
With this, proxy clips now retain their FPS:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ffprobe /tmp/de63dcd626503cbde6f3da76b0af3e8c.mkv 2&gt;&amp;1 | grep fps
  Stream #0:0: Video: mjpeg (Baseline), yuvj420p(pc, bt470bg/bt709/bt709), 960x540 [SAR 1:1 DAR 16:9], 100 fps, 100 tbr, 1k tbn, 1k tbc (default)
</pre></div>
</div></div>
It seems to work fine in the application as well.

Let's commit this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>$ ./make.py commit -m &#39;Produce proxy clips with native profile to preserve FPS.&#39;
.................................................
----------------------------------------------------------------------
Ran 49 tests in 2.560s

OK
[main b69cfb7] Produce proxy clips with native profile to preserve FPS.
 1 file changed, 6 insertions(+), 3 deletions(-)
</pre></div>
</div></div>
## Summary

This session turned out to be rather painful. Every time I do something that
involves MLT, things get painful. That tells med to isolate as much of the MLT
code as possible. It also tells me that I need to learn MLT better to
understand issues.

But segfaults worry me a bit. When working in Python, we should really not be
getting segfaults. Is there something wrong in the Python binding for MLT?

I'm sure we have to revisit proxy generation at some point. But I'm done for
now.
