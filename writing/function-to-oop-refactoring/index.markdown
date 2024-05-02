---
title: Refactoring a function to 6 classes
date: 2024-05-02
tags: refactoring,oop
---

I made a video where I show how I refactor a single function, that does many
things, to 6 classes that each does a single thing.

<center>
<iframe width="560" height="315"
src="https://www.youtube.com/embed/rubTUD0EdME" title="YouTube video player"
frameborder="0" allow="accelerometer; autoplay; clipboard-write;
encrypted-media; gyroscope; picture-in-picture; web-share"
allowfullscreen></iframe>
</center>

The resulting design is more object oriented.

I don't want to argue which is better, but instead show you what an object
oriented design can look like, because I feel like those examples are rare.

The example I'm refactoring is a function that returns the next version number
given a set of existing versions numbers stored as git tags.

In the first example, we ask for the next release version in the 1.0 series
given that no tags exist. We get the default version 1.0.0.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="o">&gt;&gt;&gt;</span> <span class="n">nextversion</span><span class="p">(</span><span class="n">series</span><span class="o">=</span><span class="s2">&quot;1.0&quot;</span><span class="p">,</span> <span class="n">pre_release</span><span class="o">=</span><span class="kc">False</span><span class="p">,</span> <span class="n">tags</span><span class="o">=</span><span class="p">[])</span>
<span class="s1">&#39;1.0.0&#39;</span>
</pre></div>
</div></div>
In the second example, version 1.0.0 already exists, and we therefore get
version 1.0.1.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="o">&gt;&gt;&gt;</span> <span class="n">nextversion</span><span class="p">(</span><span class="n">series</span><span class="o">=</span><span class="s2">&quot;1.0&quot;</span><span class="p">,</span> <span class="n">pre_release</span><span class="o">=</span><span class="kc">False</span><span class="p">,</span> <span class="n">tags</span><span class="o">=</span><span class="p">[</span><span class="s1">&#39;1.0.0&#39;</span><span class="p">])</span>
<span class="s1">&#39;1.0.1&#39;</span>
</pre></div>
</div></div>
In the third example we ask for the next pre-release version. The next release
version would be 1.0.2, and so the first pre-release version of that release is
1.0.2-1.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="o">&gt;&gt;&gt;</span> <span class="n">nextversion</span><span class="p">(</span><span class="n">series</span><span class="o">=</span><span class="s2">&quot;1.0&quot;</span><span class="p">,</span> <span class="n">pre_release</span><span class="o">=</span><span class="kc">True</span><span class="p">,</span> <span class="n">tags</span><span class="o">=</span><span class="p">[</span><span class="s1">&#39;1.0.0&#39;</span><span class="p">,</span> <span class="s1">&#39;1.0.1&#39;</span><span class="p">])</span>
<span class="s1">&#39;1.0.2-1&#39;</span>
</pre></div>
</div></div>
In the fourth example, pre-release 3 already exists, so the next pre-release is
1.0.2-4.

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="o">&gt;&gt;&gt;</span> <span class="n">nextversion</span><span class="p">(</span><span class="n">series</span><span class="o">=</span><span class="s2">&quot;1.0&quot;</span><span class="p">,</span> <span class="n">pre_release</span><span class="o">=</span><span class="kc">True</span><span class="p">,</span> <span class="n">tags</span><span class="o">=</span><span class="p">[</span><span class="s1">&#39;1.0.0&#39;</span><span class="p">,</span> <span class="s1">&#39;1.0.1&#39;</span><span class="p">,</span> <span class="s1">&#39;1.0.2-3&#39;</span><span class="p">])</span>
<span class="s1">&#39;1.0.2-4&#39;</span>
</pre></div>
</div></div>
The initial function looks like this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">nextversion</span><span class="p">(</span><span class="n">series</span><span class="p">,</span> <span class="n">pre_release</span><span class="p">,</span> <span class="n">tags</span><span class="p">):</span>
    <span class="n">version_pattern</span> <span class="o">=</span> <span class="s2">&quot;&quot;</span><span class="o">.</span><span class="n">join</span><span class="p">([</span>
        <span class="sa">r</span><span class="s2">&quot;^&quot;</span><span class="p">,</span>
        <span class="n">re</span><span class="o">.</span><span class="n">escape</span><span class="p">(</span><span class="n">series</span><span class="p">),</span>
        <span class="n">re</span><span class="o">.</span><span class="n">escape</span><span class="p">(</span><span class="s2">&quot;.&quot;</span><span class="p">),</span>
        <span class="sa">r</span><span class="s2">&quot;(?P&lt;version&gt;\d+)&quot;</span><span class="p">,</span>
        <span class="sa">r</span><span class="s2">&quot;(?P&lt;pre_release&gt;-(?P&lt;pre_release_number&gt;(\d+)))?&quot;</span><span class="p">,</span>
        <span class="sa">r</span><span class="s2">&quot;$&quot;</span><span class="p">,</span>
    <span class="p">])</span>
    <span class="n">versions</span> <span class="o">=</span> <span class="p">[]</span>
    <span class="n">pre_release_numbers</span> <span class="o">=</span> <span class="p">{}</span>
    <span class="k">for</span> <span class="n">tag</span> <span class="ow">in</span> <span class="n">tags</span><span class="p">:</span>
        <span class="n">match</span> <span class="o">=</span> <span class="n">re</span><span class="o">.</span><span class="n">match</span><span class="p">(</span><span class="n">version_pattern</span><span class="p">,</span> <span class="n">tag</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">match</span><span class="p">:</span>
            <span class="n">version</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">match</span><span class="p">[</span><span class="s2">&quot;version&quot;</span><span class="p">])</span>
            <span class="k">if</span> <span class="n">match</span><span class="p">[</span><span class="s2">&quot;pre_release&quot;</span><span class="p">]:</span>
                <span class="k">if</span> <span class="n">version</span> <span class="ow">not</span> <span class="ow">in</span> <span class="n">pre_release_numbers</span><span class="p">:</span>
                    <span class="n">pre_release_numbers</span><span class="p">[</span><span class="n">version</span><span class="p">]</span> <span class="o">=</span> <span class="p">[]</span>
                <span class="n">pre_release_numbers</span><span class="p">[</span><span class="n">version</span><span class="p">]</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="nb">int</span><span class="p">(</span><span class="n">match</span><span class="p">[</span><span class="s2">&quot;pre_release_number&quot;</span><span class="p">]))</span>
            <span class="k">else</span><span class="p">:</span>
                <span class="n">versions</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">version</span><span class="p">)</span>
    <span class="n">next_version</span> <span class="o">=</span> <span class="nb">max</span><span class="p">(</span>
        <span class="p">[</span><span class="mi">0</span><span class="p">]</span>
        <span class="o">+</span>
        <span class="p">[</span><span class="mi">1</span><span class="o">+</span><span class="n">version</span> <span class="k">for</span> <span class="n">version</span> <span class="ow">in</span> <span class="n">versions</span><span class="p">]</span>
        <span class="o">+</span>
        <span class="nb">list</span><span class="p">(</span><span class="n">pre_release_numbers</span><span class="o">.</span><span class="n">keys</span><span class="p">())</span>
    <span class="p">)</span>
    <span class="n">next_pre_release_number</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">+</span> <span class="nb">max</span><span class="p">(</span><span class="n">pre_release_numbers</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">next_version</span><span class="p">,</span> <span class="p">[</span><span class="mi">0</span><span class="p">]))</span>
    <span class="k">if</span> <span class="n">pre_release</span><span class="p">:</span>
        <span class="k">return</span> <span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">series</span><span class="si">}</span><span class="s2">.</span><span class="si">{</span><span class="n">next_version</span><span class="si">}</span><span class="s2">-</span><span class="si">{</span><span class="n">next_pre_release_number</span><span class="si">}</span><span class="s2">&quot;</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="k">return</span> <span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="n">series</span><span class="si">}</span><span class="s2">.</span><span class="si">{</span><span class="n">next_version</span><span class="si">}</span><span class="s2">&quot;</span>
</pre></div>
</div></div>
I refactor it to this:

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">nextversion</span><span class="p">(</span><span class="n">series</span><span class="p">,</span> <span class="n">pre_release</span><span class="p">,</span> <span class="n">tags</span><span class="p">):</span>
    <span class="k">return</span> <span class="n">Tags</span><span class="p">(</span><span class="n">tags</span><span class="p">)</span><span class="o">.</span><span class="n">get_next_version</span><span class="p">(</span><span class="n">series</span><span class="p">,</span> <span class="n">pre_release</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Tags</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">tags</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">tags</span> <span class="o">=</span> <span class="n">tags</span>

    <span class="k">def</span> <span class="nf">get_next_version</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">series</span><span class="p">,</span> <span class="n">pre_release</span><span class="p">):</span>
        <span class="n">series</span> <span class="o">=</span> <span class="n">Series</span><span class="p">(</span><span class="n">series</span><span class="p">)</span>
        <span class="n">versions</span> <span class="o">=</span> <span class="n">Versions</span><span class="p">()</span>
        <span class="k">for</span> <span class="n">tag</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">tags</span><span class="p">:</span>
            <span class="n">series</span><span class="o">.</span><span class="n">parse_version</span><span class="p">(</span><span class="n">tag</span><span class="p">)</span><span class="o">.</span><span class="n">add_to</span><span class="p">(</span><span class="n">versions</span><span class="p">)</span>
        <span class="k">return</span> <span class="n">versions</span><span class="o">.</span><span class="n">get_next_version</span><span class="p">(</span><span class="n">pre_release</span><span class="p">)</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">series</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Release</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">version</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">version</span> <span class="o">=</span> <span class="n">version</span>

    <span class="k">def</span> <span class="nf">add_to</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">versions</span><span class="p">):</span>
        <span class="n">versions</span><span class="o">.</span><span class="n">add_release</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">version</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">format</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">series</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">series</span><span class="o">.</span><span class="n">format_release</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">version</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">PreRelease</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">version</span><span class="p">,</span> <span class="n">pre_release_number</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">version</span> <span class="o">=</span> <span class="n">version</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_number</span> <span class="o">=</span> <span class="n">pre_release_number</span>

    <span class="k">def</span> <span class="nf">add_to</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">versions</span><span class="p">):</span>
        <span class="n">versions</span><span class="o">.</span><span class="n">add_pre_release</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">version</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_number</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">format</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">series</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">series</span><span class="o">.</span><span class="n">format_pre_release</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">version</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_number</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">NoMatchVersion</span><span class="p">:</span>

    <span class="k">def</span> <span class="nf">add_to</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">versions</span><span class="p">):</span>
        <span class="k">pass</span>

<span class="k">class</span> <span class="nc">Versions</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">versions</span> <span class="o">=</span> <span class="p">[]</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_numbers</span> <span class="o">=</span> <span class="p">{}</span>

    <span class="k">def</span> <span class="nf">add_release</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">version</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">versions</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">version</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">add_pre_release</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">version</span><span class="p">,</span> <span class="n">pre_release_number</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">version</span> <span class="ow">not</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_numbers</span><span class="p">:</span>
            <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_numbers</span><span class="p">[</span><span class="n">version</span><span class="p">]</span> <span class="o">=</span> <span class="p">[]</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pre_release_numbers</span><span class="p">[</span><span class="n">version</span><span class="p">]</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">pre_release_number</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">get_next_version</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">pre_release</span><span class="p">):</span>
        <span class="n">next_version</span> <span class="o">=</span> <span class="nb">max</span><span class="p">(</span>
            <span class="p">[</span><span class="mi">0</span><span class="p">]</span>
            <span class="o">+</span>
            <span class="p">[</span><span class="mi">1</span><span class="o">+</span><span class="n">version</span> <span class="k">for</span> <span class="n">version</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">versions</span><span class="p">]</span>
            <span class="o">+</span>
            <span class="nb">list</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">pre_release_numbers</span><span class="o">.</span><span class="n">keys</span><span class="p">())</span>
        <span class="p">)</span>
        <span class="n">next_pre_release_number</span> <span class="o">=</span> <span class="mi">1</span> <span class="o">+</span> <span class="nb">max</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">pre_release_numbers</span><span class="o">.</span><span class="n">get</span><span class="p">(</span><span class="n">next_version</span><span class="p">,</span> <span class="p">[</span><span class="mi">0</span><span class="p">]))</span>
        <span class="k">if</span> <span class="n">pre_release</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">PreRelease</span><span class="p">(</span><span class="n">next_version</span><span class="p">,</span> <span class="n">next_pre_release_number</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">Release</span><span class="p">(</span><span class="n">next_version</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Series</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">series</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">series</span> <span class="o">=</span> <span class="n">series</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">version_pattern</span> <span class="o">=</span> <span class="s2">&quot;&quot;</span><span class="o">.</span><span class="n">join</span><span class="p">([</span>
            <span class="sa">r</span><span class="s2">&quot;^&quot;</span><span class="p">,</span>
            <span class="n">re</span><span class="o">.</span><span class="n">escape</span><span class="p">(</span><span class="n">series</span><span class="p">),</span>
            <span class="n">re</span><span class="o">.</span><span class="n">escape</span><span class="p">(</span><span class="s2">&quot;.&quot;</span><span class="p">),</span>
            <span class="sa">r</span><span class="s2">&quot;(?P&lt;version&gt;\d+)&quot;</span><span class="p">,</span>
            <span class="sa">r</span><span class="s2">&quot;(?P&lt;pre_release&gt;-(?P&lt;pre_release_number&gt;(\d+)))?&quot;</span><span class="p">,</span>
            <span class="sa">r</span><span class="s2">&quot;$&quot;</span><span class="p">,</span>
        <span class="p">])</span>

    <span class="k">def</span> <span class="nf">parse_version</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">tag</span><span class="p">):</span>
        <span class="n">match</span> <span class="o">=</span> <span class="n">re</span><span class="o">.</span><span class="n">match</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">version_pattern</span><span class="p">,</span> <span class="n">tag</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">match</span><span class="p">:</span>
            <span class="n">version</span> <span class="o">=</span> <span class="nb">int</span><span class="p">(</span><span class="n">match</span><span class="p">[</span><span class="s2">&quot;version&quot;</span><span class="p">])</span>
            <span class="k">if</span> <span class="n">match</span><span class="p">[</span><span class="s2">&quot;pre_release&quot;</span><span class="p">]:</span>
                <span class="k">return</span> <span class="n">PreRelease</span><span class="p">(</span><span class="n">version</span><span class="p">,</span> <span class="nb">int</span><span class="p">(</span><span class="n">match</span><span class="p">[</span><span class="s2">&quot;pre_release_number&quot;</span><span class="p">]))</span>
            <span class="k">else</span><span class="p">:</span>
                <span class="k">return</span> <span class="n">Release</span><span class="p">(</span><span class="n">version</span><span class="p">)</span>
        <span class="k">return</span> <span class="n">NoMatchVersion</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">format_release</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">version</span><span class="p">):</span>
        <span class="k">return</span> <span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="bp">self</span><span class="o">.</span><span class="n">series</span><span class="si">}</span><span class="s2">.</span><span class="si">{</span><span class="n">version</span><span class="si">}</span><span class="s2">&quot;</span>

    <span class="k">def</span> <span class="nf">format_pre_release</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">version</span><span class="p">,</span> <span class="n">pre_release_number</span><span class="p">):</span>
        <span class="k">return</span> <span class="sa">f</span><span class="s2">&quot;</span><span class="si">{</span><span class="bp">self</span><span class="o">.</span><span class="n">series</span><span class="si">}</span><span class="s2">.</span><span class="si">{</span><span class="n">version</span><span class="si">}</span><span class="s2">-</span><span class="si">{</span><span class="n">pre_release_number</span><span class="si">}</span><span class="s2">&quot;</span>
</pre></div>
</div></div>