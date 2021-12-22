---
title: 'DRAFT: RLMeta Poster 2'
date: 2021-12-22
tags: rlmeta,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

A while ago I created a [poster](/writing/creating-rlmeta-poster/index.html) to
showcase RLMeta. The version of RLMeta on the poster is based on the version
from the [memoizing failures](/writing/rlmeta-memoize-failures/index.html)
article, but I made it smaller and more beautiful to better fit the poster. To
be able to finish the poster, I had to stop making changes and put the source
code on the poster. That was difficult because I felt the need for it to be
perfect. Eventually I did stop polishing, and left a few items unresolved.

Almost immediately after I finished the poster, I started working on a second
version. Initially, my plan was to make a second version of the poster. I
started to fix the unresolved items and I was making progress. But somehow
imperfections kept creeping in. It felt like a never ending game of chasing
perfection. That's when I decided that a second poster was probably not going
to be worth it. But I still liked the new version of RLMeta.

Instead, I decided to attempt to present the new version in the style of a code
walk through. In other words, another way to showcase RLMeta that is also a bit
more practical. Compared to the poster version, this version could also be more
easily improved because rendering the blog post is automatic whereas creating
the layout of a poster requires manual work every time the source code changes.
I also wanted to experiment with the walk through format because I thought it
could be something worth putting into the README of a project.

The rest of this blog post consists of the walk through of the new version of
RLMeta and a section on the most important changes from the poster version and
motivations for them.

## Code walk through

### Getting RLMeta

In order to follow along on this walk through, you need to download the
version of RLMeta from here: [rlmeta-poster-2.zip](rlmeta-poster-2.zip).

### File structure

The zip file consists of the source code for the RLMeta compiler, a make
script, and the compiler itself (`rlmeta.py`):

<div class="highlight"><pre><span></span>$ <span></span>tree --dirsfirst
<span></span>.
├── src
│   ├── assembler.rlmeta
│   ├── codegenerator.rlmeta
│   ├── main.py
│   ├── parser.rlmeta
│   └── support.py
├── make.py
└── rlmeta.py

1 directory, 7 files
</pre></div>

The size of the source code is quite small:

<div class="highlight"><pre><span></span>$ <span></span>wc -l src/*
<span></span>   39 src/assembler.rlmeta
   57 src/codegenerator.rlmeta
   26 src/main.py
   60 src/parser.rlmeta
  237 src/support.py
  419 total
</pre></div>

The compiler can be created from this source code only. We will see how later
in this walk through.

### Exploring RLMeta

Before we dive into how the RLMeta compiler is created, let's explore RLMeta by
writing a small, but real, program in it.

What types of programs can we write in RLMeta?

In RLMeta, we write grammars. Grammars have rules that specify how to match
objects from an input stream and specify what should happen when objects are
matched.

Let's write a grammar that counts the number of objects in an input stream and
produces a report:


<div class="highlight"><pre><span></span>$ <span></span>cat object_counter.rlmeta
<span></span>ObjectCounter {
    count <span class="nb">=</span> <span class="nc">.*</span><span class="nb">:</span>xs <span class="nb">-&gt;</span> { <span class="s">&quot;number of objects = &quot;</span> len(xs) }
}
</pre></div>

The main function of the RLMeta compiler is to transform grammars into Python
code. If invoked without arguments, the compiler reads a grammar from stdin
and writes Python code to stdout:

<div class="highlight"><pre><span></span>$ <span></span>cat object_counter.rlmeta <span class="p">|</span> python rlmeta.py
<span></span><span class="k">class</span> <span class="nc">ObjectCounter</span><span class="p">(</span><span class="n">Grammar</span><span class="p">):</span>
    <span class="n">rules</span> <span class="o">=</span> <span class="p">{</span>
        <span class="s1">&#39;count&#39;</span><span class="p">:</span> <span class="mi">0</span>
    <span class="p">}</span>
    <span class="n">code</span> <span class="o">=</span> <span class="p">[</span>
        <span class="n">PUSH_SCOPE</span><span class="p">,</span>
        <span class="n">LIST_START</span><span class="p">,</span>
        <span class="n">BACKTRACK</span><span class="p">,</span>
        <span class="mi">10</span><span class="p">,</span>
        <span class="n">MATCH</span><span class="p">,</span>
        <span class="s1">&#39;any&#39;</span><span class="p">,</span>
        <span class="k">lambda</span> <span class="n">x</span><span class="p">:</span> <span class="kc">True</span><span class="p">,</span>
        <span class="n">LIST_APPEND</span><span class="p">,</span>
        <span class="n">COMMIT</span><span class="p">,</span>
        <span class="mi">2</span><span class="p">,</span>
        <span class="n">LIST_END</span><span class="p">,</span>
        <span class="n">BIND</span><span class="p">,</span>
        <span class="s1">&#39;xs&#39;</span><span class="p">,</span>
        <span class="n">ACTION</span><span class="p">,</span>
        <span class="k">lambda</span> <span class="bp">self</span><span class="p">:</span> <span class="n">join</span><span class="p">([</span><span class="s1">&#39;number of objects = &#39;</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">lookup</span><span class="p">(</span><span class="s1">&#39;len&#39;</span><span class="p">)(</span><span class="bp">self</span><span class="o">.</span><span class="n">lookup</span><span class="p">(</span><span class="s1">&#39;xs&#39;</span><span class="p">))]),</span>
        <span class="n">POP_SCOPE</span><span class="p">,</span>
        <span class="n">RETURN</span>
    <span class="p">]</span>
</pre></div>

This is equivalent to using the `--compile` command with a value  of `-` which
stands for stdin:

<div class="highlight"><pre><span></span>$ <span></span>cat object_counter.rlmeta <span class="p">|</span> python rlmeta.py --compile - <span class="p">|</span> head -n3
<span></span><span class="k">class</span> <span class="nc">ObjectCounter</span><span class="p">(</span><span class="n">Grammar</span><span class="p">):</span>
    <span class="n">rules</span> <span class="o">=</span> <span class="p">{</span>
        <span class="s1">&#39;count&#39;</span><span class="p">:</span> <span class="mi">0</span>
</pre></div>

And, the file can also be specified directly like this:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --compile object_counter.rlmeta <span class="p">|</span> head -n3
<span></span><span class="k">class</span> <span class="nc">ObjectCounter</span><span class="p">(</span><span class="n">Grammar</span><span class="p">):</span>
    <span class="n">rules</span> <span class="o">=</span> <span class="p">{</span>
        <span class="s1">&#39;count&#39;</span><span class="p">:</span> <span class="mi">0</span>
</pre></div>

Don't worry about understanding the generated code. We will explore it more
later. Just note that the generated class inherits from a class called
`Grammar` and that it uses some constants like `PUSH_SCOPE` and `LIST_START`.
These things are defined in a support library which can be generated by the
RLMeta compiler with the `--support` command:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --support <span class="p">|</span> grep <span class="s1">&#39;^\(class\|def\)&#39;</span>
<span></span><span class="k">class</span> <span class="nc">VM</span><span class="p">:</span>
<span class="k">def</span> <span class="nf">PUSH_SCOPE</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">POP_SCOPE</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">BACKTRACK</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">COMMIT</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">CALL</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">CALL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">pc</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">RETURN</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">MATCH</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">MATCH_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">fn</span><span class="p">,</span> <span class="n">message</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">MATCH_CALL_RULE</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">LIST_START</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">LIST_APPEND</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">LIST_END</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">BIND</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">ACTION</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">PUSH_STREAM</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">POP_STREAM</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">FAIL</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">fail_message</span><span class="p">):</span>
<span class="k">class</span> <span class="nc">SemanticAction</span><span class="p">(</span><span class="nb">object</span><span class="p">):</span>
<span class="k">class</span> <span class="nc">MatchError</span><span class="p">(</span><span class="ne">Exception</span><span class="p">):</span>
<span class="k">class</span> <span class="nc">Grammar</span><span class="p">(</span><span class="nb">object</span><span class="p">):</span>
<span class="k">class</span> <span class="nc">Runtime</span><span class="p">(</span><span class="nb">dict</span><span class="p">):</span>
<span class="k">class</span> <span class="nc">Counter</span><span class="p">(</span><span class="nb">object</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">splice</span><span class="p">(</span><span class="n">depth</span><span class="p">,</span> <span class="n">item</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">concat</span><span class="p">(</span><span class="n">lists</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">join</span><span class="p">(</span><span class="n">items</span><span class="p">,</span> <span class="n">delimiter</span><span class="o">=</span><span class="s2">&quot;&quot;</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">indent</span><span class="p">(</span><span class="n">text</span><span class="p">,</span> <span class="n">prefix</span><span class="o">=</span><span class="s2">&quot;    &quot;</span><span class="p">):</span>
<span class="k">def</span> <span class="nf">compile_chain</span><span class="p">(</span><span class="n">grammars</span><span class="p">,</span> <span class="n">source</span><span class="p">):</span>
</pre></div>

To create a complete program, we also have to write a main function that
instantiates the `ObjectCounter` grammar and invokes its `count` rule.

Here is an example that passes stdin as the input stream to the `count` rule
and prints the result to stdout:


<div class="highlight"><pre><span></span>$ <span></span>cat object_counter_main.py
<span></span><span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="kn">import</span> <span class="nn">sys</span>
    <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">ObjectCounter</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="s2">&quot;count&quot;</span><span class="p">,</span> <span class="n">sys</span><span class="o">.</span><span class="n">stdin</span><span class="o">.</span><span class="n">read</span><span class="p">()))</span>
</pre></div>

The `--copy` command of the RLMeta compiler can be used to copy this main file
as is to compiled file.

Combining these pieces into a single compile command, we get this:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --support --compile object_counter.rlmeta --copy object_counter_main.py &gt; object_counter.py
<span></span>
</pre></div>

It will perform all commands in the given order and write all generated code
concatenated into a single file.

Note that the support library comes before the grammar so that `Grammar` is
defined by the time `ObjectCounter` is evaluated.

The object counter source code has now been compiled into a standalone Python
program that can be run like this:

<div class="highlight"><pre><span></span>$ <span></span><span class="nb">echo</span> <span class="s1">&#39;hello&#39;</span> <span class="p">|</span> python object_counter.py
<span></span>number of objects = 6
</pre></div>

<div class="highlight"><pre><span></span>$ <span></span><span class="nb">echo</span> <span class="s1">&#39;this is longer&#39;</span> <span class="p">|</span> python object_counter.py
<span></span>number of objects = 15
</pre></div>

So programs in RLMeta are written mainly in grammar files with some support
functions written in Python. The RLMeta compiler can process all these files to
produce a single Python file which is the compiled program.


### Compiling RLMeta itself

Now that we have an understanding of RLMeta, let's look at the command that
compiles the RLMeta compiler itself from the source code:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --embed SUPPORT src/support.py --support --compile src/parser.rlmeta --compile src/codegenerator.rlmeta --compile src/assembler.rlmeta --copy src/main.py &gt; rlmeta-raw.py
<span></span>
</pre></div>

The first command, `--embed SUPPORT src/support.py`, tells the compiler to
generate a Python variable named `SUPPORT` containing the contents of the file
`src/support.py`. The `--embed` command is the last command of the compiler that
we have not yet seen. (The RLMeta compiler needs the support library in a
variable so that it can generate it later with the `--support` command.)

Next, the `--support` command tells the compiler to generate the support
library that is embedded in it.

The `--compile ...` commands tell the compiler to compile the given grammar
files.

The last command, `--copy src/main.py`, tells the compiler to copy the main file
verbatim. Similar to what we did to the main file in the object counter.

The make script can be called with the `--compile` command to perform this
exact function:

<div class="highlight"><pre><span></span>$ <span></span>./make.py --compile &gt; rlmeta-compile.py
<span></span>[0;33mCompiling rlmeta using rlmeta.py[0m
[0;32m  O-----------------O[0m
[0;32m  | RLMeta compiled |[0m
[0;32m~~|     itself!     |[0m
[0;32m  O-----------------O[0m
</pre></div>

And all these files are exactly the same:

<div class="highlight"><pre><span></span>$ <span></span>md5sum rlmeta.py rlmeta-compile.py rlmeta-raw.py
<span></span>8f438ec43dc93d0297415c7ddbcc683c  rlmeta.py
8f438ec43dc93d0297415c7ddbcc683c  rlmeta-compile.py
8f438ec43dc93d0297415c7ddbcc683c  rlmeta-raw.py
</pre></div>

Thus, the RLMeta compiler reproduced itself exactly from the source code.


### A tour of the main function

Let's now look at how all commands of the RLMeta compiler are implemented. Here
is the main function:

<div class="highlight"><pre><span></span><span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="kn">import</span> <span class="nn">sys</span>
    <span class="k">def</span> <span class="nf">read</span><span class="p">(</span><span class="n">path</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">path</span> <span class="o">==</span> <span class="s2">&quot;-&quot;</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">sys</span><span class="o">.</span><span class="n">stdin</span><span class="o">.</span><span class="n">read</span><span class="p">()</span>
        <span class="k">with</span> <span class="nb">open</span><span class="p">(</span><span class="n">path</span><span class="p">)</span> <span class="k">as</span> <span class="n">f</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">f</span><span class="o">.</span><span class="n">read</span><span class="p">()</span>
    <span class="n">args</span> <span class="o">=</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:]</span> <span class="ow">or</span> <span class="p">[</span><span class="s2">&quot;--compile&quot;</span><span class="p">,</span> <span class="s2">&quot;-&quot;</span><span class="p">]</span>
    <span class="k">while</span> <span class="n">args</span><span class="p">:</span>
        <span class="n">command</span> <span class="o">=</span> <span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--support&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">SUPPORT</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--copy&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">read</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)))</span>
        <span class="k">elif</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--embed&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="s2">&quot;</span><span class="si">{}</span><span class="s2"> = </span><span class="si">{}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span>
                <span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">),</span>
                <span class="nb">repr</span><span class="p">(</span><span class="n">read</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)))</span>
            <span class="p">))</span>
        <span class="k">elif</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--compile&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">compile_chain</span><span class="p">(</span>
                <span class="p">[(</span><span class="n">Parser</span><span class="p">,</span> <span class="s2">&quot;file&quot;</span><span class="p">),</span> <span class="p">(</span><span class="n">CodeGenerator</span><span class="p">,</span> <span class="s2">&quot;asts&quot;</span><span class="p">),</span> <span class="p">(</span><span class="n">Assembler</span><span class="p">,</span> <span class="s2">&quot;asts&quot;</span><span class="p">)],</span>
                <span class="n">read</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">))</span>
            <span class="p">))</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">exit</span><span class="p">(</span><span class="s2">&quot;ERROR: Unknown command &#39;</span><span class="si">{}</span><span class="s2">&#39;&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">command</span><span class="p">))</span>
</pre></div>

It contains command line parsing and handles the basic cases.

The `--compile` is the most complex of them all. It calls the `compile_chain`
method which runs the given grammars/rules in order (in this case the input
will first be parsed, then passed to the code generator, and finally passed to
the assembler) and prints a pretty error message to stderr upon failure:

<div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">compile_chain</span><span class="p">(</span><span class="n">grammars</span><span class="p">,</span> <span class="n">source</span><span class="p">):</span>
    <span class="kn">import</span> <span class="nn">sys</span>
    <span class="kn">import</span> <span class="nn">pprint</span>
    <span class="k">for</span> <span class="n">grammar</span><span class="p">,</span> <span class="n">rule</span> <span class="ow">in</span> <span class="n">grammars</span><span class="p">:</span>
        <span class="k">try</span><span class="p">:</span>
            <span class="n">source</span> <span class="o">=</span> <span class="n">grammar</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">rule</span><span class="p">,</span> <span class="n">source</span><span class="p">)</span>
        <span class="k">except</span> <span class="n">MatchError</span> <span class="k">as</span> <span class="n">e</span><span class="p">:</span>
            <span class="n">MARKER</span> <span class="o">=</span> <span class="s2">&quot;</span><span class="se">\033</span><span class="s2">[0;31m&lt;ERROR POSITION&gt;</span><span class="se">\033</span><span class="s2">[0m&quot;</span>
            <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="nb">str</span><span class="p">):</span>
                <span class="n">stream_string</span> <span class="o">=</span> <span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">[:</span><span class="n">e</span><span class="o">.</span><span class="n">pos</span><span class="p">]</span> <span class="o">+</span> <span class="n">MARKER</span> <span class="o">+</span> <span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">[</span><span class="n">e</span><span class="o">.</span><span class="n">pos</span><span class="p">:]</span>
            <span class="k">else</span><span class="p">:</span>
                <span class="n">stream_string</span> <span class="o">=</span> <span class="n">pprint</span><span class="o">.</span><span class="n">pformat</span><span class="p">(</span><span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">)</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">exit</span><span class="p">(</span><span class="s2">&quot;ERROR: </span><span class="si">{}</span><span class="se">\n</span><span class="s2">POSITION: </span><span class="si">{}</span><span class="se">\n</span><span class="s2">STREAM:</span><span class="se">\n</span><span class="si">{}</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span>
                <span class="n">e</span><span class="o">.</span><span class="n">message</span><span class="p">,</span>
                <span class="n">e</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span>
                <span class="n">indent</span><span class="p">(</span><span class="n">stream_string</span><span class="p">)</span>
            <span class="p">))</span>
    <span class="k">return</span> <span class="n">source</span>
</pre></div>

This function might be useful for other grammars as well. That is why it's
included in the support library and not only in the main file.

### Following a compilation

Let's now follow a compilation of an example grammar to learn more about how
a grammar file is turned into Python code. Here it is:


<div class="highlight"><pre><span></span>$ <span></span>cat example.rlmeta
<span></span>Example {
    main <span class="nb">=</span> <span class="nc">.</span>
}
</pre></div>

And this is what it compiles to:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --compile example.rlmeta
<span></span><span class="k">class</span> <span class="nc">Example</span><span class="p">(</span><span class="n">Grammar</span><span class="p">):</span>
    <span class="n">rules</span> <span class="o">=</span> <span class="p">{</span>
        <span class="s1">&#39;main&#39;</span><span class="p">:</span> <span class="mi">0</span>
    <span class="p">}</span>
    <span class="n">code</span> <span class="o">=</span> <span class="p">[</span>
        <span class="n">PUSH_SCOPE</span><span class="p">,</span>
        <span class="n">MATCH</span><span class="p">,</span>
        <span class="s1">&#39;any&#39;</span><span class="p">,</span>
        <span class="k">lambda</span> <span class="n">x</span><span class="p">:</span> <span class="kc">True</span><span class="p">,</span>
        <span class="n">POP_SCOPE</span><span class="p">,</span>
        <span class="n">RETURN</span>
    <span class="p">]</span>
</pre></div>

The transformations that the grammar goes through were defined in the main
function:

<div class="highlight"><pre><span></span><span class="p">[(</span><span class="n">Parser</span><span class="p">,</span> <span class="s2">&quot;file&quot;</span><span class="p">),</span> <span class="p">(</span><span class="n">CodeGenerator</span><span class="p">,</span> <span class="s2">&quot;asts&quot;</span><span class="p">),</span> <span class="p">(</span><span class="n">Assembler</span><span class="p">,</span> <span class="s2">&quot;asts&quot;</span><span class="p">)],</span>
</pre></div>

So first the grammar file is passed to the `file` rule of the parser:

<div class="highlight"><pre><span></span>file <span class="nb">=</span>
  <span class="nb">|</span> (space grammar)<span class="nc">*</span><span class="nb">:</span>xs space <span class="nc">!.</span>            <span class="nb">-&gt;</span> xs
</pre></div>

It it turn calls the `grammar` rule to parse all grammars in the file:

<div class="highlight"><pre><span></span>grammar <span class="nb">=</span>
  <span class="nb">|</span> name<span class="nb">:</span>x space <span class="sc">&#39;{&#39;</span> rule<span class="nc">*</span><span class="nb">:</span>ys space <span class="sc">&#39;}&#39;</span>     <span class="nb">-&gt;</span> [<span class="s">&quot;Grammar&quot;</span> x <span class="nc">~</span>ys]
</pre></div>

This rule matches the name, the open curly brace, a set of rules, and the
closing curly brace. It will then return an AST that looks like this:

```python
[
    "Grammar",
    "Example",
    ...
]
```

This AST is handed off to the `asts` rule in the code generator:

<div class="highlight"><pre><span></span>asts          <span class="nb">=</span> ast<span class="nc">*</span><span class="nb">:</span>xs <span class="nc">!.</span>  <span class="nb">-&gt;</span> xs
</pre></div>

It it turn calls the `ast` rule to process all AST nodes:

<div class="highlight"><pre><span></span>ast           <span class="nb">=</span> [<span class="nc">%</span><span class="nb">:</span>x]       <span class="nb">-&gt;</span> x
</pre></div>

The `ast` rule treats the first argument in the AST as a rule name, and calls
that rule. In this case `Grammar`:

<div class="highlight"><pre><span></span>Grammar       <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nc">*</span><span class="nb">:</span>ys <span class="nb">-&gt;</span> [<span class="s">&quot;Grammar&quot;</span> x <span class="nc">~~</span>ys]
</pre></div>

The code generator creates a new AST node representing a grammar. But this AST
node is slightly different and meant to be processed by the assembler. The
result is this:

```python
[
    "Grammar",
    "Example",
    ... ast nodes for consumption by assembler ...
]
```

This AST is passed to the `asts` rule in the assembler:

<div class="highlight"><pre><span></span>asts     <span class="nb">=</span> ast<span class="nc">*</span><span class="nb">:</span>xs <span class="nc">!.</span>      <span class="nb">-&gt;</span> { xs }
</pre></div>

It in turn calls the `ast` rule:

<div class="highlight"><pre><span></span>ast      <span class="nb">=</span> [<span class="nc">%</span><span class="nb">:</span>x]           <span class="nb">-&gt;</span> x
</pre></div>

Which does the same trick again, now invoking the `Grammar` rule (in the
assembler) which looks like this:

<div class="highlight"><pre><span></span>Grammar  <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nc">*</span><span class="nb">:</span>ys     <span class="nb">-&gt;</span> list()<span class="nb">:</span>rules
                           <span class="nb">-&gt;</span> list()<span class="nb">:</span>code
                           <span class="nb">-&gt;</span> dict()<span class="nb">:</span>labels
                           <span class="nb">-&gt;</span> list()<span class="nb">:</span>patches
                           <span class="nb">-&gt;</span> ys
                           <span class="nb">-&gt;</span> run(<span class="s">&quot;asts&quot;</span> patches)
                           <span class="nb">-&gt;</span> { <span class="s">&quot;class &quot;</span> x <span class="s">&quot;(Grammar):</span><span class="se">\n</span><span class="s">&quot;</span> &gt;
                                  <span class="s">&quot;rules = {</span><span class="se">\n</span><span class="s">&quot;</span> &gt; join(rules <span class="s">&quot;,</span><span class="se">\n</span><span class="s">&quot;</span>) &lt; <span class="s">&quot;</span><span class="se">\n</span><span class="s">}</span><span class="se">\n</span><span class="s">&quot;</span>
                                  <span class="s">&quot;code = [</span><span class="se">\n</span><span class="s">&quot;</span> &gt; join(code  <span class="s">&quot;,</span><span class="se">\n</span><span class="s">&quot;</span>) &lt; <span class="s">&quot;</span><span class="se">\n</span><span class="s">]</span><span class="se">\n</span><span class="s">&quot;</span>
                                &lt; }
</pre></div>

This rule can be read as follows:

* Define a variable called `rules` which is a list
* Define a variable called `code` which is a list
* Define a variable called `labels` which is a dictionary
* Define a variable called `patches` which is a list
* Evaluate the AST nodes (with possible side effects recorded in the above
  variables)
* Tread the contents of the `code` variable as a list of AST nodes and process
  them with the `asts` rule of this grammar
* Return a string which is generated Python code

The generated code from our example looks like this:

    class Example(Grammar):
        rules = {
            ...
        }
        code = [
            ...
        ]

To understand how the `rule` and `code` sections are generated, we just have to
follow a few more transformations.

Let's look at one more and see how the rule in our example grammar is
transformed.

First, the rule is parsed by the `rule` rule in the parser:

<div class="highlight"><pre><span></span>rule <span class="nb">=</span>
  <span class="nb">|</span> name<span class="nb">:</span>x space <span class="sc">&#39;=&#39;</span> choice<span class="nb">:</span>y               <span class="nb">-&gt;</span> [<span class="s">&quot;Rule&quot;</span> x y]
</pre></div>

First the name is matched, then the equals sign, and then an expression
representing the body of the rule.

It our case, this rule produces this AST node:

```python
[
    "Rule",
    "main",
    ...
]
```

That note is going to be processed by the `Rule` rule in the code generator:

<div class="highlight"><pre><span></span>Rule          <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nb">:</span>y   <span class="nb">-&gt;</span> [[<span class="s">&quot;Rule&quot;</span> x]
                                <span class="nc">~</span>y
                                [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;RETURN&quot;</span>]]
</pre></div>

Generating an AST node that looks like this:

```python
[
    ["Rule", "main"],
    ...,
    ["OpCode", "RETURN"]
]
```

Here we can see that the AST from the code generator looks a bit more like
assembly code than a representation of the syntax in the grammar.

The first child in this AST node is going to be handled the `Rule` rule in the
assembler:

<div class="highlight"><pre><span></span>Rule     <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(rules { repr(x) <span class="s">&quot;: &quot;</span> len(code) })
                           <span class="nb">-&gt;</span> set(labels x len(code))
</pre></div>

It does two things:

1. Adds a string value to the `rules` list
2. Adds an entry to the `labels` dictionary to map a label to an index in the
   `code` list

At this point, the variables have the following values.

```python
rules = [
    "'main': 0",
]
labels = {
    'main': 0,
}
```

The second child in the AST node is going to be handled the `OpCode` rule in
the assembler:

<div class="highlight"><pre><span></span>OpCode   <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(code x)
</pre></div>

It adds the given op code to the `code` list, giving it this value:

```python
code = [
    ...,
    "RETURN",
]
```

Hopefully you should now be comfortable to follow transformations yourself to
understand how a compilation is done.


### The usage of the make script

When the make script is called without arguments, it performs a meta
compilation and runs a few tests:

<div class="highlight"><pre><span></span>$ <span></span>./make.py
<span></span>[0;33mCompiling rlmeta using rlmeta.py[0m
[0;33mWriting rlmeta1.py[0m
[0;33mTest: Has its own support library[0m
[0;33mTest: Disallow semantic action in the middle[0m
ERROR: expected }
POSITION: 22
STREAM:
    Grammar { x = . -&gt; [] [0;31m&lt;ERROR POSITION&gt;[0m. }
[0;33mMoving rlmeta1.py -&gt; rlmeta.py[0m
[0;32m  O-----------------O[0m
[0;32m  | RLMeta compiled |[0m
[0;32m~~|     itself!     |[0m
[0;32m  O-----------------O[0m
</pre></div>

The meaning of a meta compilation is to create a new version of RLMeta that can
still reproduce it self from the source code.

In the output above, we can see that it compiled RLMeta and wrote the result to
`rlmeta1.py`. In this case, since it is exactly the same as `rlmeta.py`, the
compilation stopped there and a few more tests were run using this compiler.
But if we make changes to the source code, `rlmeta1.py` will most likely not be
exactly the same as `rlmeta.py`, and a few more compilations might be needed.
I've written about the details of meta compilation in a [previous blog
post](/writing/modifying-rlmeta/index.html#5f6a1c91143146dbb3b865ac42562135).

## Changes from the previous version

This section explains the most important changes in this version of RLMeta
compared to the original poster version.

First of all, I wanted to work on the unresolved items which were the
following:

* Assembly code in code generator is hard to read.
* The label counter is incremented at match time, not at semantic action
  evaluation time.
* Compilation depends on bash.

In the poster article, I also had a few notes about
[future versions](/writing/creating-rlmeta-poster/index.html#b070abcd2f134cf894e33e63188a9fee):

> The smaller it is, the easier it is to understand and therefore extend. The
> more flexible it is to extend the better. If I make another poster version it
> would therefore focus on being smaller and more flexible. Since all
> successive version of RLMeta have been faster than the ones before,
> performance is also important. But small size, clarity, and flexibility come
> first.

I used these guidelines to decide if certain changes should go into the new
version or not.

One interesting thing to note is that the guidelines are sometimes
contradicting. Writing clear code might mean more lines of code which makes the
code base larger.  Perhaps that's also why I got stuck chasing perfection. I
thought I made something easier to read, but it ended up costing 10 extra lines
of code.  Should I include it?

### Generate labels in semantic actions

One thing that I left in the first version of the poster that still annoyed me
was that labels are generated at match time, not at semantic action evaluation
time. It will not produce incorrect results. At worst, some labels end up not
being used because the counter value captured was in a rule that later failed.
But dealing with labels at match time does not make sense. It should really
happen at semantic action evaluation time.

Here is what the `Not` rule looks like in the first version of the poster:

```rlmeta
Not = ast:x #:a #:b -> { "I('BACKTRACK', " b ")\n"
                         x
                         "I('COMMIT', " a ")\n"
                         "LABEL(" a ")\n"
                         "I('FAIL', 'no match expected')\n"
                         "LABEL(" b ")\n"                   }
```

Here is what the `Not` rule looks like after the change:

```rlmeta
Not = ast:x -> label():a -> label():b
            -> { "I('BACKTRACK', " b ")\n"
                 x
                 "I('COMMIT', " a ")\n"
                 "LABEL(" a ")\n"
                 "I('FAIL', 'no match expected')\n"
                 "LABEL(" b ")\n"                   }
```

This change puts label generation where it belongs, in semantic actions, and
thus makes the implementation **more clear**. The VM is no longer concerned
with labels. It is only concerned with matching. It does make semantic actions
a bit more complicated, but the bind syntax is familiar from match expressions
and an action being a sequence of things should be familiar as well.

This change required a bit of rework how semantic actions work. Previously only
one expression was allowed. Now multiple expressions are allowed. The result of
expressions can also be bound to names which subsequent expressions can refer
to. Furthermore, there are now also runtime variables that are set with
bindings. `label` is a built in runtime function that generates increasing
integers starting at 0.

The implementation of this change also **increases the flexibility** of RLMeta.
For example, it is now possible to write a semantic action that generates code
in different sections like this:

<div class="highlight"><pre><span></span>ExampleBuffers {
    program  <span class="nb">=</span> ast<span class="nb">:</span>x  <span class="nb">-&gt;</span> Buffer()<span class="nb">:</span>header
                      <span class="nb">-&gt;</span> { <span class="s">&quot;# HEADER</span><span class="se">\n</span><span class="s">&quot;</span>
                           header
                           <span class="s">&quot;# BODY</span><span class="se">\n</span><span class="s">&quot;</span>
                           x            }
    ast      <span class="nb">=</span> [<span class="nc">%</span><span class="nb">:</span>x]  <span class="nb">-&gt;</span> x
    Program  <span class="nb">=</span> ast<span class="nc">*</span>
    Function <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>name <span class="nb">-&gt;</span> header({ <span class="s">&quot;def &quot;</span> name <span class="s">&quot;</span><span class="se">\n</span><span class="s">&quot;</span> })
                      <span class="nb">-&gt;</span> { name <span class="s">&quot;()</span><span class="se">\n</span><span class="s">&quot;</span> }
}
</pre></div>

`Buffer` is a specialised list that appends an element when called as a
function:

<div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">Buffer</span><span class="p">(</span><span class="nb">list</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__call__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">arg</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">arg</span><span class="p">)</span>
</pre></div>

Here is an example AST representing a program:

<div class="highlight"><pre><span></span><span class="n">AST</span> <span class="o">=</span> <span class="p">[</span>
    <span class="p">[</span><span class="s1">&#39;Program&#39;</span><span class="p">,</span>
        <span class="p">[</span><span class="s1">&#39;Function&#39;</span><span class="p">,</span> <span class="s1">&#39;foo&#39;</span><span class="p">],</span>
        <span class="p">[</span><span class="s1">&#39;Function&#39;</span><span class="p">,</span> <span class="s1">&#39;bar&#39;</span><span class="p">]</span>
    <span class="p">]</span>
<span class="p">]</span>
</pre></div>



When the `program` rule is run on the example input, the following is output:

<div class="highlight"><pre><span></span>$ <span></span>python example_buffers.py
<span></span># HEADER
def foo
def bar
# BODY
foo()
bar()
</pre></div>


### Remove dependency on Bash

### Extract assembler

...

### TODO/NOTES

* Clarity: How does it affect understandability/learnability/readability?
* Size: Lines of code.
* Flexibility: How easy is it to modify RLMeta to be what you need?
* Performance: How fast does it compile?

CHANGES:

It started with this goal:

    RLMeta Poster 2: Experiment with PyVM and see if it can improve "assembly
    code in code generator".

Then continued with this:

* Import all current code into smart notes document.

    * Added search of code notes to smart notes.

* Adapt to Python 3.

* PyVM (first version)

    *   Split PyVM into parser and codegenerator.

    *   Two passes (and thus two grammars) are needed if macros should be
        possible to define last.

    *   What is a good AST for PyVM?

    [x] Continue to build parse tree.

    [x] Figure out how to replace support library in make.py

    $ wc -l rlmeta/*; echo; wc -l pyvm/*
       66 rlmeta/codegenerator.rlmeta
       46 rlmeta/main.py
       58 rlmeta/parser.rlmeta
       77 rlmeta/support.py
      169 rlmeta/vm.pyvm
      416 total

      22 pyvm/codegenerator.rlmeta
      19 pyvm/parser.rlmeta
      35 pyvm/support.py
      76 total

[x] Better error message than None if runtime/scope is not found.

    * Rename match -> matches in Scope
    * Immutable scope instead and fail if entry does not exist?

    $ wc -l rlmeta/*; echo; wc -l pyvm/*
       66 rlmeta/codegenerator.rlmeta
       46 rlmeta/main.py
       58 rlmeta/parser.rlmeta
       68 rlmeta/support.py
      169 rlmeta/vm.pyvm
      407 total

      22 pyvm/codegenerator.rlmeta
      19 pyvm/parser.rlmeta
      35 pyvm/support.py
      76 total

[x] Generate instruction "enum" so that strings don't have to be used

[x] Counter class is more clean

    $ wc -l rlmeta/*; echo; wc -l pyvm/*
       66 rlmeta/codegenerator.rlmeta
       46 rlmeta/main.py
       58 rlmeta/parser.rlmeta
       72 rlmeta/support.py
      169 rlmeta/vm.pyvm
      411 total

      22 pyvm/codegenerator.rlmeta
      19 pyvm/parser.rlmeta
      35 pyvm/support.py
      76 total

* No need to wrap parser output in list for codegenerator in RLMeta

[x] Put compile + error reporting function in support lib.

[x] No need to wrap parser output in list for codegenerator in PyVM

[x] You can put any crap at end of file, and parsers don't care. Fix it!

[x] VM should not know about runtime.

[x] Move "assembly" out of support library. Grammar should generate
    labels/instructions.

[x] No failure if VM-compilation fails? (Swap Instruction arguments.)

[x] Support recursive macros?

    * Probably requires function to run grammar against an object.

    * Needed to get rid of duplicated call code.

[x] Split code generator into code generator and python assembler. That makes
    each phase more clear and allows for optimizations.

[x] Better AST for action expressions.

[x] Can "native" calls be removed by adding binding in runtime?

[x] Resolve labels in assembler.

[x] Get rid of PyVM

    [x] Compilation was further complicated now when VM has to be generated and
        a combined support library created.

    [x] Clean up PyVM grammars

    [x] Write VM as clean as possible in Python. Then write a separate
        optimized VM?

[x] Put object match expr tree in parser instead of in codegen?

    - This makes the VM more clean. There is only one instruction for matching
      and the matching is done with a Python lambda. The VM knows nothing about
      how to match a single object.

TODO:

[ ] Can support library (and new Runtime) become smaller?

[ ] Should memo be removed since it is an optimization?

[ ] VM

    [ ] Review VM and see if clarity can be improved.

        [ ] Can it be written with less optimizations in mind?

    [ ] Can fail_pos be handled better?

    [ ] MatchError probably has wrong stream if error occurs deep down

    [ ] I am not happy with how the new VM looks. A mix between classes and
        functions and helpers.

[ ] Add DEBUG flag that outputs source between passes.

[ ] Why not better error message when action wrong? Why index wrong?

      Action        = .:xs
      Action        = .*:xs

    [ ] Wrong pos is reported for "Not" instruction.

    -   "Not" messes up latest_fail_pos in general. It should perhaps be
        disabled during a "Not"?

[ ] Poster with intermediate versions shown.

    * Interactive on the web. (Requires JS version.)

[ ] Try to port to JS to see how flexible it is?

[ ] Rename ast to tree?

[ ] Lists can be repeated and xs refers to last match?

    echo 'G{x=[.:xs]*}' | python rlmeta.py

[ ] Lookup concat/splice/join/indent?

## Code listings for RLMeta

Here is all the source code and also the make script.

### src/parser.rlmeta

<div class="highlight"><pre><span></span>Parser {
  file <span class="nb">=</span>
    <span class="nb">|</span> (space grammar)<span class="nc">*</span><span class="nb">:</span>xs space <span class="nc">!.</span>            <span class="nb">-&gt;</span> xs
  grammar <span class="nb">=</span>
    <span class="nb">|</span> name<span class="nb">:</span>x space <span class="sc">&#39;{&#39;</span> rule<span class="nc">*</span><span class="nb">:</span>ys space <span class="sc">&#39;}&#39;</span>     <span class="nb">-&gt;</span> [<span class="s">&quot;Grammar&quot;</span> x <span class="nc">~</span>ys]
  rule <span class="nb">=</span>
    <span class="nb">|</span> name<span class="nb">:</span>x space <span class="sc">&#39;=&#39;</span> choice<span class="nb">:</span>y               <span class="nb">-&gt;</span> [<span class="s">&quot;Rule&quot;</span> x y]
  choice <span class="nb">=</span>
    <span class="nb">|</span> (space <span class="sc">&#39;|&#39;</span>)<span class="nc">?</span>
      sequence<span class="nb">:</span>x (space <span class="sc">&#39;|&#39;</span> sequence)<span class="nc">*</span><span class="nb">:</span>xs     <span class="nb">-&gt;</span> [<span class="s">&quot;Or&quot;</span> x <span class="nc">~</span>xs]
  sequence <span class="nb">=</span>
    <span class="nb">|</span> expr<span class="nc">*</span><span class="nb">:</span>xs maybeAction<span class="nb">:</span>ys                 <span class="nb">-&gt;</span> [<span class="s">&quot;Scope&quot;</span> [<span class="s">&quot;And&quot;</span> <span class="nc">~</span>xs <span class="nc">~</span>ys]]
  expr <span class="nb">=</span>
    <span class="nb">|</span> expr1<span class="nb">:</span>x space <span class="sc">&#39;:&#39;</span> name<span class="nb">:</span>y                <span class="nb">-&gt;</span> [<span class="s">&quot;Bind&quot;</span> y x]
    <span class="nb">|</span> expr1
  expr1 <span class="nb">=</span>
    <span class="nb">|</span> expr2<span class="nb">:</span>x space <span class="sc">&#39;*&#39;</span>                       <span class="nb">-&gt;</span> [<span class="s">&quot;Star&quot;</span> x]
    <span class="nb">|</span> expr2<span class="nb">:</span>x space <span class="sc">&#39;?&#39;</span>                       <span class="nb">-&gt;</span> [<span class="s">&quot;Or&quot;</span> x [<span class="s">&quot;And&quot;</span>]]
    <span class="nb">|</span> space <span class="sc">&#39;!&#39;</span> expr2<span class="nb">:</span>x                       <span class="nb">-&gt;</span> [<span class="s">&quot;Not&quot;</span> x]
    <span class="nb">|</span> space <span class="sc">&#39;%&#39;</span>                               <span class="nb">-&gt;</span> [<span class="s">&quot;MatchCallRule&quot;</span>]
    <span class="nb">|</span> expr2
  expr2 <span class="nb">=</span>
    <span class="nb">|</span> name<span class="nb">:</span>x <span class="nc">!</span>(space <span class="sc">&#39;=&#39;</span>)                     <span class="nb">-&gt;</span> [<span class="s">&quot;MatchRule&quot;</span> x]
    <span class="nb">|</span> space char<span class="nb">:</span>x <span class="sc">&#39;-&#39;</span> char<span class="nb">:</span>y                 <span class="nb">-&gt;</span> [<span class="s">&quot;MatchObject&quot;</span> [<span class="s">&quot;Range&quot;</span> x y]]
    <span class="nb">|</span> space <span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span> (<span class="nc">!</span><span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span> matchChar)<span class="nc">*</span><span class="nb">:</span>xs <span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span>   <span class="nb">-&gt;</span> [<span class="s">&quot;And&quot;</span> <span class="nc">~</span>xs]
    <span class="nb">|</span> space <span class="sc">&#39;.&#39;</span>                               <span class="nb">-&gt;</span> [<span class="s">&quot;MatchObject&quot;</span> [<span class="s">&quot;Any&quot;</span>]]
    <span class="nb">|</span> space <span class="sc">&#39;(&#39;</span> choice<span class="nb">:</span>x space <span class="sc">&#39;)&#39;</span>            <span class="nb">-&gt;</span> x
    <span class="nb">|</span> space <span class="sc">&#39;[&#39;</span> expr<span class="nc">*</span><span class="nb">:</span>xs space <span class="sc">&#39;]&#39;</span>            <span class="nb">-&gt;</span> [<span class="s">&quot;MatchList&quot;</span> [<span class="s">&quot;And&quot;</span> <span class="nc">~</span>xs]]
  matchChar <span class="nb">=</span>
    <span class="nb">|</span> innerChar<span class="nb">:</span>x                             <span class="nb">-&gt;</span> [<span class="s">&quot;MatchObject&quot;</span> [<span class="s">&quot;Eq&quot;</span> x]]
  maybeAction <span class="nb">=</span>
    <span class="nb">|</span> actionExpr<span class="nb">:</span>x                            <span class="nb">-&gt;</span> [[<span class="s">&quot;Action&quot;</span> x]]
    <span class="nb">|</span>                                         <span class="nb">-&gt;</span> []
  actionExpr <span class="nb">=</span>
    <span class="nb">|</span> space <span class="sc">&#39;-&gt;&#39;</span> hostExpr<span class="nb">:</span>x
      (space <span class="sc">&#39;:&#39;</span> name <span class="nb">|</span> <span class="nb">-&gt;</span> <span class="s">&quot;&quot;</span>)<span class="nb">:</span>y actionExpr<span class="nb">:</span>z <span class="nb">-&gt;</span> [<span class="s">&quot;Set&quot;</span> y x z]
    <span class="nb">|</span> space <span class="sc">&#39;-&gt;&#39;</span> hostExpr<span class="nb">:</span>x                   <span class="nb">-&gt;</span> x
  hostExpr <span class="nb">=</span>
    <span class="nb">|</span> space string<span class="nb">:</span>x                          <span class="nb">-&gt;</span> [<span class="s">&quot;String&quot;</span> x]
    <span class="nb">|</span> space <span class="sc">&#39;[&#39;</span> hostListItem<span class="nc">*</span><span class="nb">:</span>xs space <span class="sc">&#39;]&#39;</span>    <span class="nb">-&gt;</span> [<span class="s">&quot;List&quot;</span> <span class="nc">~</span>xs]
    <span class="nb">|</span> space <span class="sc">&#39;{&#39;</span> formatExpr<span class="nc">*</span><span class="nb">:</span>xs space <span class="sc">&#39;}&#39;</span>      <span class="nb">-&gt;</span> [<span class="s">&quot;Format&quot;</span> <span class="nc">~</span>xs]
    <span class="nb">|</span> var<span class="nb">:</span>x space <span class="sc">&#39;(&#39;</span> hostExpr<span class="nc">*</span><span class="nb">:</span>ys space <span class="sc">&#39;)&#39;</span>  <span class="nb">-&gt;</span> [<span class="s">&quot;Call&quot;</span> x <span class="nc">~</span>ys]
    <span class="nb">|</span> var<span class="nb">:</span>x
  hostListItem <span class="nb">=</span>
    <span class="nb">|</span> space <span class="sc">&#39;~&#39;</span><span class="nc">*</span><span class="nb">:</span>ys hostExpr<span class="nb">:</span>x                <span class="nb">-&gt;</span> [<span class="s">&quot;ListItem&quot;</span> len(ys) x]
  formatExpr <span class="nb">=</span>
    <span class="nb">|</span> space <span class="sc">&#39;&gt;&#39;</span> formatExpr<span class="nc">*</span><span class="nb">:</span>xs space <span class="sc">&#39;&lt;&#39;</span>      <span class="nb">-&gt;</span> [<span class="s">&quot;Indent&quot;</span> [<span class="s">&quot;Format&quot;</span> <span class="nc">~</span>xs]]
    <span class="nb">|</span> hostExpr
  var <span class="nb">=</span>
    <span class="nb">|</span> name<span class="nb">:</span>x <span class="nc">!</span>(space <span class="sc">&#39;=&#39;</span>)                     <span class="nb">-&gt;</span> [<span class="s">&quot;Lookup&quot;</span> x]
  string    <span class="nb">=</span> <span class="sc">&#39;&quot;&#39;</span>  (<span class="nc">!</span><span class="sc">&#39;&quot;&#39;</span>  innerChar)<span class="nc">*</span><span class="nb">:</span>xs <span class="sc">&#39;&quot;&#39;</span>  <span class="nb">-&gt;</span> { xs }
  char      <span class="nb">=</span> <span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span>  <span class="nc">!</span><span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span> innerChar  <span class="nb">:</span>x  <span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span> <span class="nb">-&gt;</span> x
  innerChar <span class="nb">=</span> <span class="sc">&#39;</span><span class="se">\\</span><span class="sc">&#39;</span> escape <span class="nb">|</span> <span class="nc">.</span>
  escape    <span class="nb">=</span> <span class="sc">&#39;</span><span class="se">\\</span><span class="sc">&#39;</span> <span class="nb">-&gt;</span> <span class="s">&quot;</span><span class="se">\\</span><span class="s">&quot;</span> <span class="nb">|</span> <span class="sc">&#39;</span><span class="se">\&#39;</span><span class="sc">&#39;</span> <span class="nb">-&gt;</span> <span class="s">&quot;&#39;&quot;</span>
            <span class="nb">|</span> <span class="sc">&#39;&quot;&#39;</span>  <span class="nb">-&gt;</span> <span class="s">&quot;</span><span class="se">\&quot;</span><span class="s">&quot;</span> <span class="nb">|</span> <span class="sc">&#39;n&#39;</span>  <span class="nb">-&gt;</span> <span class="s">&quot;</span><span class="se">\n</span><span class="s">&quot;</span>
  name      <span class="nb">=</span> space nameStart<span class="nb">:</span>x nameChar<span class="nc">*</span><span class="nb">:</span>xs  <span class="nb">-&gt;</span> { x xs }
  nameStart <span class="nb">=</span> <span class="sc">&#39;a&#39;</span><span class="nc">-</span><span class="sc">&#39;z&#39;</span> <span class="nb">|</span> <span class="sc">&#39;A&#39;</span><span class="nc">-</span><span class="sc">&#39;Z&#39;</span>
  nameChar  <span class="nb">=</span> <span class="sc">&#39;a&#39;</span><span class="nc">-</span><span class="sc">&#39;z&#39;</span> <span class="nb">|</span> <span class="sc">&#39;A&#39;</span><span class="nc">-</span><span class="sc">&#39;Z&#39;</span> <span class="nb">|</span> <span class="sc">&#39;0&#39;</span><span class="nc">-</span><span class="sc">&#39;9&#39;</span>
  space     <span class="nb">=</span> (<span class="sc">&#39; &#39;</span> <span class="nb">|</span> <span class="sc">&#39;</span><span class="se">\n</span><span class="sc">&#39;</span>)<span class="nc">*</span>
}
</pre></div>

### src/codegenerator.rlmeta

<div class="highlight"><pre><span></span>CodeGenerator {
  Grammar       <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nc">*</span><span class="nb">:</span>ys <span class="nb">-&gt;</span> [<span class="s">&quot;Grammar&quot;</span> x <span class="nc">~~</span>ys]
  Rule          <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nb">:</span>y   <span class="nb">-&gt;</span> [[<span class="s">&quot;Rule&quot;</span> x]
                                  <span class="nc">~</span>y
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;RETURN&quot;</span>]]
  Or            <span class="nb">=</span>
    <span class="nb">|</span> ast<span class="nb">:</span>x Or<span class="nb">:</span>y              <span class="nb">-&gt;</span> label()<span class="nb">:</span>a <span class="nb">-&gt;</span> label()<span class="nb">:</span>b
                              <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;BACKTRACK&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> a]
                                  <span class="nc">~</span>x
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;COMMIT&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> b]
                                  [<span class="s">&quot;Label&quot;</span> a]
                                  <span class="nc">~</span>y
                                  [<span class="s">&quot;Label&quot;</span> b]]
    <span class="nb">|</span> ast
  Scope         <span class="nb">=</span> ast<span class="nb">:</span>x       <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;PUSH_SCOPE&quot;</span>]
                                  <span class="nc">~</span>x
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;POP_SCOPE&quot;</span>]]
  And           <span class="nb">=</span> ast<span class="nc">*</span><span class="nb">:</span>xs     <span class="nb">-&gt;</span> [<span class="nc">~~</span>xs]
  Bind          <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nb">:</span>y   <span class="nb">-&gt;</span> [<span class="nc">~</span>y
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;BIND&quot;</span>]
                                  [<span class="s">&quot;Value&quot;</span> x]]
  Star          <span class="nb">=</span> ast<span class="nb">:</span>x       <span class="nb">-&gt;</span> label()<span class="nb">:</span>a <span class="nb">-&gt;</span> label()<span class="nb">:</span>b
                              <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;LIST_START&quot;</span>]
                                  [<span class="s">&quot;Label&quot;</span> a]
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;BACKTRACK&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> b]
                                  <span class="nc">~</span>x
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;LIST_APPEND&quot;</span>]
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;COMMIT&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> a]
                                  [<span class="s">&quot;Label&quot;</span> b]
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;LIST_END&quot;</span>]]
  Not           <span class="nb">=</span> ast<span class="nb">:</span>x       <span class="nb">-&gt;</span> label()<span class="nb">:</span>a <span class="nb">-&gt;</span> label()<span class="nb">:</span>b
                              <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;BACKTRACK&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> b]
                                  <span class="nc">~</span>x
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;COMMIT&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> a]
                                  [<span class="s">&quot;Label&quot;</span> a]
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;FAIL&quot;</span>]
                                  [<span class="s">&quot;Value&quot;</span> <span class="s">&quot;no match&quot;</span>]
                                  [<span class="s">&quot;Label&quot;</span> b]]
  MatchCallRule <span class="nb">=</span>             <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;MATCH_CALL_RULE&quot;</span>]]
  MatchRule     <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x         <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;CALL&quot;</span>]
                                  [<span class="s">&quot;Target&quot;</span> x]]
  MatchObject   <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x         <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;MATCH&quot;</span>]
                                  x]
  MatchList     <span class="nb">=</span> ast<span class="nb">:</span>x       <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;PUSH_STREAM&quot;</span>]
                                  <span class="nc">~</span>x
                                  [<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;POP_STREAM&quot;</span>]]
  Action        <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x         <span class="nb">-&gt;</span> [[<span class="s">&quot;OpCode&quot;</span> <span class="s">&quot;ACTION&quot;</span>]
                                  [<span class="s">&quot;Action&quot;</span> x]]
  asts          <span class="nb">=</span> ast<span class="nc">*</span><span class="nb">:</span>xs <span class="nc">!.</span>  <span class="nb">-&gt;</span> xs
  ast           <span class="nb">=</span> [<span class="nc">%</span><span class="nb">:</span>x]       <span class="nb">-&gt;</span> x
}
</pre></div>

### src/assembler.rlmeta

<div class="highlight"><pre><span></span>Assembler {
  Grammar  <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nc">*</span><span class="nb">:</span>ys     <span class="nb">-&gt;</span> list()<span class="nb">:</span>rules
                             <span class="nb">-&gt;</span> list()<span class="nb">:</span>code
                             <span class="nb">-&gt;</span> dict()<span class="nb">:</span>labels
                             <span class="nb">-&gt;</span> list()<span class="nb">:</span>patches
                             <span class="nb">-&gt;</span> ys
                             <span class="nb">-&gt;</span> run(<span class="s">&quot;asts&quot;</span> patches)
                             <span class="nb">-&gt;</span> { <span class="s">&quot;class &quot;</span> x <span class="s">&quot;(Grammar):</span><span class="se">\n</span><span class="s">&quot;</span> &gt;
                                    <span class="s">&quot;rules = {</span><span class="se">\n</span><span class="s">&quot;</span> &gt; join(rules <span class="s">&quot;,</span><span class="se">\n</span><span class="s">&quot;</span>) &lt; <span class="s">&quot;</span><span class="se">\n</span><span class="s">}</span><span class="se">\n</span><span class="s">&quot;</span>
                                    <span class="s">&quot;code = [</span><span class="se">\n</span><span class="s">&quot;</span> &gt; join(code  <span class="s">&quot;,</span><span class="se">\n</span><span class="s">&quot;</span>) &lt; <span class="s">&quot;</span><span class="se">\n</span><span class="s">]</span><span class="se">\n</span><span class="s">&quot;</span>
                                  &lt; }
  Rule     <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(rules { repr(x) <span class="s">&quot;: &quot;</span> len(code) })
                             <span class="nb">-&gt;</span> set(labels x len(code))
  Label    <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> set(labels x len(code))
  Target   <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(patches [<span class="s">&quot;Patch&quot;</span> len(code) x])
                             <span class="nb">-&gt;</span> add(code <span class="s">&quot;placeholder&quot;</span>)
  Patch    <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x <span class="nc">.</span><span class="nb">:</span>y         <span class="nb">-&gt;</span> set(code x get(labels y))
  OpCode   <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(code x)
  Value    <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(code repr(x))
  Eq       <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> add(code repr(x))
                             <span class="nb">-&gt;</span> add(code { <span class="s">&quot;lambda x: x == &quot;</span> repr(x) })
  Range    <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x <span class="nc">.</span><span class="nb">:</span>y         <span class="nb">-&gt;</span> add(code repr({<span class="s">&quot;range &quot;</span> repr(x) <span class="s">&quot;-&quot;</span> repr(y)}))
                             <span class="nb">-&gt;</span> add(code { <span class="s">&quot;lambda x: &quot;</span> repr(x) <span class="s">&quot; &lt;= x &lt;= &quot;</span> repr(y) })
  Any      <span class="nb">=</span>                 <span class="nb">-&gt;</span> add(code repr(<span class="s">&quot;any&quot;</span>))
                             <span class="nb">-&gt;</span> add(code <span class="s">&quot;lambda x: True&quot;</span>)
  Action   <span class="nb">=</span> ast<span class="nb">:</span>x           <span class="nb">-&gt;</span> add(code {<span class="s">&quot;lambda self: &quot;</span> x})
  Set      <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nb">:</span>y ast<span class="nb">:</span>z <span class="nb">-&gt;</span> { <span class="s">&quot;self.bind(&quot;</span> repr(x) <span class="s">&quot;, &quot;</span> y <span class="s">&quot;, lambda: &quot;</span> z <span class="s">&quot;)&quot;</span> }
  String   <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> repr(x)
  List     <span class="nb">=</span> astList<span class="nb">:</span>x       <span class="nb">-&gt;</span> { <span class="s">&quot;concat([&quot;</span> x <span class="s">&quot;])&quot;</span> }
  ListItem <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x ast<span class="nb">:</span>y       <span class="nb">-&gt;</span> { <span class="s">&quot;splice(&quot;</span> repr(x) <span class="s">&quot;, &quot;</span> y <span class="s">&quot;)&quot;</span> }
  Format   <span class="nb">=</span> astList<span class="nb">:</span>x       <span class="nb">-&gt;</span> { <span class="s">&quot;join([&quot;</span> x <span class="s">&quot;])&quot;</span> }
  Indent   <span class="nb">=</span> ast<span class="nb">:</span>x           <span class="nb">-&gt;</span> { <span class="s">&quot;indent(&quot;</span> x <span class="s">&quot;, &quot;</span>
                                  <span class="s">&quot;self.lookup(&#39;indentprefix&#39;))&quot;</span> }
  Call     <span class="nb">=</span> ast<span class="nb">:</span>x astList<span class="nb">:</span>y <span class="nb">-&gt;</span> { x <span class="s">&quot;(&quot;</span> y <span class="s">&quot;)&quot;</span> }
  Lookup   <span class="nb">=</span> <span class="nc">.</span><span class="nb">:</span>x             <span class="nb">-&gt;</span> { <span class="s">&quot;self.lookup(&quot;</span> repr(x) <span class="s">&quot;)&quot;</span> }
  asts     <span class="nb">=</span> ast<span class="nc">*</span><span class="nb">:</span>xs <span class="nc">!.</span>      <span class="nb">-&gt;</span> { xs }
  astList  <span class="nb">=</span> ast<span class="nc">*</span><span class="nb">:</span>xs         <span class="nb">-&gt;</span> join(xs <span class="s">&quot;, &quot;</span>)
  ast      <span class="nb">=</span> [<span class="nc">%</span><span class="nb">:</span>x]           <span class="nb">-&gt;</span> x
}
</pre></div>

### src/support.py

<div class="highlight"><pre><span></span><span class="k">class</span> <span class="nc">VM</span><span class="p">:</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">code</span><span class="p">,</span> <span class="n">rules</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">code</span> <span class="o">=</span> <span class="n">code</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">rules</span> <span class="o">=</span> <span class="n">rules</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">start_rule</span><span class="p">,</span> <span class="n">stream</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">action</span> <span class="o">=</span> <span class="n">SemanticAction</span><span class="p">(</span><span class="kc">None</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pc</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">rules</span><span class="p">[</span><span class="n">start_rule</span><span class="p">]</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">call_backtrack_stack</span> <span class="o">=</span> <span class="p">[]</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">stream_rest</span> <span class="o">=</span> <span class="p">(</span><span class="n">stream</span><span class="p">,</span> <span class="kc">None</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">pos_rest</span> <span class="o">=</span> <span class="p">(</span><span class="mi">0</span><span class="p">,</span> <span class="nb">tuple</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">scope_rest</span> <span class="o">=</span> <span class="p">(</span><span class="kc">None</span><span class="p">,</span> <span class="kc">None</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">latest_fail_message</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">latest_fail_pos</span> <span class="o">=</span> <span class="p">(</span><span class="kc">None</span><span class="p">,</span> <span class="nb">tuple</span><span class="p">())</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">memo</span> <span class="o">=</span> <span class="p">{}</span>
        <span class="k">while</span> <span class="kc">True</span><span class="p">:</span>
            <span class="n">result</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">()(</span><span class="bp">self</span><span class="p">)</span>
            <span class="k">if</span> <span class="n">result</span><span class="p">:</span>
                <span class="k">return</span> <span class="n">result</span>

    <span class="k">def</span> <span class="nf">pop_arg</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">code</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">code</span><span class="p">[</span><span class="bp">self</span><span class="o">.</span><span class="n">pc</span><span class="p">]</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pc</span> <span class="o">+=</span> <span class="mi">1</span>
        <span class="k">return</span> <span class="n">code</span>

<span class="k">def</span> <span class="nf">PUSH_SCOPE</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span> <span class="o">=</span> <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span><span class="p">)</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope</span> <span class="o">=</span> <span class="p">{}</span>

<span class="k">def</span> <span class="nf">POP_SCOPE</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span>

<span class="k">def</span> <span class="nf">BACKTRACK</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="o">.</span><span class="n">append</span><span class="p">((</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">(),</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span>
    <span class="p">))</span>

<span class="k">def</span> <span class="nf">COMMIT</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="o">.</span><span class="n">pop</span><span class="p">()</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">pc</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">()</span>

<span class="k">def</span> <span class="nf">CALL</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">CALL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">())</span>

<span class="k">def</span> <span class="nf">CALL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">pc</span><span class="p">):</span>
    <span class="n">key</span> <span class="o">=</span> <span class="p">(</span><span class="n">pc</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="o">+</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,))</span>
    <span class="k">if</span> <span class="n">key</span> <span class="ow">in</span> <span class="n">vm</span><span class="o">.</span><span class="n">memo</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">vm</span><span class="o">.</span><span class="n">memo</span><span class="p">[</span><span class="n">key</span><span class="p">][</span><span class="mi">0</span><span class="p">]</span> <span class="ow">is</span> <span class="kc">None</span><span class="p">:</span>
            <span class="n">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">memo</span><span class="p">[</span><span class="n">key</span><span class="p">][</span><span class="mi">1</span><span class="p">])</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="n">vm</span><span class="o">.</span><span class="n">action</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">memo</span><span class="p">[</span><span class="n">key</span><span class="p">]</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="o">.</span><span class="n">append</span><span class="p">((</span><span class="n">vm</span><span class="o">.</span><span class="n">pc</span><span class="p">,</span> <span class="n">key</span><span class="p">))</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pc</span> <span class="o">=</span> <span class="n">pc</span>

<span class="k">def</span> <span class="nf">RETURN</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="k">if</span> <span class="ow">not</span> <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="p">:</span>
        <span class="k">return</span> <span class="n">vm</span><span class="o">.</span><span class="n">action</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">pc</span><span class="p">,</span> <span class="n">key</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="o">.</span><span class="n">pop</span><span class="p">()</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">memo</span><span class="p">[</span><span class="n">key</span><span class="p">]</span> <span class="o">=</span> <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">action</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">MATCH</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">object_description</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">()</span>
    <span class="n">fn</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">()</span>
    <span class="n">MATCH_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">fn</span><span class="p">,</span> <span class="p">(</span><span class="s2">&quot;expected </span><span class="si">{}</span><span class="s2">&quot;</span><span class="p">,</span> <span class="n">object_description</span><span class="p">))</span>

<span class="k">def</span> <span class="nf">MATCH_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">fn</span><span class="p">,</span> <span class="n">message</span><span class="p">):</span>
    <span class="k">if</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span> <span class="o">&gt;=</span> <span class="nb">len</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">)</span> <span class="ow">or</span> <span class="ow">not</span> <span class="n">fn</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">[</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">]):</span>
        <span class="n">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">message</span><span class="p">)</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">match</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">[</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">]</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">action</span> <span class="o">=</span> <span class="n">SemanticAction</span><span class="p">(</span><span class="n">match</span><span class="p">)</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pos</span> <span class="o">+=</span> <span class="mi">1</span>

<span class="k">def</span> <span class="nf">MATCH_CALL_RULE</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">MATCH_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="k">lambda</span> <span class="n">x</span><span class="p">:</span> <span class="n">x</span> <span class="ow">in</span> <span class="n">vm</span><span class="o">.</span><span class="n">rules</span><span class="p">,</span> <span class="p">(</span><span class="s2">&quot;expected rule name&quot;</span><span class="p">,))</span>
    <span class="n">CALL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">rules</span><span class="p">[</span><span class="n">vm</span><span class="o">.</span><span class="n">action</span><span class="o">.</span><span class="n">value</span><span class="p">])</span>

<span class="k">def</span> <span class="nf">LIST_START</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span> <span class="o">=</span> <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span><span class="p">)</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope</span> <span class="o">=</span> <span class="p">[]</span>

<span class="k">def</span> <span class="nf">LIST_APPEND</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">action</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">LIST_END</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">action</span> <span class="o">=</span> <span class="n">SemanticAction</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="k">lambda</span> <span class="bp">self</span><span class="p">:</span> <span class="p">[</span><span class="n">x</span><span class="o">.</span><span class="n">eval</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">runtime</span><span class="p">)</span> <span class="k">for</span> <span class="n">x</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">value</span><span class="p">])</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span>

<span class="k">def</span> <span class="nf">BIND</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">[</span><span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">()]</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">action</span>

<span class="k">def</span> <span class="nf">ACTION</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">vm</span><span class="o">.</span><span class="n">action</span> <span class="o">=</span> <span class="n">SemanticAction</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">())</span>

<span class="k">def</span> <span class="nf">PUSH_STREAM</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="k">if</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span> <span class="o">&gt;=</span> <span class="nb">len</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">)</span> <span class="ow">or</span> <span class="ow">not</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">[</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">],</span> <span class="nb">list</span><span class="p">):</span>
        <span class="n">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="p">(</span><span class="s2">&quot;expected list&quot;</span><span class="p">,))</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span> <span class="o">=</span> <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span><span class="p">)</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span> <span class="o">+</span> <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,)</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">stream</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">[</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">]</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pos</span> <span class="o">=</span> <span class="mi">0</span>

<span class="k">def</span> <span class="nf">POP_STREAM</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="k">if</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span> <span class="o">&lt;</span> <span class="nb">len</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">):</span>
        <span class="n">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="p">(</span><span class="s2">&quot;expected end of list&quot;</span><span class="p">,))</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="p">[</span><span class="o">-</span><span class="mi">1</span><span class="p">],</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="p">[:</span><span class="o">-</span><span class="mi">1</span><span class="p">]</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">pos</span> <span class="o">+=</span> <span class="mi">1</span>

<span class="k">def</span> <span class="nf">FAIL</span><span class="p">(</span><span class="n">vm</span><span class="p">):</span>
    <span class="n">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">pop_arg</span><span class="p">(),))</span>

<span class="k">def</span> <span class="nf">FAIL_</span><span class="p">(</span><span class="n">vm</span><span class="p">,</span> <span class="n">fail_message</span><span class="p">):</span>
    <span class="n">fail_pos</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="o">+</span><span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,)</span>
    <span class="k">if</span> <span class="n">fail_pos</span> <span class="o">&gt;=</span> <span class="n">vm</span><span class="o">.</span><span class="n">latest_fail_pos</span><span class="p">:</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">latest_fail_message</span> <span class="o">=</span> <span class="n">fail_message</span>
        <span class="n">vm</span><span class="o">.</span><span class="n">latest_fail_pos</span> <span class="o">=</span> <span class="n">fail_pos</span>
    <span class="n">call_backtrack_entry</span> <span class="o">=</span> <span class="nb">tuple</span><span class="p">()</span>
    <span class="k">while</span> <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="p">:</span>
        <span class="n">call_backtrack_entry</span> <span class="o">=</span> <span class="n">vm</span><span class="o">.</span><span class="n">call_backtrack_stack</span><span class="o">.</span><span class="n">pop</span><span class="p">()</span>
        <span class="k">if</span> <span class="nb">len</span><span class="p">(</span><span class="n">call_backtrack_entry</span><span class="p">)</span> <span class="o">==</span> <span class="mi">7</span><span class="p">:</span>
            <span class="k">break</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="n">vm</span><span class="o">.</span><span class="n">memo</span><span class="p">[</span><span class="n">call_backtrack_entry</span><span class="p">[</span><span class="mi">1</span><span class="p">]]</span> <span class="o">=</span> <span class="p">(</span><span class="kc">None</span><span class="p">,</span> <span class="n">fail_message</span><span class="p">)</span>
    <span class="k">if</span> <span class="nb">len</span><span class="p">(</span><span class="n">call_backtrack_entry</span><span class="p">)</span> <span class="o">!=</span> <span class="mi">7</span><span class="p">:</span>
        <span class="k">raise</span> <span class="n">MatchError</span><span class="p">(</span>
            <span class="n">vm</span><span class="o">.</span><span class="n">latest_fail_message</span><span class="p">[</span><span class="mi">0</span><span class="p">]</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="o">*</span><span class="n">vm</span><span class="o">.</span><span class="n">latest_fail_message</span><span class="p">[</span><span class="mi">1</span><span class="p">:]),</span>
            <span class="n">vm</span><span class="o">.</span><span class="n">latest_fail_pos</span><span class="p">[</span><span class="o">-</span><span class="mi">1</span><span class="p">],</span>
            <span class="n">vm</span><span class="o">.</span><span class="n">stream</span>
        <span class="p">)</span>
    <span class="p">(</span><span class="n">vm</span><span class="o">.</span><span class="n">pc</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">stream_rest</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">pos_rest</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope</span><span class="p">,</span> <span class="n">vm</span><span class="o">.</span><span class="n">scope_rest</span><span class="p">)</span> <span class="o">=</span> <span class="n">call_backtrack_entry</span>

<span class="k">class</span> <span class="nc">SemanticAction</span><span class="p">(</span><span class="nb">object</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">value</span><span class="p">,</span> <span class="n">fn</span><span class="o">=</span><span class="k">lambda</span> <span class="bp">self</span><span class="p">:</span> <span class="bp">self</span><span class="o">.</span><span class="n">value</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">value</span> <span class="o">=</span> <span class="n">value</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">fn</span> <span class="o">=</span> <span class="n">fn</span>

    <span class="k">def</span> <span class="nf">eval</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">runtime</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">runtime</span> <span class="o">=</span> <span class="n">runtime</span>
        <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">fn</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>

    <span class="k">def</span> <span class="nf">bind</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">name</span><span class="p">,</span> <span class="n">value</span><span class="p">,</span> <span class="n">continuation</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">runtime</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">runtime</span><span class="o">.</span><span class="n">set</span><span class="p">(</span><span class="n">name</span><span class="p">,</span> <span class="n">value</span><span class="p">)</span>
        <span class="k">return</span> <span class="n">continuation</span><span class="p">()</span>

    <span class="k">def</span> <span class="nf">lookup</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">name</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">name</span> <span class="ow">in</span> <span class="bp">self</span><span class="o">.</span><span class="n">value</span><span class="p">:</span>
            <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">value</span><span class="p">[</span><span class="n">name</span><span class="p">]</span><span class="o">.</span><span class="n">eval</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">runtime</span><span class="p">)</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="k">return</span> <span class="bp">self</span><span class="o">.</span><span class="n">runtime</span><span class="p">[</span><span class="n">name</span><span class="p">]</span>

<span class="k">class</span> <span class="nc">MatchError</span><span class="p">(</span><span class="ne">Exception</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">message</span><span class="p">,</span> <span class="n">pos</span><span class="p">,</span> <span class="n">stream</span><span class="p">):</span>
        <span class="ne">Exception</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">message</span> <span class="o">=</span> <span class="n">message</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">pos</span> <span class="o">=</span> <span class="n">pos</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">stream</span> <span class="o">=</span> <span class="n">stream</span>

<span class="k">class</span> <span class="nc">Grammar</span><span class="p">(</span><span class="nb">object</span><span class="p">):</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">rule</span><span class="p">,</span> <span class="n">stream</span><span class="p">,</span> <span class="n">runtime</span><span class="o">=</span><span class="p">{}):</span>
        <span class="k">return</span> <span class="n">Runtime</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="nb">dict</span><span class="p">(</span><span class="n">runtime</span><span class="p">,</span> <span class="o">**</span><span class="p">{</span>
            <span class="s2">&quot;label&quot;</span><span class="p">:</span> <span class="n">Counter</span><span class="p">(),</span>
            <span class="s2">&quot;indentprefix&quot;</span><span class="p">:</span> <span class="s2">&quot;    &quot;</span><span class="p">,</span>
            <span class="s2">&quot;list&quot;</span><span class="p">:</span> <span class="nb">list</span><span class="p">,</span>
            <span class="s2">&quot;dict&quot;</span><span class="p">:</span> <span class="nb">dict</span><span class="p">,</span>
            <span class="s2">&quot;add&quot;</span><span class="p">:</span> <span class="k">lambda</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">:</span> <span class="n">x</span><span class="o">.</span><span class="n">append</span><span class="p">(</span><span class="n">y</span><span class="p">),</span>
            <span class="s2">&quot;get&quot;</span><span class="p">:</span> <span class="k">lambda</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">:</span> <span class="n">x</span><span class="p">[</span><span class="n">y</span><span class="p">],</span>
            <span class="s2">&quot;set&quot;</span><span class="p">:</span> <span class="k">lambda</span> <span class="n">x</span><span class="p">,</span> <span class="n">y</span><span class="p">,</span> <span class="n">z</span><span class="p">:</span> <span class="n">x</span><span class="o">.</span><span class="fm">__setitem__</span><span class="p">(</span><span class="n">y</span><span class="p">,</span> <span class="n">z</span><span class="p">),</span>
            <span class="s2">&quot;len&quot;</span><span class="p">:</span> <span class="nb">len</span><span class="p">,</span>
            <span class="s2">&quot;repr&quot;</span><span class="p">:</span> <span class="nb">repr</span><span class="p">,</span>
            <span class="s2">&quot;join&quot;</span><span class="p">:</span> <span class="n">join</span><span class="p">,</span>
        <span class="p">}))</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">rule</span><span class="p">,</span> <span class="n">stream</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Runtime</span><span class="p">(</span><span class="nb">dict</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">grammar</span><span class="p">,</span> <span class="n">values</span><span class="p">):</span>
        <span class="nb">dict</span><span class="o">.</span><span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="nb">dict</span><span class="p">(</span><span class="n">values</span><span class="p">,</span> <span class="n">run</span><span class="o">=</span><span class="bp">self</span><span class="o">.</span><span class="n">run</span><span class="p">))</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">grammar</span> <span class="o">=</span> <span class="n">grammar</span>

    <span class="k">def</span> <span class="nf">set</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">key</span><span class="p">,</span> <span class="n">value</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">Runtime</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">grammar</span><span class="p">,</span> <span class="nb">dict</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="o">**</span><span class="p">{</span><span class="n">key</span><span class="p">:</span> <span class="n">value</span><span class="p">}))</span>

    <span class="k">def</span> <span class="nf">run</span><span class="p">(</span><span class="bp">self</span><span class="p">,</span> <span class="n">rule</span><span class="p">,</span> <span class="n">stream</span><span class="p">):</span>
        <span class="k">return</span> <span class="n">VM</span><span class="p">(</span><span class="bp">self</span><span class="o">.</span><span class="n">grammar</span><span class="o">.</span><span class="n">code</span><span class="p">,</span> <span class="bp">self</span><span class="o">.</span><span class="n">grammar</span><span class="o">.</span><span class="n">rules</span><span class="p">)</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">rule</span><span class="p">,</span> <span class="n">stream</span><span class="p">)</span><span class="o">.</span><span class="n">eval</span><span class="p">(</span><span class="bp">self</span><span class="p">)</span>

<span class="k">class</span> <span class="nc">Counter</span><span class="p">(</span><span class="nb">object</span><span class="p">):</span>

    <span class="k">def</span> <span class="fm">__init__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">value</span> <span class="o">=</span> <span class="mi">0</span>

    <span class="k">def</span> <span class="fm">__call__</span><span class="p">(</span><span class="bp">self</span><span class="p">):</span>
        <span class="n">result</span> <span class="o">=</span> <span class="bp">self</span><span class="o">.</span><span class="n">value</span>
        <span class="bp">self</span><span class="o">.</span><span class="n">value</span> <span class="o">+=</span> <span class="mi">1</span>
        <span class="k">return</span> <span class="n">result</span>

<span class="k">def</span> <span class="nf">splice</span><span class="p">(</span><span class="n">depth</span><span class="p">,</span> <span class="n">item</span><span class="p">):</span>
    <span class="k">if</span> <span class="n">depth</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
        <span class="k">return</span> <span class="p">[</span><span class="n">item</span><span class="p">]</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="k">return</span> <span class="n">concat</span><span class="p">([</span><span class="n">splice</span><span class="p">(</span><span class="n">depth</span><span class="o">-</span><span class="mi">1</span><span class="p">,</span> <span class="n">subitem</span><span class="p">)</span> <span class="k">for</span> <span class="n">subitem</span> <span class="ow">in</span> <span class="n">item</span><span class="p">])</span>

<span class="k">def</span> <span class="nf">concat</span><span class="p">(</span><span class="n">lists</span><span class="p">):</span>
    <span class="k">return</span> <span class="p">[</span><span class="n">x</span> <span class="k">for</span> <span class="n">xs</span> <span class="ow">in</span> <span class="n">lists</span> <span class="k">for</span> <span class="n">x</span> <span class="ow">in</span> <span class="n">xs</span><span class="p">]</span>

<span class="k">def</span> <span class="nf">join</span><span class="p">(</span><span class="n">items</span><span class="p">,</span> <span class="n">delimiter</span><span class="o">=</span><span class="s2">&quot;&quot;</span><span class="p">):</span>
    <span class="k">return</span> <span class="n">delimiter</span><span class="o">.</span><span class="n">join</span><span class="p">(</span>
        <span class="n">join</span><span class="p">(</span><span class="n">item</span><span class="p">,</span> <span class="n">delimiter</span><span class="p">)</span> <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">item</span><span class="p">,</span> <span class="nb">list</span><span class="p">)</span> <span class="k">else</span> <span class="nb">str</span><span class="p">(</span><span class="n">item</span><span class="p">)</span>
        <span class="k">for</span> <span class="n">item</span> <span class="ow">in</span> <span class="n">items</span>
    <span class="p">)</span>

<span class="k">def</span> <span class="nf">indent</span><span class="p">(</span><span class="n">text</span><span class="p">,</span> <span class="n">prefix</span><span class="o">=</span><span class="s2">&quot;    &quot;</span><span class="p">):</span>
    <span class="k">return</span> <span class="s2">&quot;&quot;</span><span class="o">.</span><span class="n">join</span><span class="p">(</span><span class="n">prefix</span><span class="o">+</span><span class="n">line</span> <span class="k">for</span> <span class="n">line</span> <span class="ow">in</span> <span class="n">text</span><span class="o">.</span><span class="n">splitlines</span><span class="p">(</span><span class="kc">True</span><span class="p">))</span>

<span class="k">def</span> <span class="nf">compile_chain</span><span class="p">(</span><span class="n">grammars</span><span class="p">,</span> <span class="n">source</span><span class="p">):</span>
    <span class="kn">import</span> <span class="nn">sys</span>
    <span class="kn">import</span> <span class="nn">pprint</span>
    <span class="k">for</span> <span class="n">grammar</span><span class="p">,</span> <span class="n">rule</span> <span class="ow">in</span> <span class="n">grammars</span><span class="p">:</span>
        <span class="k">try</span><span class="p">:</span>
            <span class="n">source</span> <span class="o">=</span> <span class="n">grammar</span><span class="p">()</span><span class="o">.</span><span class="n">run</span><span class="p">(</span><span class="n">rule</span><span class="p">,</span> <span class="n">source</span><span class="p">)</span>
        <span class="k">except</span> <span class="n">MatchError</span> <span class="k">as</span> <span class="n">e</span><span class="p">:</span>
            <span class="n">MARKER</span> <span class="o">=</span> <span class="s2">&quot;</span><span class="se">\033</span><span class="s2">[0;31m&lt;ERROR POSITION&gt;</span><span class="se">\033</span><span class="s2">[0m&quot;</span>
            <span class="k">if</span> <span class="nb">isinstance</span><span class="p">(</span><span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">,</span> <span class="nb">str</span><span class="p">):</span>
                <span class="n">stream_string</span> <span class="o">=</span> <span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">[:</span><span class="n">e</span><span class="o">.</span><span class="n">pos</span><span class="p">]</span> <span class="o">+</span> <span class="n">MARKER</span> <span class="o">+</span> <span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">[</span><span class="n">e</span><span class="o">.</span><span class="n">pos</span><span class="p">:]</span>
            <span class="k">else</span><span class="p">:</span>
                <span class="n">stream_string</span> <span class="o">=</span> <span class="n">pprint</span><span class="o">.</span><span class="n">pformat</span><span class="p">(</span><span class="n">e</span><span class="o">.</span><span class="n">stream</span><span class="p">)</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">exit</span><span class="p">(</span><span class="s2">&quot;ERROR: </span><span class="si">{}</span><span class="se">\n</span><span class="s2">POSITION: </span><span class="si">{}</span><span class="se">\n</span><span class="s2">STREAM:</span><span class="se">\n</span><span class="si">{}</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span>
                <span class="n">e</span><span class="o">.</span><span class="n">message</span><span class="p">,</span>
                <span class="n">e</span><span class="o">.</span><span class="n">pos</span><span class="p">,</span>
                <span class="n">indent</span><span class="p">(</span><span class="n">stream_string</span><span class="p">)</span>
            <span class="p">))</span>
    <span class="k">return</span> <span class="n">source</span>
</pre></div>

### src/main.py

<div class="highlight"><pre><span></span><span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="kn">import</span> <span class="nn">sys</span>
    <span class="k">def</span> <span class="nf">read</span><span class="p">(</span><span class="n">path</span><span class="p">):</span>
        <span class="k">if</span> <span class="n">path</span> <span class="o">==</span> <span class="s2">&quot;-&quot;</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">sys</span><span class="o">.</span><span class="n">stdin</span><span class="o">.</span><span class="n">read</span><span class="p">()</span>
        <span class="k">with</span> <span class="nb">open</span><span class="p">(</span><span class="n">path</span><span class="p">)</span> <span class="k">as</span> <span class="n">f</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">f</span><span class="o">.</span><span class="n">read</span><span class="p">()</span>
    <span class="n">args</span> <span class="o">=</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:]</span> <span class="ow">or</span> <span class="p">[</span><span class="s2">&quot;--compile&quot;</span><span class="p">,</span> <span class="s2">&quot;-&quot;</span><span class="p">]</span>
    <span class="k">while</span> <span class="n">args</span><span class="p">:</span>
        <span class="n">command</span> <span class="o">=</span> <span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--support&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">SUPPORT</span><span class="p">)</span>
        <span class="k">elif</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--copy&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">read</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)))</span>
        <span class="k">elif</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--embed&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="s2">&quot;</span><span class="si">{}</span><span class="s2"> = </span><span class="si">{}</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span>
                <span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">),</span>
                <span class="nb">repr</span><span class="p">(</span><span class="n">read</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">)))</span>
            <span class="p">))</span>
        <span class="k">elif</span> <span class="n">command</span> <span class="o">==</span> <span class="s2">&quot;--compile&quot;</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">compile_chain</span><span class="p">(</span>
                <span class="p">[(</span><span class="n">Parser</span><span class="p">,</span> <span class="s2">&quot;file&quot;</span><span class="p">),</span> <span class="p">(</span><span class="n">CodeGenerator</span><span class="p">,</span> <span class="s2">&quot;asts&quot;</span><span class="p">),</span> <span class="p">(</span><span class="n">Assembler</span><span class="p">,</span> <span class="s2">&quot;asts&quot;</span><span class="p">)],</span>
                <span class="n">read</span><span class="p">(</span><span class="n">args</span><span class="o">.</span><span class="n">pop</span><span class="p">(</span><span class="mi">0</span><span class="p">))</span>
            <span class="p">))</span>
        <span class="k">else</span><span class="p">:</span>
            <span class="n">sys</span><span class="o">.</span><span class="n">exit</span><span class="p">(</span><span class="s2">&quot;ERROR: Unknown command &#39;</span><span class="si">{}</span><span class="s2">&#39;&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">command</span><span class="p">))</span>
</pre></div>

### make.py

<div class="highlight"><pre><span></span><span class="ch">#!/usr/bin/env python</span>

<span class="kn">import</span> <span class="nn">os</span>
<span class="kn">import</span> <span class="nn">subprocess</span>
<span class="kn">import</span> <span class="nn">sys</span>

<span class="k">def</span> <span class="nf">make_next_version</span><span class="p">():</span>
    <span class="n">final_compiler</span> <span class="o">=</span> <span class="n">meta_compile_rlmeta</span><span class="p">()</span>
    <span class="n">test</span><span class="p">(</span><span class="n">final_compiler</span><span class="p">)</span>
    <span class="n">mv</span><span class="p">(</span><span class="n">final_compiler</span><span class="p">,</span> <span class="s2">&quot;rlmeta.py&quot;</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">meta_compile_rlmeta</span><span class="p">():</span>
    <span class="n">compiler</span> <span class="o">=</span> <span class="s2">&quot;rlmeta.py&quot;</span>
    <span class="n">content</span> <span class="o">=</span> <span class="n">read</span><span class="p">(</span><span class="n">compiler</span><span class="p">)</span>
    <span class="k">for</span> <span class="n">i</span> <span class="ow">in</span> <span class="nb">range</span><span class="p">(</span><span class="mi">4</span><span class="p">):</span>
        <span class="n">next_compiler</span> <span class="o">=</span> <span class="s2">&quot;rlmeta</span><span class="si">{}</span><span class="s2">.py&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">i</span><span class="o">+</span><span class="mi">1</span><span class="p">)</span>
        <span class="n">next_content</span> <span class="o">=</span> <span class="n">compile_rlmeta</span><span class="p">(</span><span class="n">compiler</span><span class="p">)</span>
        <span class="n">log</span><span class="p">(</span><span class="s2">&quot;Writing </span><span class="si">{}</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">next_compiler</span><span class="p">))</span>
        <span class="n">write</span><span class="p">(</span><span class="n">next_compiler</span><span class="p">,</span> <span class="n">next_content</span><span class="p">)</span>
        <span class="k">if</span> <span class="n">next_content</span> <span class="o">==</span> <span class="n">content</span><span class="p">:</span>
            <span class="k">return</span> <span class="n">next_compiler</span>
        <span class="n">compiler</span> <span class="o">=</span> <span class="n">next_compiler</span>
        <span class="n">content</span> <span class="o">=</span> <span class="n">next_content</span>
    <span class="n">fail</span><span class="p">(</span><span class="s2">&quot;Unable to produce metacompiler.&quot;</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">compile_rlmeta</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">):</span>
    <span class="n">log</span><span class="p">(</span><span class="s2">&quot;Compiling rlmeta using </span><span class="si">{}</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">))</span>
    <span class="k">return</span> <span class="n">run_rlmeta</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">,</span> <span class="p">[</span>
        <span class="s2">&quot;--embed&quot;</span><span class="p">,</span> <span class="s2">&quot;SUPPORT&quot;</span><span class="p">,</span> <span class="s2">&quot;src/support.py&quot;</span><span class="p">,</span>
        <span class="s2">&quot;--support&quot;</span><span class="p">,</span>
        <span class="s2">&quot;--compile&quot;</span><span class="p">,</span> <span class="s2">&quot;src/parser.rlmeta&quot;</span><span class="p">,</span>
        <span class="s2">&quot;--compile&quot;</span><span class="p">,</span> <span class="s2">&quot;src/codegenerator.rlmeta&quot;</span><span class="p">,</span>
        <span class="s2">&quot;--compile&quot;</span><span class="p">,</span> <span class="s2">&quot;src/assembler.rlmeta&quot;</span><span class="p">,</span>
        <span class="s2">&quot;--copy&quot;</span><span class="p">,</span> <span class="s2">&quot;src/main.py&quot;</span><span class="p">,</span>
    <span class="p">])</span>

<span class="k">def</span> <span class="nf">test</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">):</span>
    <span class="n">log</span><span class="p">(</span><span class="s2">&quot;Test: Has its own support library&quot;</span><span class="p">)</span>
    <span class="k">assert</span> <span class="n">run_rlmeta</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">,</span> <span class="p">[</span><span class="s2">&quot;--support&quot;</span><span class="p">])</span> <span class="o">==</span> <span class="n">read</span><span class="p">(</span><span class="s2">&quot;src/support.py&quot;</span><span class="p">)</span>
    <span class="n">log</span><span class="p">(</span><span class="s2">&quot;Test: Disallow semantic action in the middle&quot;</span><span class="p">)</span>
    <span class="n">run_rlmeta</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">,</span> <span class="p">[],</span> <span class="sa">b</span><span class="s2">&quot;Grammar { x = . -&gt; [] . }&quot;</span><span class="p">,</span> <span class="n">expect_failure</span><span class="o">=</span><span class="kc">True</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">run_rlmeta</span><span class="p">(</span><span class="n">rlmeta</span><span class="p">,</span> <span class="n">args</span><span class="p">,</span> <span class="n">stdin</span><span class="o">=</span><span class="sa">b</span><span class="s2">&quot;&quot;</span><span class="p">,</span> <span class="n">expect_failure</span><span class="o">=</span><span class="kc">False</span><span class="p">):</span>
    <span class="n">process</span> <span class="o">=</span> <span class="n">subprocess</span><span class="o">.</span><span class="n">Popen</span><span class="p">(</span>
        <span class="p">[</span><span class="s2">&quot;python&quot;</span><span class="p">,</span> <span class="n">rlmeta</span><span class="p">]</span><span class="o">+</span><span class="n">args</span><span class="p">,</span>
        <span class="n">stdin</span><span class="o">=</span><span class="n">subprocess</span><span class="o">.</span><span class="n">PIPE</span><span class="p">,</span>
        <span class="n">stdout</span><span class="o">=</span><span class="n">subprocess</span><span class="o">.</span><span class="n">PIPE</span>
    <span class="p">)</span>
    <span class="n">stdout</span><span class="p">,</span> <span class="n">_</span> <span class="o">=</span> <span class="n">process</span><span class="o">.</span><span class="n">communicate</span><span class="p">(</span><span class="n">stdin</span><span class="p">)</span>
    <span class="k">if</span> <span class="n">expect_failure</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">process</span><span class="o">.</span><span class="n">returncode</span> <span class="o">==</span> <span class="mi">0</span><span class="p">:</span>
            <span class="n">fail</span><span class="p">(</span><span class="s2">&quot;Expected failure&quot;</span><span class="p">)</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="k">if</span> <span class="n">process</span><span class="o">.</span><span class="n">returncode</span> <span class="o">!=</span> <span class="mi">0</span><span class="p">:</span>
            <span class="n">fail</span><span class="p">(</span><span class="s2">&quot;Expected success&quot;</span><span class="p">)</span>
    <span class="k">return</span> <span class="n">stdout</span>

<span class="k">def</span> <span class="nf">mv</span><span class="p">(</span><span class="n">src</span><span class="p">,</span> <span class="n">dest</span><span class="p">):</span>
    <span class="n">log</span><span class="p">(</span><span class="s2">&quot;Moving </span><span class="si">{}</span><span class="s2"> -&gt; </span><span class="si">{}</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">src</span><span class="p">,</span> <span class="n">dest</span><span class="p">))</span>
    <span class="n">os</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">dest</span><span class="p">)</span>
    <span class="n">os</span><span class="o">.</span><span class="n">rename</span><span class="p">(</span><span class="n">src</span><span class="p">,</span> <span class="n">dest</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">cleanup</span><span class="p">():</span>
    <span class="k">for</span> <span class="n">path</span> <span class="ow">in</span> <span class="p">[</span>
        <span class="s2">&quot;rlmeta1.py&quot;</span><span class="p">,</span>
        <span class="s2">&quot;rlmeta2.py&quot;</span><span class="p">,</span>
        <span class="s2">&quot;rlmeta3.py&quot;</span><span class="p">,</span>
        <span class="s2">&quot;rlmeta4.py&quot;</span><span class="p">,</span>
    <span class="p">]:</span>
        <span class="k">if</span> <span class="n">os</span><span class="o">.</span><span class="n">path</span><span class="o">.</span><span class="n">exists</span><span class="p">(</span><span class="n">path</span><span class="p">):</span>
            <span class="n">log</span><span class="p">(</span><span class="s2">&quot;Deleting </span><span class="si">{}</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">path</span><span class="p">))</span>
            <span class="n">os</span><span class="o">.</span><span class="n">remove</span><span class="p">(</span><span class="n">path</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">read</span><span class="p">(</span><span class="n">path</span><span class="p">):</span>
    <span class="k">with</span> <span class="nb">open</span><span class="p">(</span><span class="n">path</span><span class="p">,</span> <span class="s2">&quot;rb&quot;</span><span class="p">)</span> <span class="k">as</span> <span class="n">f</span><span class="p">:</span>
        <span class="k">return</span> <span class="n">f</span><span class="o">.</span><span class="n">read</span><span class="p">()</span>

<span class="k">def</span> <span class="nf">write</span><span class="p">(</span><span class="n">path</span><span class="p">,</span> <span class="n">content</span><span class="p">):</span>
    <span class="k">with</span> <span class="nb">open</span><span class="p">(</span><span class="n">path</span><span class="p">,</span> <span class="s2">&quot;wb&quot;</span><span class="p">)</span> <span class="k">as</span> <span class="n">f</span><span class="p">:</span>
        <span class="k">return</span> <span class="n">f</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">content</span><span class="p">)</span>

<span class="k">def</span> <span class="nf">log</span><span class="p">(</span><span class="n">message</span><span class="p">):</span>
    <span class="n">sys</span><span class="o">.</span><span class="n">stderr</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="s2">&quot;</span><span class="se">\033</span><span class="s2">[0;33m</span><span class="si">{}</span><span class="se">\033</span><span class="s2">[0m</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">message</span><span class="p">))</span>

<span class="k">def</span> <span class="nf">success</span><span class="p">(</span><span class="n">message</span><span class="p">):</span>
    <span class="n">sys</span><span class="o">.</span><span class="n">stderr</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="s2">&quot;</span><span class="se">\033</span><span class="s2">[0;32m</span><span class="si">{}</span><span class="se">\033</span><span class="s2">[0m</span><span class="se">\n</span><span class="s2">&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">message</span><span class="p">))</span>

<span class="k">def</span> <span class="nf">fail</span><span class="p">(</span><span class="n">message</span><span class="p">):</span>
    <span class="n">sys</span><span class="o">.</span><span class="n">exit</span><span class="p">(</span><span class="s2">&quot;</span><span class="se">\033</span><span class="s2">[0;31mERROR: </span><span class="si">{}</span><span class="se">\033</span><span class="s2">[0m&quot;</span><span class="o">.</span><span class="n">format</span><span class="p">(</span><span class="n">message</span><span class="p">))</span>

<span class="k">if</span> <span class="vm">__name__</span> <span class="o">==</span> <span class="s2">&quot;__main__&quot;</span><span class="p">:</span>
    <span class="n">cleanup</span><span class="p">()</span>
    <span class="k">if</span> <span class="n">sys</span><span class="o">.</span><span class="n">argv</span><span class="p">[</span><span class="mi">1</span><span class="p">:]</span> <span class="o">==</span> <span class="p">[</span><span class="s2">&quot;--compile&quot;</span><span class="p">]:</span>
        <span class="n">sys</span><span class="o">.</span><span class="n">stdout</span><span class="o">.</span><span class="n">buffer</span><span class="o">.</span><span class="n">write</span><span class="p">(</span><span class="n">compile_rlmeta</span><span class="p">(</span><span class="s2">&quot;rlmeta.py&quot;</span><span class="p">))</span>
    <span class="k">else</span><span class="p">:</span>
        <span class="n">make_next_version</span><span class="p">()</span>
    <span class="n">cleanup</span><span class="p">()</span>
    <span class="n">success</span><span class="p">(</span><span class="s2">&quot;  O-----------------O&quot;</span><span class="p">)</span>
    <span class="n">success</span><span class="p">(</span><span class="s2">&quot;  | RLMeta compiled |&quot;</span><span class="p">)</span>
    <span class="n">success</span><span class="p">(</span><span class="s2">&quot;~~|     itself!     |&quot;</span><span class="p">)</span>
    <span class="n">success</span><span class="p">(</span><span class="s2">&quot;  O-----------------O&quot;</span><span class="p">)</span>
</pre></div>
