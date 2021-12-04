---
title: 'DRAFT: RLMeta Poster 2'
date: 2021-12-04
tags: rlmeta,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

A while ago I created a [poster](/writing/creating-rlmeta-poster/index.html) to
showcase RLMeta. The version of RLMeta on the poster is based on the version
from the [memoizing failures](/writing/rlmeta-memoize-failures/index.html)
article, but I made it smaller and more beautiful to better suit a poster. To
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
more practical. Compared to the poster version, this versions could also be
more easily improved because the rendering of the blog post is automatic
compared to the rendering of the poster which is a lot of manual work every
time the source code changes. I also wanted to experiment with the walk through
format because I thought it might be something that is worth putting into the
README of a project.

The rest of this blog post consists of the walk through of the new version of
RLMeta and a section on the most important changes from the poster version and
motivations for them.

## Getting the source code

In order to follow along on this walk through, you need this version of RLMeta
which can be download here: [rlmeta-poster-2.zip](rlmeta-poster-2.zip).

## Structure

The zip file consists of the source code, a make script, and the RLMeta
compiler itself (`rlmeta.py`):

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

Looking at the source code, this is it:

<div class="highlight"><pre><span></span>$ <span></span>wc -l src/*
<span></span>   39 src/assembler.rlmeta
   57 src/codegenerator.rlmeta
   26 src/main.py
   60 src/parser.rlmeta
  237 src/support.py
  419 total
</pre></div>

The RLMeta compiler can be created from the source code.

## Exploring RLMeta

Before we dive into how the RLMeta compiler is created, let's explore what it
can do. The RLMeta compiler takes various inputs and outputs Python code.

The most fundamental way it does that is with the `--compile` option that
specifies a grammar file:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --compile &lt;<span class="o">(</span><span class="nb">echo</span> <span class="s1">&#39;Foo { foo = .  }&#39;</span><span class="o">)</span>
<span></span><span class="k">class</span> <span class="nc">Foo</span><span class="p">(</span><span class="n">Grammar</span><span class="p">):</span>
    <span class="n">rules</span> <span class="o">=</span> <span class="p">{</span>
        <span class="s1">&#39;foo&#39;</span><span class="p">:</span> <span class="mi">0</span>
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

This is the same as piping a grammar into the compiler with no arguments:

<div class="highlight"><pre><span></span>$ <span></span><span class="nb">echo</span> <span class="s1">&#39;Foo { foo = . }&#39;</span> <span class="p">|</span> python rlmeta.py
<span></span><span class="k">class</span> <span class="nc">Foo</span><span class="p">(</span><span class="n">Grammar</span><span class="p">):</span>
    <span class="n">rules</span> <span class="o">=</span> <span class="p">{</span>
        <span class="s1">&#39;foo&#39;</span><span class="p">:</span> <span class="mi">0</span>
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

The generated Python code depends on a support library. The `--support` option
can be used to generate that library:

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

Next, the compiler has a `--embed` options which takes a name and a file. It
will generated a Python variable assignment where the first argument is the
name of the variable and the contents of the file is the string value of the
variable:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --embed FOO &lt;<span class="o">(</span><span class="nb">echo</span> hello<span class="o">)</span>
<span></span><span class="n">FOO</span> <span class="o">=</span> <span class="s1">&#39;hello</span><span class="se">\n</span><span class="s1">&#39;</span>
</pre></div>

And finally, it has an option to do verbatim copy of a file:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --copy &lt;<span class="o">(</span><span class="nb">echo</span> <span class="s1">&#39;print(&quot;hello&quot;)&#39;</span><span class="o">)</span>
<span></span><span class="nb">print</span><span class="p">(</span><span class="s2">&quot;hello&quot;</span><span class="p">)</span>
</pre></div>

## Do a meta-compilation

Now that we have an understanding of the RLMeta compiler, let's see how we
can create it from the source code. Here is the full command:

<div class="highlight"><pre><span></span>$ <span></span>python rlmeta.py --embed SUPPORT src/support.py --support --compile src/parser.rlmeta --compile src/codegenerator.rlmeta --compile src/assembler.rlmeta --copy src/main.py &gt; rlmeta-raw.py
<span></span>
</pre></div>

Let's break it down:

* First we invoke the compiler: `python rlmeta.py`.
* The first argument `--embed SUPPORT src/support.py` tells the compiler to
  generate a Python variable named `SUPPORT` containing the contents of the
  file `src/suppor.py`.
* The next argument `--support` tells the compiler to generate the support
  library that is embedded in the compiler.
* The `--compile ...` arguments tell the compiler to compile the given grammar
  files.
* The last argument, `--copy src/main.py` tells the compiler to copy the file
  verbatim.

The make script can be called with the `compile` argument to perform this exact
function:

<div class="highlight"><pre><span></span>$ <span></span>./make.py compile &gt; rlmeta-compile.py
<span></span>
</pre></div>

And all these files are exactly the same:

<div class="highlight"><pre><span></span>$ <span></span>md5sum rlmeta.py rlmeta-compile.py rlmeta-raw.py
<span></span>8f438ec43dc93d0297415c7ddbcc683c  rlmeta.py
8f438ec43dc93d0297415c7ddbcc683c  rlmeta-compile.py
8f438ec43dc93d0297415c7ddbcc683c  rlmeta-raw.py
</pre></div>

Thus, the RLMeta compiler reproduced itself exactly from the source code.

## The usage of the make script

## Follow transformation of a simple program

## Code

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

--

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
