---
title: 'DRAFT: RLMeta Poster 2'
date: 2021-11-22
tags: rlmeta,draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

A while ago I created a [poster](/writing/creating-rlmeta-poster/index.html) to
showcase RLMeta. To be able to finish the poster, I had to stop polishing
RLMeta and put the source code on a poster. This was difficult because
I felt the need for it to be perfect.  Eventually I did stop polishing, but I
left a few items unresolved.

Recently (how many months later?), I picked up where I left off. Initially, my plan was to make a second
version of the poster. I started to fix the unresolved items and I was making
progress. But somehow imperfections kept creeping in. It felt like a never
ending game of chasing perfection. That's when I decided that a second poster
would probably not be worth it. But I still liked the new version of RLMeta.
What should I do?

I decided to attempt to present the new version of RLMeta in the style of a
code walk through.  In other words, another way to showcase RLMeta that is
hopefully a bit more practical. That is the remaining of this blog post. After
the walk through there will be also be a section on the most important changes
from the previous version and motivation for them.

Compared to poster version, it could also be more easily improved. Not having
to go to print.

Such a walk through might be a good way to structure a README for a project,
and it might serve as a start fo.

## Getting the source code

In order to follow along on this walk through, you need the source code of
RLMeta which can be download here: [rlmeta-poster-2.zip](rlmeta-poster-2.zip).

## Structure

    $ tree --dirsfirst
    .
    ├── src
    │   ├── assembler.rlmeta
    │   ├── codegenerator.rlmeta
    │   ├── main.py
    │   ├── parser.rlmeta
    │   └── support.py
    ├── make.py
    └── rlmeta.py
    
    1 directory, 7 files

Looking at the source code, this is it:

    $ wc -l src/*
       39 src/assembler.rlmeta
       57 src/codegenerator.rlmeta
       26 src/main.py
       60 src/parser.rlmeta
      237 src/support.py
      419 total

## Do a meta-compilation

    $ ./make.py compile > rlmeta-new.py

    $ md5sum rlmeta.py rlmeta-new.py
    8f438ec43dc93d0297415c7ddbcc683c  rlmeta.py
    8f438ec43dc93d0297415c7ddbcc683c  rlmeta-new.py

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
