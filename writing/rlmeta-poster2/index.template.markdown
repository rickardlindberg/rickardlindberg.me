A while ago I created a [poster](/writing/creating-rlmeta-poster/index.html) to
showcase RLMeta. To be able to finish the poster, I had to stop coding on
RLMeta and put the source code on a poster. That was difficult because I felt
the need for it to be perfect. Eventually I did stop polishing, and left a few
items unresolved.

Almost immediately after I finished the poster, I started work on a second
version. Initially, my plan was to make a second version of the poster. I
started to fix the unresolved items and I was making progress. But somehow
imperfections kept creeping in. It felt like a never ending game of chasing
perfection. That's when I decided that a second poster would probably not be
worth it. But I still liked the new version of RLMeta.

I decided to attempt to present the new version of RLMeta in the style of a
code walk through. In other words, another way to showcase RLMeta that is also
a bit more practical. Compared to the poster version, this versions could also
be more easily improved because the rendering of the blog post is automatic
compared to the rendering of the poster which is a lot of manual work every
time the source code changes. I also wanted to experiment with the walk through
format because I think it might be something worth putting into the README of a
project.

The remaining of this blog post consists of the walk through of the new
version of RLMeta and then a section on the most important changes from the
poster version and motivations for them.

## Getting the source code

In order to follow along on this walk through, you need this version of RLMeta
which can be download here: [rlmeta-poster-2.zip](rlmeta-poster-2.zip).

## Structure

The zip file consists of the source code, a make script, and the RLMeta
compiler itself (`rlmeta.py`):

$:shell:rlmeta-poster-2:tree --dirsfirst

Looking at the source code, this is it:

$:shell:rlmeta-poster-2:wc -l src/*

The RLMeta compiler can be created from the source code.

## Exploring RLMeta

Before we dive into how the RLMeta compiler is created, let's explore what it
can do. The RLMeta compiler takes various inputs and outputs Python code.

The most fundamental way it does that is with the `--compile` option that
specifies a grammar file:

$:shell:rlmeta-poster-2:python rlmeta.py --compile <(echo 'Foo { foo = . }')

This is the same as piping a grammar into the compiler with no arguments:

$:shell:rlmeta-poster-2:echo 'Foo { foo = . }' | python rlmeta.py

The generated Python code depends on a support library. The `--support` option
can be used to generate that library:

$:shell:rlmeta-poster-2:python rlmeta.py --support | grep '^\(class\|def\)'

Next, the compiler has a `--embed` options which takes a name and a file. It
will generated a Python variable assignment where the first argument is the
name of the variable and the contents of the file is the string value of the
variable:

$:shell:rlmeta-poster-2:python rlmeta.py --embed FOO <(echo hello)

And finally, it has an option to do verbatim copy of a file:

$:shell:rlmeta-poster-2:python rlmeta.py --copy <(echo 'print("hello")')

## Do a meta-compilation

Now that we have an understanding of the RLMeta compiler, let's see how we
can create it from the source code. Here is the full command:

$:shell:rlmeta-poster-2:python rlmeta.py --embed SUPPORT src/support.py --support --compile src/parser.rlmeta --compile src/codegenerator.rlmeta --compile src/assembler.rlmeta --copy src/main.py > rlmeta-raw.py

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

$:shell:rlmeta-poster-2:./make.py compile > rlmeta-compile.py

And all these files are exactly the same:

$:shell:rlmeta-poster-2:md5sum rlmeta.py rlmeta-compile.py rlmeta-raw.py

Thus, the RLMeta compiler reproduced itself exactly from the source code.

## The usage of the make script

## Follow transformation of a simple program

## Code

$:code:rlmeta-poster-2/src/parser.rlmeta

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
