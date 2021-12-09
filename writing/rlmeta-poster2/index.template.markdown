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

## Getting RLMeta

In order to follow along on this walk through, you need to download this
version of RLMeta from here: [rlmeta-poster-2.zip](rlmeta-poster-2.zip).

## File structure

The zip file consists of the source code, a make script, and the RLMeta
compiler:

$:shell:rlmeta-poster-2:tree --dirsfirst

The size of the source code is quite small:

$:shell:rlmeta-poster-2:wc -l src/*

The RLMeta compiler can be created from this source code only. We will see how
later in this walk through.

## Exploring the RLMeta compiler

Before we dive into how the RLMeta compiler is created, let's explore what it
can do.

The main function of the compiler is to transform grammars into to Python code.
It does that with the `--compile` option which specifies a grammar file to
transform:

$~shell~rlmeta-poster-2~echo -e 'Example {\n  main = .\n}' > example.grammar

$:shell:rlmeta-poster-2:cat example.grammar:rlmeta

$:shell:rlmeta-poster-2:python rlmeta.py --compile example.grammar:python

The same function can be achieved by piping a grammar into its stdin:

$:shell:rlmeta-poster-2:cat example.grammar | python rlmeta.py:python

When the compiler is invoked without arguments, the `--compile` option is
assumed with a value of `-` which stands for stdin.

Don't worry about understanding the generated code now. We will explore it more
later.

The generated Python code for a grammar depends on a support library. The
`--support` option can be used to generate that library:

$:shell:rlmeta-poster-2:python rlmeta.py --support | grep '^\(class\|def\)':python

Next, the compiler has an `--embed` option which takes a name and a
filename. It is used to generate a Python variable assignment where the name is
the name of the variable and the value is the contents of the file:

$:shell:rlmeta-poster-2:python rlmeta.py --embed FOO example.grammar:python

And finally, the compiler has an option to do verbatim copy of files with the
`--copy` option:

$:shell:rlmeta-poster-2:python rlmeta.py --copy example.grammar:rlmeta

$~shell~rlmeta-poster-2~rm example.grammar

## Writing a small program in RLMeta

What types of programs can we write using RLMeta?

In RLMeta, we write grammars. Grammars have rules that specify how to match
objects from an input stream and specify what should happen when objects are
matched.

Let's write a grammar that counts the number of objects in an input stream and
produces a report:

$~shell~rlmeta-poster-2~echo -e 'ObjectCounter {\n  count = .*:xs -> { "number of objects = " len(xs) }\n}' > object_counter.grammar

$:shell:rlmeta-poster-2:cat object_counter.grammar:rlmeta

Compiling this grammar gives a Python class with the same name:

$:shell:rlmeta-poster-2:python rlmeta.py --compile object_counter.grammar | grep '^class':python

To be able to use this class, the support library must come before it so that
`Grammar`, for example, is defined:

$:shell:rlmeta-poster-2:python rlmeta.py --support --compile object_counter.grammar | grep '^class':python

To create a complete program, we also have to write a main function that
instantiates the `ObjectCounter` grammar and invokes its `count` rule.

Here is an example that passes stdin to the `count` rule and prints the result
to stdout:

$~shell~rlmeta-poster-2~echo -e 'import sys\nsys.stdout.write(ObjectCounter().run("count", sys.stdin.read()))' > object_counter_main.py

$:shell:rlmeta-poster-2:cat object_counter_main.py:python

Combining these pieces, we get this:

$:shell:rlmeta-poster-2:python rlmeta.py --support --compile object_counter.grammar --copy object_counter_main.py > object_counter.py

$:shell:rlmeta-poster-2:echo 'hello' | python object_counter.py

$:shell:rlmeta-poster-2:echo 'this is longer' | python object_counter.py

$~shell~rlmeta-poster-2~rm object_counter.grammar
$~shell~rlmeta-poster-2~rm object_counter_main.py
$~shell~rlmeta-poster-2~rm object_counter.py

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

$~shell~rlmeta-poster-2~rm rlmeta-compile.py
$~shell~rlmeta-poster-2~rm rlmeta-raw.py

## The usage of the make script

## Code

$:code:rlmeta-poster-2/src/parser.rlmeta

$:code:rlmeta-poster-2/src/codegenerator.rlmeta

$:code:rlmeta-poster-2/src/assembler.rlmeta

$:code:rlmeta-poster-2/src/support.py

$:code:rlmeta-poster-2/src/main.py

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
