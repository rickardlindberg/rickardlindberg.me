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
