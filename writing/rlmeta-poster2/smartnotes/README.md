CHANGES:

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

TODO:

[ ] Put compile + error reporting function in support lib.

[ ] Support recursive macros?

    * Probably requires function to run grammar against an object

[ ] You can put any crap at end of file, and parsers don't care. Fix it!