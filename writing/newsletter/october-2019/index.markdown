---
title: Newsletter October 2019
date: 2019-11-02
tags: newsletter
---

This is what I've been up to in October 2019:

* I read [Stack Computers: the new
  wave](http://users.ece.cmu.edu/~koopman/stack_computers/) by Philip J.
  Koopman, Jr. I did not dig into details about the different computers, but
  the rest of the book gave me a good understanding stack computers.

* I thought about how stack computers could support programming language
  features. In particular, I though about how they could support local
  variables. A calling convention could be used where function parameters are
  always put on the stack. If a function wants to, it can store those in a
  frame for later reference. But it does not have to. The compiler might also
  optimize away local variables in favor of pure stack operations.

* I continued working on [compiling expressions to x86 machine
  code](/writing/expr-to-x86-compiler/index.html). I did debugging with gdb and
  learned some useful commands. I managed to compile a subset of expressions
  down to x86 machine code. It was satisfying. I was helped by [Adventures in
  JIT compilation: Part 4 - in
  Python](https://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-4-in-python/).

* I got an urge to do a rewrite of [RLiterate](/projects/rliterate/index.html)
  to improve performance and fix bugs that make it annoying to use. I use
  RLiterate to write articles for my blog. I really like the features of it,
  but the poor performance makes it annoying to use. I had though of a slightly
  different architecture that would mitigate those problems. But it would
  require a complete rewrite. I have now successfully validated that the new
  architecture works better and I will continue to build the next version. You
  can see it [here](/projects/rliterate/book2/index.html).

* I worked on RLiterate in small chunks. First I did some work and published
  that version on my homepage. Then at a later time I did review of what I had
  done. Then I went through and fixed comments from review. This workflow has
  worked quite well. Having time between the two activities has allowed me to
  rest and reflect.

* Because I got distracted with RLiterate, I did not make progress on the
  article about [memoizing failures in
  RLMeta](/writing/rlmeta-memoize-failures/index.html) that I said I would
  hoped have finished by now. I have not abandoned my drafts though. Something
  more interesting got in between. I will get back to writing at some point.
