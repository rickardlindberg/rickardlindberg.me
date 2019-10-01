---
title: Newsletter September 2019
date: 2019-10-01
tags: newsletter
---

This is what I've been up to in September 2019:

* I finished the article on [parsing left associative operators in
  RLMeta](/writing/rlmeta-left-associativity/index.html). I'm pleased with how
  it turned out.

* I played with [Cython](https://cython.org/) to see if RLMeta VM could be made
  faster if converted to a C extension module. (It could!) I might write a blog
  post about it.

* I learned more about Python C extension modules from [What are (c)python
  extension
  modules?](https://thomasnyberg.com/what_are_extension_modules.html).

* I watched [A Look at the Design of
  Lua](https://cacm.acm.org/magazines/2018/11/232214-a-look-at-the-design-of-lua/fulltext).
  I was inspired by the setting in which it seems to have been created. Small
  office. Whiteboard. Collaboration.  I want to implement a minimal
  OOP-language in order to learn how to do it.

* I finished porting [Timeline](/projects/timeline/index.html) to Python 3.

    * The most difficult part was to verify that division operations were still
      working the same. (Python 3 integer division gives float instead of
      integer as in Python 2.)

    * I concluded that maintaining software is hard work. Even if your program
      does not change, the external world changes, and you need to change your
      program accordingly.

    * I finished the article on the [segfault
      failure](/writing/timeline-segfault-wxpython/index.html) that I found
      during porting.

* I learned about a trick help me get started on a task faster: leave work
  undone. Leave a test failing. Leave a sentence half written.

* I learned that your number one priority is not what you write on top of the
  TODO list, but what you actually do.

* I finished a post with my [notes on Alan
  Kay](/writing/alan-kay-notes/index.html). He is a big inspiration to me.

* I read most parts of [Common Systems Programming Optimizations &
  Tricks](https://paulcavallaro.com/blog/common-systems-programming-optimizations-tricks/).
  Low level optimizations are interesting. It reminded me of Bob Nystrom's
  [game programming book](http://gameprogrammingpatterns.com/) that discusses
  some [similar
  topics](http://gameprogrammingpatterns.com/optimization-patterns.html).

* I continued with the article about [memoizing failures in
  RLMeta](/writing/rlmeta-memoize-failures/index.html). It is almost finished,
  but because I got distracted by other work, it is not. I hope to finish it
  next month.

* I started working on [compiling expressions to x86 machine
  code](/writing/expr-to-x86-compiler/index.html):

    * I don't really know assembly, so I found [A fundamental introduction to
      x86 assembly
      programming](https://www.nayuki.io/page/a-fundamental-introduction-to-x86-assembly-programming)
      useful.

    * I got segafults because of 32/64-bit mixup. It just shows that I don't
      really know what I'm doing yet.

    * I learned that `add` and `imul` instructions do not work the same. `imul`
      can not store result back in memory but must go via register.

    * I learned that I subtracted operands in the wrong order.

    * I read [Writing a basic x86-64 JIT compiler from scratch in stock
      Python](https://csl.name/post/python-jit/).

    * I read [JIT compiling a subset of Python to
      x86-64](https://csl.name/post/python-compiler/).
