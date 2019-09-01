---
title: Newsletter August 2019
date: 2019-08-31
tags: newsletter
---

This is what I've been up to in August 2019:

* I finished the [RLMeta VM](/writing/rlmeta-vm/index.html) article. When
  working on it, I noticed that error handling has not gotten much love. I want
  to improve it and write about it in another article.

* I found [Build Your Own Text
  Editor](https://viewsourcecode.org/snaptoken/kilo/index.html) that explains
  how to build a text editor by showing and explaining incremental patches. I
  found the approach interesting.

    * This approach feels similar to what is described in [Literate
      programming: Knuth is doing it
      wrong](http://akkartik.name/post/literate-programming) and [A new way to
      organize programs](http://akkartik.name/post/wart-layers).

    * The above made me think about if programs could be organized in features
      without the use of literate programming. That would require some kind of
      plugin architechture where features could be plugged in independent of
      eachother.

* I read Guido van Rossum's article on [Generating a PEG
  Parser](https://medium.com/@gvanrossum_83706/generating-a-peg-parser-520057d642a9).
  This made me realize that RLMeta does not memoize correctly. I'm working on
  an [article](/writing/rlmeta-memoize-failures/index.html) where I fix it.

* I worked on porting [Timeline](/projects/timeline/index.html) to Python 3. It
  is a work in progress, but the application now starts. I have a few items
  left on my TODO list that I want to complete before considering the port
  done. I ran into a problem that I [blogged
  about](/writing/timeline-doctest-wxpython/index.html). An another one [in
  progress](/writing/timeline-segfault-wxpython/index.html).

* I found [this
  tweet](https://twitter.com/mfeathers/status/1159495761714769921) by Michael
  Feathers interesting:

    > Fred Brooks said that software is best when it looks like it was designed
    > by one mind.
    >
    > Mob programming is an attempt to create one mind of many.
    >
    > Quality goes down as the number of people touching code independently
    > goes up.
    >
    > Interesting constraint problem.

    And a [follow up](https://twitter.com/raganwald/status/1159498133635850240):

    > Interesting parallel to Alan Kay’s bifurcation of languages into
    > “Agglutination of features” vs. “Crystallization of style.”
    >
    > The former are almost always defined by committee, the latter by solo
    > designers or very small, tight teams.

    And another [follow up](https://twitter.com/jit_j/status/1159501352243998720):

    > There are quality OSS projects that are made up of individual
    > contributors and a distributed review process. It's possible for
    > independent workers to maintain and extend a codebase, as long as they
    > buy into a shared style and a shared philosophy. Process matters though.

    How can we build software so that it appears to be desgined by a single
    mind?

* I wrote some code for an [upcoming
  article](/writing/rlmeta-left-associativity/index.html) on parsing left
  associative operators in RLMeta.

* I read some of Eli Bendersky's articles on parsing:

    * [Recursive descent, LL and predictive parsers](https://eli.thegreenplace.net/2008/09/26/recursive-descent-ll-and-predictive-parsers)

    * [Some problems of recursive descent parsers](https://eli.thegreenplace.net/2009/03/14/some-problems-of-recursive-descent-parsers)

    * [A recursive descent parser with an infix expression evaluator](https://eli.thegreenplace.net/2009/03/20/a-recursive-descent-parser-with-an-infix-expression-evaluator)

    * [Top-Down operator precedence parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing)

    * [Parsing expressions by precedence climbing](https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing)

* One of them led me to read Bob Nystrom's article [Pratt Parsers: Expression
  Parsing Made
  Easy](http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/).

* I want to learn more about JIT compilation. As I read [Hello, JIT World: The
  Joy of Simple
  JITs](http://blog.reverberate.org/2012/12/hello-jit-world-joy-of-simple-jits.html)
  describing a JIT compiler for Brainfuck I thought: Is JIT-ing byte code just
  getting rid of the interpreter loop? I'm not completely sure of the big
  picture yet. And how does a tracing JIT compiler differ from a regular JIT
  compiler. Brainfuck seems to be a popular example in JIT articles.

* I read about Cython and though that I should try it for RLMeta VM. Should be
  quite easy to port it to C and make incremental performance improvements.
