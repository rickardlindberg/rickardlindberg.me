---
title: RLMeta - Exploring ideas from the sixties
date: 2017-11-18
---

In this article I explain how I implemented a domain specific language for
writing compilers. First I show what it looks like and then I show in detail
how I implemented it.

## Background

I got the inspiration from another domain specific language for writing
compilers called META II. It came out in the 1960s. META II is interesting
because not only is it a useful language for writing compilers, but the
language is also implemented in itself.  So implementing such a language
involves bootstrapping.  I wanted to understand how META II worked, and did it
by implementing a similar language and writing an article explaining how I did
it. Because a lack of inspiration, I named it RLMeta as in Rickard Lindberg's
Meta language.

## Design goals

I had two design goals for RLMeta:

1. The whole implementation should fit in a single file and be small.

2. The language should be practically useful.

Those two are a bit in conflict. If the implementation should be small, some
useful features might have to be left out. But since RLMeta is a metacompiler
written in itself, and is small, it is fairly easy to develop it further.

So a bonus goal was that RLMeta might should be able to serve as a base for a
compiler writing language that comes with sensible defaults out of the box.

## Examples

I will now show what RLMeta looks like and what it can be used for.

A compiler is something that takes a source program on stdin and writes a
target program on stdout:

  cat src | ./compiler

Show some example compilers written in RLMeta to demonstrate how it works.

:include examples.rlmetatest

## Implementation RLMeta

Show the definition of META:

:include rlmeta/rlmeta.rlmeta

Explain that rlmeta 0 is written in itself.

## Bootstrap process

### RLMeta0

First version:

    rlmeta0.rlmeta (rlmeta0 written in itself)
    rlmeta0.py (hand coded)

Rough steps:

* Draft rlmeta0.rlmeta on paper
* Type it digitally
* Sketched template for rlmeta0.py
* Translated one rule at a time
* Realized that grammar was wrong
* Fixed grammar/compiler back and forth until I had a metacompiler

### Discuss limitation on RLMeta0

- whitespace should be handled in grammar
- repetitions: ? + *
- assign match to names instead of using %
- get rid of regex matching

### Improving RLMeta0

Show how to incrementally produce new metacompilers with new features.

1. Describe feature to implement
2. Present source language diff
3. Present bootsrap diagram

What feature to choose?

1. Try implementing it
2. See what is missing
3. Implement that
4. Go back

## Alternative bootstrap process

Implement in other language. Via Ohm?

## More examples

What can we do with RLMeta?

* "Binary" output for compilation to AST
* DSL for writing tests of RLMeta programs: RLMetaTest

## Ideas

* Prelude that includes space, empty, identifier
* Binary helpers (A, B, S) for ASTs?

## Limitations

Because RLMeta wants to be small and elegant, some features are not
implemented:

* ..
* ..

## Lessons learned

Compiling something other than itself is useful. I got a metacompiler that did
not generate runtime. So it worked for the meta case, but not for a compiler
that did not include verbatim runtime. Discovered during development of
rlmetatest.

## Resources

RLMeta was my second attempt at implementing a language like META II. I
failed one time earlier because I couldn't fully understand all the conecpts.

I'm grateful to the following resources that helped me understand how META II
worked:

* Wikipedia article: https://en.wikipedia.org/wiki/META_II
* Original paper: http://www.hcs64.com/files/pd1-3-schorre.pdf
* Metacompiler tutorial: http://www.bayfronttechnologies.com/mc_tutorial.html
* https://github.com/melvinzhang/meta2-lua
* OMeta implementations

Further reading:

* Ohm
    * https://news.ycombinator.com/item?id=15491336
