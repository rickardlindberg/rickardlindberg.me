---
title: RLMeta - A domain specific language for writing compilers
date: 2017-11-18
---

This article explains how I implemented a domain specific language for writing
compilers. First I show what it looks like and then I show in detail how I
implemented it.

## Background

The inspiration comes from another domain specific language for writing
compilers called META II. I wanted to understand how META II worked, and did it
by implementing a similar language and writing an article explaining how I did
it.

## Design goals

Small and elegant yet practical.

## Examples

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

## More examples

What can we do with RLMeta?

* "Binary" output for compilation to AST
* DSL for writing tests of RLMeta programs: RLMetaTest

## Limitations

Because RLMeta wants to be small and elegant, some features are not
implemented:

* ..
* ..

## Resources

The following resources helped me understand META II:

* Wikipedia article: https://en.wikipedia.org/wiki/META_II
* Original paper: http://www.hcs64.com/files/pd1-3-schorre.pdf
* http://www.bayfronttechnologies.com/mc_tutorial.html
* https://github.com/melvinzhang/meta2-lua

Further reading:

* Ohm
    * https://news.ycombinator.com/item?id=15491336
