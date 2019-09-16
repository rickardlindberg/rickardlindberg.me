---
title: Ideas
---

This page records random ideas that interest me.

---

Learn about [Earley parsing](https://en.wikipedia.org/wiki/Earley_parser).

---

Develop a prototype of the code collaboration platform that I discussed in [A
new home for Timeline](/writing/new-home-for-timeline/index.html).

---

Host this website on my own server. Re-build website whenever a commit is made.
Also keep RLiterate book up to date.

---

Implement minimal OOP-language in the same fashion as Lua.

---

The goal of a free software community should be to maximize the number of
contributors. It is first about community, second about the product.

---

Implement the object model in [Open Reusable Object
Models](http://www.vpri.org/pdf/tr2006003a_objmod.pdf) to understand it better.

---

Read the book [Mindstorms: Children, Computers, And Powerful
Ideas](https://www.amazon.com/Mindstorms-Children-Computers-Powerful-Ideas/dp/0465046746?keywords=mindstorm+papert&qid=1538158112&sr=8-1-spell&ref=mp_s_a_1_1).

---

Interesting blog: [Eli Bendersky's website](https://eli.thegreenplace.net/)
([The Expression Problem in Go](https://eli.thegreenplace.net/2018/the-expression-problem-in-go/)).

---

Convert rlselect to an RLiterate document and publish it on this blog.

---

Convert my dotfiles to an RLiterate document and publish it on this blog.

---

Write more articles in the [RLMeta](/tags/rlmeta/index.html) series:

* Connecting "Learn how to implement languages" and RLMeta: 6 years
  later I did what I suggested.

* Porting RLMeta to JavaScript.

* Automatic syntax highlighting for RLMeta grammars.

* Bootstrapping with [Ohm](https://ohmlang.github.io/).

* Implement a template language that can be used to replace compile scripts.
  Draft:

```
import sys

#: python -c 'import sys; sys.stdout.write("SUPPORT = "+repr(sys.stdin.read()))' < support.py

#: cat support.py

#: python "$rlmeta_compiler" < parser.rlmeta

#: python "$rlmeta_compiler" < codegenerator.rlmeta

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(compile_grammar(sys.stdin.read()))
        except _MatchError as e:
            sys.stderr.write(e.describe())
            sys.exit(1)
```

---

How to design new languages?

Implement the solution to a problem manually in "assembly language", then
figure out a higher level syntax that can translate down to it.

---

All in one'ness: https://www.youtube.com/watch?v=Uv1UfLPK7_Q

Document and program is in the same file.

If someone sends you a document, you can just "run" it. No need to know what
program it was written in.

What programs work like this?

* Smalltalk images
* TiddlyWiki

"Supercomputers" are everywhere nowadays.

There is no technical reason why I can't "run" and all in one'ness document on
my smartphone.

But everything is (vendor)locked in.

What is the smallest virtual machine that must be ported to different devices
to allow all in one'ness documents everywhere?

---

Is linear text a bad way to organize information?

Is a wiki a better tool for organizing information?

An article (linear text) presents one story. It conveys certain information.
But that information can probably be conveyed in a different linear way. Is
there a non-linear representation that is useful? Or must information always be
presented to other humans in forms of a linear story?

---

Read the literate program: [A Retargetable C Compiler: Design and Implementation](https://www.amazon.com/-/dp/0805316701/).

---

Implement a regular expression engine as described in [Regular Expression Matching: the Virtual Machine Approach](https://swtch.com/~rsc/regexp/regexp2.html).

---

QuickCheck method:

1. think about unit test cases
2. write one single property
3. label the distribution according to unit test cases
4. enforce distribution percentages

Bonus:

1. Model a thing with a simpler thing (dictionary with a list for example)

---

Erlang / FP / OOP:

Concurrent system must use multiple computers. Multiple computers must
communicate with message passing.

Why not program "local" system with that model as well?

Like many small computers (or objects) communicating with eachother by message
passing.

How are messages represented in Erlang? Does a message send work exactly the
same no matter if the receiver is on the same computer or not?

---

SICP modeling data with functions:

```scheme
(define (pair x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1"))))
  dispatch)

(define (frst p) (p 0))

(define (snd p) (p 1))
```

`pair` is an object that responds to messages ('0' and '1').

In this setup it is impossible to get the individual coordinates without
sending a message. True encapsulation of state.

---

Implement "a" LISP.

Target a byte code machine? Target x86?

---

Interesting website: [Future of Coding](https://futureofcoding.org/).

---

Interesting website: [Conal Elliott](http://conal.net/). (FRP author?)

---

Let the computer do what it is good at. Simulation? When I program, I want help
with that. Visualize and simulate my program to see if it matches my
understanding. And my brain can do what it is good at. Problem solving.

---

Build tool to archive files. Files are imported into one big pile. A search
facility can query the pile. Metadata is associated with each file. One field
might be original path where it was imported from. Another its name and
modification times.

---

We write software as we produce plastic: we don't think about how to recycle
it.

---

One rendering tree. In the browser, you can not put a graph anywhere in the
DOM. You need a canvas, and then that is another rendering tree. Having one
rendering tree is more powerful.

---

If you need do draw on the whiteboard to explain your system, you are lacking a
visualisation.

---

Interesting article: [Baby's First Garbage Collector](http://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/).

---

On writing well:

* Choose a mode/tense/etc.. and stick with it.
* How (in what capacity) to address the reader?
* What do I want to say? Have I said it?
* One point: reductions time/place. What to skip?
* Think clearly => write clearly. Every sentence should build on the previous.
  Logical steps forward.
* Don't end with summary.

---

Interesting blogs:

* https://danluu.com/
* https://prog21.dadgum.com/
* https://hillelwayne.com/
