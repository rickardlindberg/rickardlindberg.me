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

Different coordinate systems are suited to different problems. Choosing the
right coordinate system makes the problem easier to solve. The same should be
true for programming problems. Each problem probably has an ideal programming
language in which it can be solved. The task for a programmer should be to find
that language, implement it, and solve the problem in that language. Try to
find and document examples of this hypothesis.

The ideal language is found if no accidental complexity is present when the
problem is solved in that language. That will also make the problem be
expressed in few lines of code. So a crude metric of good software is its size.
Or rather its size for the functionality it gives.

---

The goal of a free software community should be to maximize the number of
contributors. It is first about community, second about the product.

---

Explore late bound systems. What does late binding mean?

---

Explore architecture of
[B5000](https://en.wikipedia.org/wiki/Burroughs_large_systems) ([emulator
project](https://github.com/pkimpel/retro-b5500)). How is it related to
[Nothing](https://github.com/alexwarth/nothing)?

---

Implement the object model in [Open Reusable Object
Models](http://www.vpri.org/pdf/tr2006003a_objmod.pdf) to understand it better.

---

Read the book [Mindstorms: Children, Computers, And Powerful
Ideas](https://www.amazon.com/Mindstorms-Children-Computers-Powerful-Ideas/dp/0465046746?keywords=mindstorm+papert&qid=1538158112&sr=8-1-spell&ref=mp_s_a_1_1).

---

The STEPS project:

* [STEPS Toward The Reinvention of Programming](http://www.vpri.org/pdf/tr2007008_steps.pdf) (First Year Progress Report, December 2007.)

* [STEPS Toward The Reinvention of Programming, 2008 Progress Report Submitted to the National Science Foundation (NSF), October 2008](http://www.vpri.org/pdf/tr2008004_steps08.pdf)

* [STEPS Toward The Reinvention of Programming, 2009 Progress Report Submitted to the National Science Foundation (NSF) October 2009](http://www.vpri.org/pdf/tr2009016_steps09.pdf)

* [STEPS Toward Expressive Programming Systems, 2010 Progress Report Submitted to the National Science Foundation (NSF) October 2010](http://www.vpri.org/pdf/tr2010004_steps10.pdf)

* [Steps Toward Expressive Programming Systems](http://www.vpri.org/pdf/tr2011004_steps11.pdf) (Annual report to the NSF)

* [STEPS Toward the Reinvention of Programming, 2012 Final Report Submitted to the National Science Foundation (NSF) October 2012](http://www.vpri.org/pdf/tr2012001_steps.pdf)

Related:

* Nice summary blog post: [Towards Moore's Law Software: Part 3 of 3](https://www.moserware.com/2008/04/towards-moores-law-software-part-3-of-3.html).

* Read the blog post [Computer Revolution](http://blog.rtens.org/computer-revolution.html).

---

Interesting blog: [Eli Bendersky's website](https://eli.thegreenplace.net/)
([The Expression Problem in Go](https://eli.thegreenplace.net/2018/the-expression-problem-in-go/)).

---

Convert rlselect to an RLiterate document and publish it on this blog.

---

Convert my dotfiles to an RLiterate document and publish it on this blog.

---

Write more articles in the [RLMeta](/tags/rlmeta/index.html) series:

* Optimizing RLMeta.

* Associativity and precedence. A good resource:
  [http://beastie.cs.ua.edu/proglan/readings/precedence.pdf](http://beastie.cs.ua.edu/proglan/readings/precedence.pdf).

* Connecting "Learn how to implement languages" and RLMeta: 6 years
  later I did what I suggested.

* Porting RLMeta to JavaScript.

* Automatic syntax highlighting for RLMeta grammars.

* Bootstrapping with [Ohm](https://ohmlang.github.io/).

* Demonstrate how a feature is added to RLMeta and the need for compiling
  itself in steps. Link to that post from within the RLMeta post.

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

* Implement call rule in the matching string to improve performance of
  ast-traverse.

* How to implement off-side rule? (Python INDENT/DEDENT.)

```
def foo():
    for i in range(
5):
        print(str(i))

foo()
```

* Implement PEG with VM as described in [A Text Pattern-Matching Tool based on Parsing Expression Grammars](http://www.inf.puc-rio.br/%7Eroberto/docs/peg.pdf).

---

Direct manipulation.
[WYSIWYG](https://www.quora.com/What-exactly-is-WYSIWYG/answer/Alan-Kay-11):

> In programming, you don’t want to go through a edit in an editor, submit to a
> compiler, which submits to a loader, which requires your system to
> intiialize, and so forth. You just want to be able to deal directly and
> safely with what you are trying to achieve.

PDF by Lary Tesler about modes: [A Personal History of Modeless Text Editing
and Cut/Copy-Paste ](http://delivery.acm.org/10.1145/2220000/2212896/p70-tesler.pdf).

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

Interesting posts by Alan Kay:

* [Ask HN: Relationship between OO and functional programming?](https://news.ycombinator.com/item?id=11808551)
* [Alan Kay has agreed to do an AMA today](https://news.ycombinator.com/item?id=11939851)

---

[The Early History Of Smalltalk - Bret Victor](http://worrydream.com/EarlyHistoryOfSmalltalk/)

Quotes:

> Somewhere in all of this, I realized that the bridge to an object-based
> system could be in terms of each object as a syntax directed interpreter of
> messages sent to it. In one fell swoop this would unify object-oriented
> semantics with the ideal of a completely extensible language. The mental
> image was one of separate computers sending requests to other computers that
> had to be accepted and understood by the receivers before anything could
> happen. In today's terms every object would be a server offering services
> whose deployment and discretion depended entirely on the server's notion of
> relationship with the servee.

> Again, the whole point of OOP is not to have to worry about what is inside an
> object. Objects made on different machines and with different languages
> should be able to talk to each other—and will have to in the future.
> Late-binding here involves trapping incompatibilities into recompatibility
> methods—a good discussion of some of the issues is found in [Popek 1984].

> Staying with the metaphor of late-binding, what further late-binding schemes
> might we expect to see? One of the nicest late-binding schemes that is being
> experimented with is the metaobject protocol work at Xerox PARC [Kiczales
> 1991]. The notion is that the language designer's choice for the internal
> representation of instances, variables, etc., may not cover what the
> implementer needs, so within a fixed semantics they allow the implementer to
> give the system strategies—for example, using a hashed lookup for slots in an
> instance instead of direct indexing. These are then efficiently compiled and
> extend the base implementation of the system.

---

Read [The Reactive Engine](http://www.chilton-computing.org.uk/inf/pdfs/kay.htm).

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

[prototypes vs classes was: Re: Sun's HotSpot](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html):

> Just a gentle reminder that I took some pains at the last OOPSLA to try to
> remind everyone that Smalltalk is not only NOT its syntax or the class
> library, it is not even about classes. I'm sorry that I long ago coined the
> term "objects" for this topic because it gets many people to focus on the
> lesser idea.
>
> The big idea is "messaging" -- that is what the kernal of Smalltalk/Squeak
> is all about (and it's something that was never quite completed in our
> Xerox PARC phase).

---

Interesting quotes from [Dr. Alan Kay on the Meaning of “Object-Oriented Programming”](http://www.purl.org/stefan_ram/pub/doc_kay_oop_en):

> I thought of objects being like biological cells and/or individual computers
> on a network, only able to communicate with messages (so messaging came at
> the very beginning -- it took a while to see how to do messaging in a
> programming language efficiently enough to be useful).

> I wanted to get rid of data. The B5000 almost did this via its almost
> unbelievable HW architecture. I realized that the cell/whole-computer
> metaphor would get rid of data, and that "<-" would be just another message
> token (it took me quite a while to think this out because I really thought of
> all these symbols as names for functions and procedures.

> The original Smalltalk at Xerox PARC came out of the above. The subsequent
> Smalltalk's are complained about in the end of the History chapter: they
> backslid towards Simula and did not replace the extension mechanisms with
> safer ones that were anywhere near as useful.

> OOP to me means only messaging, local retention and protection and
> hiding of state-process, and extreme late-binding of all things. It
> can be done in Smalltalk and in LISP. There are possibly other
> systems in which this is possible, but I'm not aware of them.

Interesting quotes from [Hacker News thread](https://news.ycombinator.com/item?id=19415983):

> Containers are a validation of Kay's idea that sharing encapsulated objects
> is easier than sharing data.

> Because a lot of people (including me) have a Simula based view of "object
> oriented", we tend to think of objects as data structures with functions
> attached to them. Alan Kay had a different view, as far as I can tell. He
> viewed objects as being a collection of abilities. You could invoke these
> abilities by sending the object a "message". How you send that message is
> irrelevant. The important thing is that the object is not a collection of
> data, but rather the object contains the program state necessary to provide
> the ability (and nothing more). One of the things he talks about (I can't
> remember if he does in this specific email exchange, though) is the idea that
> once the data is inside the object, you can't actually access it any more. It
> becomes a detail that the programmer doesn't have to worry about.

> Smalltalk had to cut corners with messaging due to the limited processing of
> the time, nevertheless it has fully reified messages; one can express the
> sending of messages between objects.
>
> Smalltalk inspired Erlang, which is the fully-asynchronous
> messaging/independent threads of execution part of OO only.
>
> Self did OO without inheritance (composition by prototypes).

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

Interesting project: [https://github.com/d-cook/SomethingNew](https://github.com/d-cook/SomethingNew).

---

One rendering tree. In the browser, you can not put a graph anywhere in the
DOM. You need a canvas, and then that is another rendering tree. Having one
rendering tree is more powerful.

---

If you need do draw on the whiteboard to explain your system, you are lacking a
visualisation.
