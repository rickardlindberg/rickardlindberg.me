---
title: 'Alan Kay notes'
date: 2019-09-25
tags: alankay
---

Back in 2012 I watched a talk by Alan Kay called [Programming and
Scaling](https://youtu.be/YyIQKBzIuBY) in which he talks about how software
today has gotten so complex that we can't comprehend it. Ever since then I've
been interested in his ideas. In this post I summarize notes and quotes that
I've gathered while studying Kay and related topics. Fogus did a similar post
to this called [Soup](http://blog.fogus.me/2018/10/25/soup/).

## How to tackle complexity?

In the talk Kay hints at one way to tackle complexity which involves solving
problems in higher level languages. After listening to his talk and also
reading
[SICP](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)
in university, I had the following though about how to tackle complexity in
software:

Different coordinate systems are suited for different math problems. Choosing
the right coordinate system makes the problem easier to solve. The same should
be true for programming problems. Each problem probably has an ideal
programming language in which it can be solved. The task for a programmer
should be to find that language, implement it, and solve the problem in that
language.

The ideal language is found if no accidental complexity is present when the
problem is solved in that language. That will also make the problem be
expressed in few lines of code. So a crude metric of well written software is
its size.

## STEPS project

Then I found out about the STEPS project (from [VPRI](http://vpri.org/))
initiated by Kay in which they try to significantly reduce the number of lines
of code required to implement "personal computing".  They do it partly by
inventing new languages suited to the problems at hand.

The best overview of the STEPS project I think is the NSF proposal and the
progress reports:

* [Proposal to NSF – Granted on August 31st 2006](http://www.vpri.org/pdf/rn2006002_nsfprop.pdf) ([mirror](rn2006002_nsfprop.pdf))

* [STEPS Toward The Reinvention of Programming](http://www.vpri.org/pdf/tr2007008_steps.pdf) ([mirror](tr2007008_steps.pdf)) (First Year Progress Report, December 2007.)

* [STEPS Toward The Reinvention of Programming, 2008 Progress Report Submitted to the National Science Foundation (NSF), October 2008](http://www.vpri.org/pdf/tr2008004_steps08.pdf) ([mirror](tr2008004_steps08.pdf))

* [STEPS Toward The Reinvention of Programming, 2009 Progress Report Submitted to the National Science Foundation (NSF) October 2009](http://www.vpri.org/pdf/tr2009016_steps09.pdf) ([mirror](tr2009016_steps09.pdf))

* [STEPS Toward Expressive Programming Systems, 2010 Progress Report Submitted to the National Science Foundation (NSF) October 2010](http://www.vpri.org/pdf/tr2010004_steps10.pdf) ([mirror](tr2010004_steps10.pdf))

* [Steps Toward Expressive Programming Systems](http://www.vpri.org/pdf/tr2011004_steps11.pdf) ([mirror](tr2011004_steps11.pdf)) (Annual report to the NSF)

* [STEPS Toward the Reinvention of Programming, 2012 Final Report Submitted to the National Science Foundation (NSF) October 2012](http://www.vpri.org/pdf/tr2012001_steps.pdf) ([mirror](tr2012001_steps.pdf))

All their writings can be found [here](http://vpri.org/writings.php). Some
favorites of mine:

* [Experimenting With Programming Languages](http://www.vpri.org/pdf/tr2008003_experimenting.pdf) ([mirror](tr2008003_experimenting.pdf))
* [Programming and Programming Languages](http://www.vpri.org/pdf/rn2010001_programm.pdf) ([mirror](rn2010001_programm.pdf))
* [Open Reusable Object Models](http://www.vpri.org/pdf/tr2006003a_objmod.pdf) ([mirror](tr2006003a_objmod.pdf))
* [Active Essays on the Web](http://www.vpri.org/pdf/tr2009002_active_essays.pdf) ([mirror](tr2009002_active_essays.pdf))
* [Chains of meaning in the STEPS system](http://www.vpri.org/pdf/m2009011_chns_mng.pdf) ([mirror](m2009011_chns_mng.pdf))
* [Making Applications in KSWorld](http://www.vpri.org/pdf/m2013003_ksapps.pdf) ([mirror](m2013003_ksapps.pdf))
* [KScript and KSworld: A Time-Aware and Mostly Declarative Language and Interactive GUI Framework](http://www.vpri.org/pdf/tr2013002_KSonward.pdf) ([mirror](tr2013002_KSonward.pdf))

The STEPS project has many components. One of them is OMeta (described in
*Experimenting With Programming Languages*) which inspired me to implement
[RLMeta](/tags/rlmeta/index.html).

A blog post series mentioning the STEPS project and related ideas:

* [Towards Moore's Law Software: Part 1 of 3](https://www.moserware.com/2008/04/towards-moores-law-software-part-1-of-3.html)
* [Towards Moore's Law Software: Part 2 of 3](https://www.moserware.com/2008/04/towards-moores-law-software-part-2-of-3.html)
* [Towards Moore's Law Software: Part 3 of 3](https://www.moserware.com/2008/04/towards-moores-law-software-part-3-of-3.html)

## Object oriented programming

Kay invented the term object oriented. But I get the feeling that mainstream
object oriented programming today is not what he meant it to be. I tried to
figure out what he meant it to be. Here are some quotes that I found relevant:

From Kay from [The Early History Of Smalltalk - Bret Victor](http://worrydream.com/EarlyHistoryOfSmalltalk/):

> Somewhere in all of this, I realized that the bridge to an object-based
> system could be in terms of each object as a syntax directed interpreter of
> messages sent to it. In one fell swoop this would unify object-oriented
> semantics with the ideal of a completely extensible language. The mental
> image was one of separate computers sending requests to other computers that
> had to be accepted and understood by the receivers before anything could
> happen. In today's terms every object would be a server offering services
> whose deployment and discretion depended entirely on the server's notion of
> relationship with the servee.

From Kay from [The Early History Of Smalltalk - Bret Victor](http://worrydream.com/EarlyHistoryOfSmalltalk/):

> Again, the whole point of OOP is not to have to worry about what is inside an
> object. Objects made on different machines and with different languages
> should be able to talk to each other—and will have to in the future.
> Late-binding here involves trapping incompatibilities into recompatibility
> methods—a good discussion of some of the issues is found in [Popek 1984].

From Kay from [prototypes vs classes was: Re: Sun's HotSpot](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html):

> Just a gentle reminder that I took some pains at the last OOPSLA to try to
> remind everyone that Smalltalk is not only NOT its syntax or the class
> library, it is not even about classes. I'm sorry that I long ago coined the
> term "objects" for this topic because it gets many people to focus on the
> lesser idea.
>
> The big idea is "messaging" -- that is what the kernal of Smalltalk/Squeak
> is all about (and it's something that was never quite completed in our
> Xerox PARC phase).

From Kay from [Dr. Alan Kay on the Meaning of “Object-Oriented Programming”](http://www.purl.org/stefan_ram/pub/doc_kay_oop_en):

> I thought of objects being like biological cells and/or individual computers
> on a network, only able to communicate with messages (so messaging came at
> the very beginning -- it took a while to see how to do messaging in a
> programming language efficiently enough to be useful).

From Kay from [Dr. Alan Kay on the Meaning of “Object-Oriented Programming”](http://www.purl.org/stefan_ram/pub/doc_kay_oop_en):

> I wanted to get rid of data. The B5000 almost did this via its almost
> unbelievable HW architecture. I realized that the cell/whole-computer
> metaphor would get rid of data, and that "<-" would be just another message
> token (it took me quite a while to think this out because I really thought of
> all these symbols as names for functions and procedures.

From Kay from [Dr. Alan Kay on the Meaning of “Object-Oriented Programming”](http://www.purl.org/stefan_ram/pub/doc_kay_oop_en):

> The original Smalltalk at Xerox PARC came out of the above. The subsequent
> Smalltalk's are complained about in the end of the History chapter: they
> backslid towards Simula and did not replace the extension mechanisms with
> safer ones that were anywhere near as useful.

From Kay from [Dr. Alan Kay on the Meaning of “Object-Oriented Programming”](http://www.purl.org/stefan_ram/pub/doc_kay_oop_en):

> OOP to me means only messaging, local retention and protection and
> hiding of state-process, and extreme late-binding of all things. It
> can be done in Smalltalk and in LISP. There are possibly other
> systems in which this is possible, but I'm not aware of them.

From a user in the [Hacker News thread](https://news.ycombinator.com/item?id=19415983)
discussing the above article:

> Containers are a validation of Kay's idea that sharing encapsulated objects
> is easier than sharing data.

From a user in the [Hacker News thread](https://news.ycombinator.com/item?id=19415983)
discussing the above article:

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

From a user in the [Hacker News thread](https://news.ycombinator.com/item?id=19415983)
discussing the above article:

> Smalltalk had to cut corners with messaging due to the limited processing of
> the time, nevertheless it has fully reified messages; one can express the
> sending of messages between objects.
>
> Smalltalk inspired Erlang, which is the fully-asynchronous
> messaging/independent threads of execution part of OO only.
>
> Self did OO without inheritance (composition by prototypes).

## Late binding

Late-binding is another idea that Kay talks about that I want to understand
better.

From Kay from [The Early History Of Smalltalk - Bret Victor](http://worrydream.com/EarlyHistoryOfSmalltalk/):

> Staying with the metaphor of late-binding, what further late-binding schemes
> might we expect to see? One of the nicest late-binding schemes that is being
> experimented with is the metaobject protocol work at Xerox PARC [Kiczales
> 1991]. The notion is that the language designer's choice for the internal
> representation of instances, variables, etc., may not cover what the
> implementer needs, so within a fixed semantics they allow the implementer to
> give the system strategies—for example, using a hashed lookup for slots in an
> instance instead of direct indexing. These are then efficiently compiled and
> extend the base implementation of the system.

## Direct manipulation

From Kay from [What exactly is WYSIWYG?](https://www.quora.com/What-exactly-is-WYSIWYG/answer/Alan-Kay-11):

> In programming, you don’t want to go through a edit in an editor, submit to a
> compiler, which submits to a loader, which requires your system to
> intiialize, and so forth. You just want to be able to deal directly and
> safely with what you are trying to achieve.

A related idea about no modes is presented by Larry Tesler in [A Personal
History of Modeless Text Editing and
Cut/Copy-Paste](http://worrydream.com/refs/Tesler%20-%20A%20Personal%20History%20of%20Modeless%20Text%20Editing%20and%20Cut-Copy-Paste.pdf).

## Random topics

Other topics that I got from Kay:

* What vs. How Programming: State the problem, then let the computer solve the
  problem: what programming instead of how programming.
* Living in the Future: University students need powerful machines so that they
  can code like they live in the future. So that they can code without caring
  about optimizations.
* Most people can only see the future through the past.
* Learning to See:
    * Drawing without "parsing" the object.
    * Turn it upside down and just draw it like you see it.
    * Measure instead of perceive.
* Engineering Vs. Science Vs. Tinkering.
* T-shirt Programming.
* Meta Language.
* Computing History.
* Inventing Future.

## Further reading

* [Ask HN: Relationship between OO and functional programming?](https://news.ycombinator.com/item?id=11808551)
* [Alan Kay has agreed to do an AMA today](https://news.ycombinator.com/item?id=11939851)
* [The Reactive Engine](http://www.chilton-computing.org.uk/inf/pdfs/kay.htm)
* [Alan Kay search on Youtube](https://www.youtube.com/results?search_query=alan+kay)
* [SomethingNew](https://github.com/d-cook/SomethingNew)
