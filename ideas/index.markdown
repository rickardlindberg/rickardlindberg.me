---
title: Ideas
---

This page contains ideas that I think would be interesting to work on or think
about in the future.

* * *

Convert microservice draft post to [IPython][ipython] notebook.

* * *

A follow up to [Data structures in
OOP](/writing/reflections-on-programming/2012-06-23-data-structures-in-oop/index.html).

Explore designs for an object oriented program that works with data of the
following kind:

* There are events
* Events can belong to a category
* Multiple events can belong to the same category
* There can not be two categories with the same name
* There can be two events with the same text

Areas of interest:

* Modification: what is the interface for modifying these relationships in an
  OOP way?

* How can we save snapshot of this structure for easy undo? (Object copy with
  copy.deepcopy?)

* How do we move entities between different sets?

* * *

Host [Timeline][timeline] on our own machines or host it distributed?

[SourceForge](https://sourceforge.net/) seems to have [some
disadvantages](https://notepad-plus-plus.org/news/notepad-plus-plus-leaves-sf.html).
Probably all hosted platforms will have similar problems sooner or later. So
maybe distributed is the way to go?

Can everything live in the repo? Bug tracker? Discussions? Can we fire up a
distributed web app that act as SF or Github?

* * *

Implement a compiler for
[FORTH](https://en.wikipedia.org/wiki/Forth_%28programming_language%29).

All I know is that it is assembler-like and stack based.

It would be a fun activity that will possibly teach me something about
programming paradigms.

* * *

Learn [Docker](https://www.docker.com/) by setting up an environment for
running [IPython][ipython].

* * *

Learn [Meteor](https://www.meteor.com/) by implementing a chat system.

Maybe try to use it for [Timeline][timeline] live discussions.

* * *

Extract timeline canvas component from [Timeline][timeline]. Define it in a
domain specific language so that it can be compiled to run on Android as well.

* * *

Play with [IPython][ipython] to do science on the
[Timeline][timeline] repo.

- [Ruby Rogues episode that inspired me](http://devchat.tv/ruby-rogues/184-rr-what-we-actually-know-about-software-development-and-why-we-believe-it-s-true-with-greg-wilson-and-andreas-stefik)
- [Example notebook](http://nbviewer.ipython.org/github/tarmstrong/code-analysis/blob/master/IPythonReviewTime.ipynb)
- [Wikipedia: Scientific method](http://en.wikipedia.org/wiki/Scientific_method)

[timeline]: http://thetimelineproj.sourceforge.net/
[ipython]: http://ipython.org/
