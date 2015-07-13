---
title: Ideas
---

This page contains ideas that I think would be interesting to work on or think
about. I collect them here so that I can come back to them when I have time and
motivation.

* * *

Use Discourse for Timeline discussions?

* * *

Being afraid to change code is a sad thing.

Is breaking things better that being afraid to change?

Can we refactor modules internally and then create compatibility APIs that we
deprecate and later remove. For example, in Python, we can create a class
alias:

    class NewNameOfClass(object):
        ...

    OldNameOfClass = NewNameOfClass

* * *

How to get more people involved in the development of Timeline?

There are plenty of things you con do without having to do programming. I'm
thinking the most useful thin you can contribute is your domain expertise. How
do you use Timeline? What problems are you facing? Help propose solutions to
problems and help test them out and give feedback.

How can we create a community of Timeline people working together to build this
piece of software. Pull-request model focuses much on code. But how can we best
collaborate about the non-programming things?

* * *

Improve my IPython Notebook workflow for this website:

- Make export more automatic
- Fix CSS

* * *

Implement textual similarity algorithm. How similar are two lines of text?

* * *

Make science page on my homepage where I collect and learn.

http://users.encs.concordia.ca/~pcr/paper/Rahman2015IEEES-preprint.pdf

* * *

Can we write test cases using IPython Notebook? Would it serve as good
documentation as well?

[https://github.com/ipython/ipython/wiki/Cookbook:-Notebook-utilities](https://github.com/ipython/ipython/wiki/Cookbook:-Notebook-utilities)

* * *

Build code reader tool that aids the reading of source code. What features
should it have?

Highlights [last copy paste
row](http://www.st.ewi.tudelft.nl/~mbeller/publications/2015_beller_zaidman_karpov_the_last_line_effect_preprint.pdf),
shows all files that are modified together.

Get some inspiration from scientific papers:
[http://software-carpentry.org/blog/2015/05/icse2015.html](http://software-carpentry.org/blog/2015/05/icse2015.html)

What aids reading code?

- Syntax highlighting?
- Navigation?
- Highlight piece of text?

How to do experiments?

Build tool where these features can be toggled and then run experiments to see
which aids reading. For example: ask a group of people to answer a question
about a piece of code using and not using syntax highlighting.

**What is the simplest first version of this tool?**

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

Completed ideas
---------------

Jenkins machine that builds [Timeline][timeline] documentation does not seem to
[handle](https://jenkins.rickardlindberg.me/job/timeline-doc/242/console) code
directives in restructured text:

Why?

*Completed 22 June 2015 by upgrading the docutils python package.*

* * *

Play with [IPython][ipython] to do science on the [Timeline][timeline] repo.

- [Ruby Rogues episode that inspired me](http://devchat.tv/ruby-rogues/184-rr-what-we-actually-know-about-software-development-and-why-we-believe-it-s-true-with-greg-wilson-and-andreas-stefik)
- [Example notebook](http://nbviewer.ipython.org/github/tarmstrong/code-analysis/blob/master/IPythonReviewTime.ipynb)
- [Wikipedia: Scientific method](http://en.wikipedia.org/wiki/Scientific_method)

*Completed 21 June 2015 by publishing first analysis article
[here](/writing/analysis-timeline-emails/index.html).*



[timeline]: http://thetimelineproj.sourceforge.net/
[ipython]: http://ipython.org/
