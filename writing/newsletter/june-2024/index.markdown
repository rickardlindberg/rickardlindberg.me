---
title: "Newsletter June 2024: Quines and Smalltalk"
date: 2024-07-02
tags: newsletter
---

I subscribe to [Avdi's newsletter](https://avdi.codes/newsletters/) because I
find some of his writing and links interesting. I was
clicking around on his site from one of the links in a newsletter and
rediscovered [Your Newsletter (A Pattern Language of Banana
Stands)](https://avdi.codes/your-newsletter/). This prompted me to try again to
create a proper newsletter from these posts, and this one will be the first
after a long break to also go out as en email.

## Quine Wiki

I came across [this toot](https://merveilles.town/@m15o/112592228824944602)
that says that the [Yon](https://m15o.ichi.city/yon/) wiki is now implemented
as a [quine](https://en.wikipedia.org/wiki/Quine_(computing)) meaning that when
you save a page in the wiki, it generates a new html file which is the new
wiki, complete with source code for the wiki itself.

If you want to edit pages, you can do it through the user interface, but if you
want to change other parts of the wiki, you have to to edit the source code
(html file with Javascript).

That got me thinking. What would it take to make the whole wiki system editable
from within itself? You would have to also embed the whole programming
environment in the interface. Something like what Smalltalk did.

## View Source Code

It also got be thinking of discoverable source code. If an application allowed
modifications of itself from within itself, then source code must be present.
"View source" in the browser was how I fist started learning programming. In
Smalltalk you can "view source" for anything (almost). Wouldn't that be a nicer
(programming) world to live in?

## Readable Code

That led me to think about how to write code that is also easy to read. And I
remembered [Build Your Own Text
Editor](https://viewsourcecode.org/snaptoken/kilo/) which is a sort of literate
programming approach to explaining how to build a text editor. One problem with
the literate programming approach is that is seems to take much more effort to
create such programs and maintain them. Is there another way to make programs
easier to understand?

One idea that came to mind was stripping features. Smaller programs are easier
to read and understand than bigger ones. What if we can remove all but the most
basic features? That would be a much smaller codebase to study. Then we can
gradually add features once we understand the base.

One approach that I thought of to do this in practice is if test cases are
somehow organized by feature. Then you can run the tests for the features that
you are interested in, notice what code is covered, and delete all code which
is not executed. Then you can study that.

Another thing that came to mind was how to organize code by features.

I might experiment with some of this for
[rlselect](/projects/rlselect/index.html). I would like for people to know how
this program works and be able to customize it to their needs. But I would like
the customizations to be local to all users by forking instead of growing the
code base for rlselect.  This might be similar to what Kartik is doing with
[freewheeling apps](https://akkartik.name/freewheeling/).
