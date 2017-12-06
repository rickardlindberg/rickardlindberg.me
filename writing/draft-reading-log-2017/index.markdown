---
title: Programming thoughts 2017
date: 2017-12-31
---

Here is a summary of book and articles that I read during 2017.

This is more a log of programming ideas I thought about. And stuff I consumed
in relation to that.

## ??

* [Postfix??]()

## October

* [Programming Clojure]()

## November

* [Writing An Interpreter In Go]()

* [Effective Programs - 10 Years of Clojure - Rich Hickey](https://www.youtube.com/watch?v=2V1FtfBDsLU)

* [Spec-ulation Keynote - Rich Hickey](https://www.youtube.com/watch?v=oyLBGkS5ICk)

* [ArrrrCamp 2014- Unicorn Unix Magic Tricks ](https://www.youtube.com/watch?v=DGhlQomeqKc)

* Implement languages
  * Expression language
  * A version of APL
  * Smalltalk is good at producing the next version of itself
    * Meta programming
    * Making itself obsolete
    * ...
    * Squek: bootstrapping mechanism for something much better than Smalltalk
      * How do I obsolete it?

* [Dan Ingalls: Object-Oriented Programming](https://www.youtube.com/watch?v=P2mh92d-T3Y&t=851s)
  This was a simple yet detailed description of how Smalltak was implemented. I
  got inspired to try to implement a small object oriented language in this way
  to learn more about them. Two ideas I had about such language. First: can it use
  postfix notation so that "object message1 message2" is interpreted like
  "(object message1) message2"? Inspired by the syntax of
  [Factor](https://en.wikipedia.org/wiki/Factor_(programming_language)).
  Second: can such a language be a base for embedding, similar to Lua?

## December

* [Is it really "Complex"? Or did we just make it "Complicated"?](https://www.youtube.com/watch?v=ubaX1Smg6pY)
  I watched a similar talk by Alan many years ago. I got the same feeling of
  agreement this time that we should absolutely design new languages to fit the
  problem. Solving a programming problem should be first finding/designing the
  perfect language to express that problem in, and then make that language run.
  I want to experiment with this idea for Timeline. By how much can we shrink
  code size by implementing Timeline in many languages (that all still compile
  down to Python/wxPython code maybe) that are designed for the problems that
  Timeline solve.
