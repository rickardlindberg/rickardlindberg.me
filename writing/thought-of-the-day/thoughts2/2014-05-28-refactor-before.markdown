---
title: Refactor before
---

I find it difficult to properly do the refactoring step in the TDD cycle. I
know the step is there, but I don't think hard enough about it, and continuing
to the next test is often more fun.

So when do I refactor? Refactoring works best for me right before a change. I
like to clean up the surrounding code before I make a change to it.

Doing the refactoring before ensures that I do it. It also gives me a goal:
Make the next change (that I've started to think about) easier to implement. I
don't think refactoring without a goal is bad. I think there are general things
you can improve that don't require a goal. But if all tests are passing, I know
that I just make small changes like improve a name or extract a method. With a
goal, for me, refactoring becomes more meaningful.

Perhaps this thinking can be applied to TDD as well. The cycle is red -> green
-> refactor. But, because it's a cycle, it repeats. So we could think of it as
refactor -> red -> green. It's almost the same, but mentally puts the
refactoring step first. If you peek at the upcoming test, you have a goal for
the current refactoring.

--

* [Comments on Google+](https://plus.google.com/112175093836850283531/posts/2zibBaK3hbh)
