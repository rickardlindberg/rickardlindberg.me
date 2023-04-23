---
title: Thinking about test setup
date: 2023-04-23
tags: agdpp,draft
agdpp: true
---

In the [previous](/writing/agdpp-shooting-arrow/index.html) episode we were not
quite happy with our test suite. Are we testing things at the right level? Do
we see any smells? Are we missing tests? We will take some time in this episode
to reflect on those questions so that things will go smooth, testing wise, when
working on the next story.

## A concrete problem

$:output:python:
...
$:END

## TODO

* In the last episode I had a feeling that everything was not alright with the
  tests. I poked around a bit and noticed that I could delete some vital
  production code that would break the game without my tests noticing.

* The promise of TDD and automated testing is that you have very high test
  coverage. What I aspire to is to create a test suite that within seconds
  tells me if I broke the production code. It might not be realistic, but when
  I find a case where I could break my production code without my tests
  noticing, I want to look more closely.

* One reason to not test is that you don't know how. That can be solved by
  practicing. Another reason might be that you decide that the cost is not
  worth it. However, the cost of testing goes down the better you get at it.

* Anyway, I'm getting slightly off topic. Where were we?

* Test failure! Let's look at the example.

    def tick(self, dt, events):
        for event in events:
            ...
            elif event.is_keydown_space():
                self.arrow.shoot()
        ...

    def tick(self, dt, events):
        for event in events:
            ...
            self.arrow.shoot()
        ...

  This makes the arrow shoot right when you start the game, and no test is
  failing.

* What test is missing? Well, I generally prefer that tests for a class should
  test behavior of that class. Since the if-statement resides in the
  BalloonShooter class, that's who's missing a test.

* I think the reason for the missing test is that I wanted to test this
  behavior at a lower level. And part of it is, but not the input handling.

* In the beginning, it makes sense to write tests only for the top level
  application class. That frees us up to make refactorings without needing to
  change tests. After a while certain subsystems might emerge and stories start
  to cover that subsystem only.

* What tests are left to write after ATDD?

* Forgot to test inital case? Just one frame, and not animated?

* Extracted tests too early.

## Summary

...

See you in the next episode!
