---
title: April 2022 Update
date: 2022-05-01
tags: newsletter
---

This is what I've been up to in April 2022:

* I continued working on
  [RLCI](https://github.com/rickardlindberg/rlci/). In particular I
  started working on the server component and wrote it using
  [asyncio](https://docs.python.org/3/library/asyncio.html). So far,
  that feels like a good choice.

* I read [Refactoring
  databases](https://www.martinfowler.com/books/refactoringDatabases.html)
  and started reading [NoSQL
  Distilled](https://www.martinfowler.com/books/nosql.html). The
  reason I picked up the first one was that I sought to understand how
  you can do zero-downtime deployments and still evolve your database.
  That is, how can both an old version of an application and a new
  version of an application work with the same database? The answer
  was that you must evolve your database schema such that it is
  compatible with both versions. So if you move a column for example,
  you can create a trigger that make sure that both the old and the
  new column are in sync. And when a transition period is over, you
  can drop the old column, and the move column refactoring is
  complete. I picked up the second book to find if the answer was any
  different for NoSQL databases which generally are a bit more
  flexible when it comes to database schema. My guess is that a
  similar approach is needed. So far the book has given me some new
  ideas about what databases are for and how different databases
  serve different purposes. This will all be useful in going forward
  with a storage mechanism for RCLI.

* I published another five programming videos on my Youtube channel:

    * [How to find time to work on your hobby projects?](https://youtu.be/lqV5c_fN29Q)
    * [How I got unstuck with RLCI.](https://youtu.be/aXkGdvKZu1A)
    * [Learning Python asyncio when working on CI server.](https://youtu.be/GO-sfUaqbvg)
    * [What makes programming videos interesting?](https://youtu.be/ihS0HagBfKY)
    * [Refactoring a class to test in isolation: REAL WORLD EXAMPLE](https://youtu.be/uV2eeAcrxzU)
