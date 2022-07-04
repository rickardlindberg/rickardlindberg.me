---
title: June 2022 Update
date: 2022-07-04
tags: newsletter
---

This is what I've been up to in June 2022:

* I started over with [RLCI](https://github.com/rickardlindberg/rlci/) and
  tried to build it using an agile approach. The README has a bunch of more
  information about it. In this process, the following resources helped me:

    * [Testing Without Mocks: A Pattern Language](https://www.jamesshore.com/v2/blog/2018/testing-without-mocks)
      Again from James Shore. Gave me insight into how to design and test.

    * [You Don't Hate Mocks; You Hate Side-Effects](https://blog.thecodewhisperer.com/permalink/you-dont-hate-mocks-you-hate-side-effects)
      The following quote explained it well to me the design relationship
      between pure functions and functions with side effects:

      > We always have the option of replacing a side-effect with returning a
      > value that represents the effect, as long as we can instruct the clients
      > to interpret that value as a command to execute.

    * [Favor real dependencies for unit testing](https://stackoverflow.blog/2022/01/03/favor-real-dependencies-for-unit-testing/)
      This helped me solve a testing problem I had. More on that in an upcoming
      video.

* I discovered that one of the authors of *Continuous Delivery*, Dave Farley,
  has a [Youtube channel](https://www.youtube.com/c/ContinuousDelivery). I
  found some of his videos interesting.

* I published another four programming videos on my [Youtube
  channel](https://www.youtube.com/channel/UC4XI09URnsM_YYTSizAMliA):

  * [How James Shore ruined my RLCI project.](https://youtu.be/yKkGXn1zmaI)
  * [Rebooting RLCI with an agile approach using TDD and zero friction development.](https://youtu.be/Re7litDdulU)
  * [Adding continuous integration functionality to RLCI.](https://youtu.be/sokSvnAkd5E)
  * [Making RLCI pipelines run in isolation.](https://youtu.be/0jJEPgomRCc)
