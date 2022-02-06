---
title: August 2021 Update
date: 2021-09-07
tags: newsletter
---

This is what I've been up to in August 2021:

* I continued with the draft post [RLMeta poster
  2](/writing/rlmeta-poster-2/index.html).

  * I had some ideas of things I wanted to improve in the next version of
    RLMeta. But mainly, I just wanted to work on it again because I think it is
    a fun piece of software to work on.

  * Before I could continue to improve RLMeta, I first had to port it to Python
    3 because my new laptop now runs Python 3. The port was rather small.

  * I then continued with various improvements that I plan to document in the
    blog post.

  * I'm not sure that the final product of the blog post is going to be a
    poster. Right now I'm thinking it should be some kind of interactive, live,
    demo that you can play with in your browser that illustrates, somewhat
    similar to the poster, how RLMeta works. That would also require a port of
    RLMeta to Javascript, which should be fun.

  * I find it difficult to know when to stop improving RLMeta. Sometimes I feel
    like I chase perfection. In every iteration, I get closer, but there seems
    to always be something left that annoys me.

  * My next big goal with RLMeta is to make it not depend on Python. In order
    for that to happen, I need to implement a subset of the Python language
    that RLMeta uses. All the way down to machine code. So it is a quite
    big project, but should be fun.

* I thought about where it makes sense to put CI/CD code. The most common place
  to put it in (that I've seen) is in the repository itself. But I feel like
  that is not the most logical place. Information about how to use a project in
  a CI/CD chain should not be coupled with the project itself. I feel like
  build scripts belong in the repository because how to build a piece of
  software feels related to the project itself. But in CI/CD, you often deal
  with larger aspects than the project in isolation. For example, in a CI/CD
  chain you might want to send email notifications upon build failure. That is
  not related to the project. It is related to how you implement CI/CD. So I'm
  thinking that a CI/CD system should be programmed separately from the project
  repos.

* I got inspired by [this video](https://www.youtube.com/watch?v=bpbYyF-FBqU)
  by Van Neistat about a tool he can't live well without: the pencil. I could
  relate to that as I also feel that the pencil is "the pump that brings up the
  ideas from the well", so I decided to buy a T-shirt:

  <center><img src="pencil.jpg"></center>
