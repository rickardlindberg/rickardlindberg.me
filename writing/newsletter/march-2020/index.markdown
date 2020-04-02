---
title: Newsletter March 2020
date: 2020-04-02
tags: newsletter
---

This is what I've been up to in March 2020:

* I continued with [RLiterate](/projects/rliterate/book2/index.html):

    * I tried [Typometer](https://pavelfatin.com/typometer/) to measure the
      time it takes from a kepress to a character appearing on the screen.

    * My first approach to speed up drawing was to draw immediately with
      `Layout`/`Update`. That turned out to not work so well. I documented the
      problem in a [blog post](/writing/wx-layout-update/index.html).

    * I concluded that to draw fast in RLiterate, an approach that works quite
      well is to only draw the part where the cursor is first, and draw
      everything else a few milliseconds later if there are no additional input
      events. For example, if a title is edited in the workspace, the table of
      contents might have to be redrawn as well. But it can wait, since the
      focus is not on it right now. I documented this behavior quite well
      [here](/projects/rliterate/book2/index.html#5fb3048d154d4f50af92d4cbe63db75a).

    * I think RLiterate's true potential has not yet been explored. I think
      more paragraph types could add much value. It is something that I hope to
      explore after I finish RLiterate 2. But I don't want to rush it. I want
      to write RLiterate 2 as clean as possible while still being capable of
      roughly what RLiterate (1) is.

    * I'm also thinking that RLiterate must be a multi process program. There
      is only so much you can do in GUI thread without it being sluggish. And
      using threads in Python does not help much since they are not truly run
      in parallell. So I'm thinking a multi process worker architecture is
      needed for heavy tasks. Like syntax highlighting, spell checking,
      generating files, etc. Then a little worker icon in toolbar to let the
      user know that stuff is happening in the background.

* I read [The Flawed History of Graphical User
  Interfaces](https://medium.com/s/story/lets-pretend-this-never-happened-8abf0bc9648c).
  Computers don't have to be what they are. We can make them to do anything.
  Including alternative user interfaces.

* I read [Some tentative guidelines for GUI
  composability](https://hackernoon.com/some-tentative-guidelines-for-gui-composability-2900abead1d9).
  Isolated applications that can't be composed is a bad idea. Yet the default.

* I watched [Tudor Gîrba - Moldable
  development](https://www.youtube.com/watch?v=Pot9GnHFOVU&t=1026s). It is an
  interesting project that I want to explore more.

* I read [One rendering tree](https://medium.com/feenk/one-rendering-tree-918eae49bcff).

    > Well, the user interface framework has a tree, and somewhere in that tree,
    > there is a canvas, and in that canvas … well, there is another world, with
    > another tree. That these are different worlds, and different worlds means we
    > cannot combine them easily. We do not want different worlds. We want just
    > one. A seamless one.

    This is something that might be useful for RLiterate. But that would
    require yet another complete rewrite. I should finish RLiterate 2 and learn
    from it. Then study one rendering tree. Then see if a third rewrite is
    suitable or not. But it is far away in the future.

* I read [10 Most(ly dead) Influential Programming
  Languages](https://www.hillelwayne.com/post/influential-dead-languages/).  I
  would like to learn more APL. Perhaps implement my own APL to learn more.
