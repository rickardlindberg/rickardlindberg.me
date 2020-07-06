---
title: Newsletter June 2020
date: 2020-07-06
tags: newsletter
---

This is what I've been up to in June 2020:

* I read parts of [Let's Build a Compiler, by Jack
  Crenshaw](https://compilers.iecc.com/crenshaw/). It was interesting and it
  would be fun to try to implement the described language using
  [RLMeta](/projects/rlmeta/index.html).

* I read a few articles from Julia Evans that I found interesting:

    * [Swapping, memory limits, and
      cgroups](https://jvns.ca/blog/2017/02/17/mystery-swap/)

    * [How much memory is my process
      using?](https://jvns.ca/blog/2016/12/03/how-much-memory-is-my-process-using-/)

    * [What even is a container: namespaces and
      cgroups](https://jvns.ca/blog/2016/10/10/what-even-is-a-container/)

* I decided to have a look at [RLiterate](/projects/rliterate/index.html)
  again.

    * I continued work on direct manipulation of text. Text in RLiterate is
      represented as a list of text segements with different styles. That
      structure has to be projected to the screen and edits have to go into the
      correct segement. Segments might be projected with some additional markup
      text that is not present in the text itself. So the problem is a little
      bit more involved than plain text editing. I'm struggling to solve this
      problem in an elegant way.

    * I have read about two so called projectional editors to get inspiration
      for how to solve these problems:
      [ProjecturEd](https://github.com/projectured/projectured/wiki/Overview)
      and
      [ProjectIt](https://projectit-org.github.io/ProjectIt/projectit-component/0.0.4/index.html).

    * One particular problem I'm working on is how to represent selections.
      Today, a selection is both in the GUI domain and the document domain. If
      the selection is only in the document domain, then two selection markers
      can appear in GUI since the same page can be projected more than once
      onto the workspace.  When I click a title for example, I want exactly
      that title to be selected, not the same underlying title on another
      projected page. What is a good representation of selections that take
      this into account?

    * I streamlined how props are generated. I wrote about the problem and my
      prefered pattern [here](/projects/rliterate/book2/index.html#742ebf5ff2d8457ab8c01b3721052425).

    * I improved the performance of the text widget. When calculating where all
      characters will appear on the screen, you have to take
      [kerning](https://en.wikipedia.org/wiki/Kerning) into account. Previously
      that was done by calculating the bounding rectangle for consequtively
      longer substrings. For example: bounding rectangle for "H", then bounding
      rectangle for "He", and so on until "Hello". Now only bounding rectangles
      for pairs of characters are calculated. So "He", and "el", "ll", and
      "lo".  My guess (which seems to hold for now) is that kerning only
      affects two consecutive characters, in which case this optimization
      works. Since the number of pairs are limited by the alphabet, these
      calculations can also be cached.
