---
title: Newsletter July 2020
date: 2020-08-03
tags: newsletter
---

This is what I've been up to in July 2020:

* I read Atomic Habits by James Clear and blogged about the key takeaway that I
  got from reading it: [the two-minute
  rule](/writing/atomic-habits-two-minute-rule/index.html).

* I read The Bullet Journal Method by Ryder Carroll. I am planning to blog
  about the key takeaway that I got from reading it.

* I read [Thoughts from a Not-So-Influential
  Educator](https://third-bit.com/2020/07/09/acm-sigsoft-award.html):

    > So, what would I change in 2020? First, I believe we should teach
    > students design by having them study the great works of the past, just as
    > other disciplines do. They should spend at least a full course reading
    > code and building working models of common applications in order to
    > become familiar with prior art and prior thinking. In their assignments,
    > they should compare and contrast undo/redo handling in Emacs and Vim or
    > the data structures beneath Git and Mercurial, or create programs like
    > Mary Rose Cook’s [Gitlet](http://gitlet.maryrosecook.com/), Matt
    > Brubeck’s [layout
    > engine](https://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html),
    > or Conor Stack’s [little
    > database](https://cstack.github.io/db_tutorial/).

    The mentioned resources are interesting, and I want to study them more. It
    feels to me like simplified (but not dumbed down) versions of programs to
    understand them better. That approach feels interesting to me. Can we
    structure all programs in a similar way with a simple model/core and then
    addonds to make it more practically useful?

* I played around with [Sway](https://swaywm.org/). As a big fan of
  [i3](https://i3wm.org/), I'm looking for an alternative that works under
  Wayland as that seems to be the future. My conclusion is that Sway will
  probably work well for me.

* I though about shooting some videos of
  [RLiterate](/projects/rliterate/index.html) to demonstrate its features and
  show what I'm working on at the moment. Let me know if you would be
  interested in that.

* I continued with [RLiterate](/projects/rliterate/index.html):

    * I worked on an improved representation of selections.

    * I though about how to visualise a selection that is not active. Say you
      select some text and focus another application. What should happen to the
      selection? Grayed out? Disappear? Just hide the cursor? Different
      applications seem to handle this differently.

    * I though about when to clear the selection entirely? The obvious
      solution, now that I think about it, is to clear it when something else
      is selected. But what if the page that the selection is on disappears?
      How to notice that and clear selection?
