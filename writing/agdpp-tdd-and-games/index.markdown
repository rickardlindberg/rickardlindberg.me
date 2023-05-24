---
title: 'DRAFT: Does TDD work when building a game?'
date: 2023-05-24
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

When I [started](/writing/agdpp-introduction/index.html) this series, one of
the points I wanted to explore was how well TDD has worked when building a
game.

The short answer is yes, it absolutely works.

The longer answer is ...

## Summary

See you in the next episode!

## TODO

* TDD did not work for joystick movement. I needed to "feel" what was
  right. I did it as a spike.

* Make tests independent of specifics speeds/angles so that they can be
  adjusted until they feel good without tests failing.

* Marick quite

commit 7ff61c0ab2fd736bc75e32ba0ff109e838dcd716
Author: Rickard Lindberg <rickard@rickardlindberg.me>
Date:   Fri Apr 28 06:57:39 2023 +0200

    Modify speed and positions for better gameplay.

commit 328fb9829ec1cd93a108e6cdadf730207975bdbd (HEAD -> main, origin/main)
Author: Rickard Lindberg <rickard@rickardlindberg.me>
Date:   Tue May 2 07:24:37 2023 +0200

    Make tests independent of turn speed and adjust until it felt good in game.

* Learning about events
    * Test applications
        * Quickly takes you to a scene in a game
        * Test specific function that don't fit TDD
        * Test look and feel of level 99
        * Test look and feel of level progression speed

<p>
<center>
![Score text.](score-text.png)
</center>
</p>

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>