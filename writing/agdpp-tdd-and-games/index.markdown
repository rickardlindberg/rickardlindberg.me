---
title: 'DRAFT: Does TDD work when building a game?'
date: 2023-06-06
tags: agdpp,draft
agdpp: true
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

When I [started](/writing/agdpp-introduction/index.html) this series, one of
the areas that I wanted to explore was how well TDD works when building a game.

The short answer is that I find that it works as well as on any other project
I've done.

The longer answer is that TDD might not be enough. There are some things which
are hard to test in a game. That's what I want to talk about in this blog post
and also how we can overcome the limit of TDD in those situations.

## Summary

See you in the next episode!

## TODO

* TDD did not work for joystick movement. I needed to "feel" what was
  right. I did it as a spike.

* Make tests independent of specifics speeds/angles so that they can be
  adjusted until they feel good without tests failing.

* Marick quite

* Examples

    commit 7ff61c0ab2fd736bc75e32ba0ff109e838dcd716

        Modify speed and positions for better gameplay.

    commit 328fb9829ec1cd93a108e6cdadf730207975bdbd (HEAD -> main, origin/main)

        Make tests independent of turn speed and adjust until it felt good in game.

    commit 1f50d4deb34226531fdb50df2ea4f5db3c2d2906 (HEAD -> main)

        Create balloon particles that "look good".

* Learning about events
    * Test applications
        * Quickly takes you to a scene in a game
        * Test specific function that don't fit TDD
        * Test look and feel of level 99
        * Test look and feel of level progression speed

> And it seems to me that with a video-oriented game, we always wind up needing
> to watch it run, both for confidence and because, every now and then, we
> discover a problem.

https://ronjeffries.com/articles/-y023/python/-o110/110/

* Timeline GUI preview

<p>
<center>
![Score text.](score-text.png)
</center>
</p>

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span>
</pre></div>
</div></div>