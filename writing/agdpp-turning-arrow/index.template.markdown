---
title: Turning arrow
date: 2023-05-09
tags: agdpp,draft
agdpp: true
---

We have a basic version of a balloon shooter in place. We have a balloon moving
across the screen and an arrow that can be shot and hit the balloon to score a
point.

<center>
![Arrow shooting straight.](points.png)
</center>

When I play the game now, I feel like I want to turn the arrow. I think that
will make the game a little more fun. Then you have to control both angle and
timing instead of just timing to hit a balloon.

## Approach

* refactor first to make the implementation easy

## Result

<center>
![Turning arrow.](turning-arrow.png)
</center>

## Summary

Full source code from this episode on
[GitHub](https://github.com/rickardlindberg/agdpp/tree/turning-arrow).

See you in the next episode!

# TODO

    commit b08065975c2233f27916a049c75529d1be3295c5
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Thu Apr 27 07:03:44 2023 +0200

        Add Arrow.clone_shooting.

    ...

    commit e2f869113c37b91086888d262c22dd4376d47395
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Fri Apr 28 06:51:48 2023 +0200

        Arrow angle can be changed with keys.
