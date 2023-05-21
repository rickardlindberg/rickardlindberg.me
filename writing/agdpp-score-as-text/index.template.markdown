---
title: Score as text
date: 2023-05-21
tags: agdpp,draft
agdpp: true
---

Currently, the game keeps track of the score by drawing yellow point markers.
One for each point:

<p>
<center>
![Point markers.](points.png)
</center>
</p>

I find it tedious to count them when my son want's to know the score, and I
think he also expressed that he wanted to have the score as a number in the
upper right corner.

That's what we will work on in this episode. The end result will look like
this:

<p>
<center>
![Score text.](score-text.png)
</center>
</p>

## Summary

See you in the next episode!

## TODO

$:output:python:
$:END

    commit 7112e2a3075c6f83c9753b62e046be6f2a7654b8
    Author: Rickard Lindberg <rickard@rickardlindberg.me>
    Date:   Mon May 1 08:45:31 2023 +0200

        Score is draw as number instead of yellow circles that you have to count.
