---
title: A new home for Timeline
date: 2017-11-06
tags: timeline
---

In this article I present thoughts on a new home for
[Timeline](/projects/timeline/index.html).

Timeline is free software, and in order to develop it, we need tools and
infrastructure. For example, we need a place to host code repositories and a
system to facilitate discussions.  The most popular platform today providing
such functionality is probably GitHub. Timeline uses another platform called
SourceForge, nowadays seen as a predecessor to GitHub.

## Wanted features

I believe that when developing free software, building a community is
important. The easier it is to engage, the better.  I've identified two
features of a platform that I believe will make it easier to engage.

### Registration free discussions

Today, Timeline uses a mailing list (provided by SourceForge) for discussions.
The most common usage of the mailing list is that non registered users send
emails about problems they have. These emails provide valuable feedback. We
don't want to force users to register to provide this feedback. It should be
super simple. This works well today.

A problem with mailing lists is that when discussions continue in an thread,
everyone has to remember to hit "Reply all" on every email to keep everyone in
the loop. This inevitably fails. We end up with discussions where registered
users reply to the list only and the original (non registered) sender is not
notified, or where registered users reply directly to the original sender, and
no one else is notified.

We want a discussion system that is easy to post to (just send an email), and
that always keeps everyone in the loop.

### Registration free pull requests

In order to contribute code to Timeline today, you need push access to the code
repository. For that, you also need to be registered. This makes it harder to
submit patches for newcomers. We want it to be easy for people to contribute
patches. It should be as easy as to contribute feedback via email. We would
like to use the pull request model (popularized by GitHub), but no registration
should be needed to submit a pull request.

## Proprietary systems

Another reason that prompted me to think about a new home for Timeline
was that someone sent an email to the Timeline mailing list suggesting that we
should move away from SourceForge and included a link to the article [Notepad++
leaves
SourceForge](https://notepad-plus-plus.org/news/notepad-plus-plus-leaves-sf.html).
The Notepad++ project thought SourceForge behaved unacceptably and decided to
move somewhere else.

But what is a better home? GitHub seems
to behave nicer today, but what about tomorrow? The article [The Github
threat](https://carlchenet.com/the-github-threat/) argues that GitHub might not
be the silver bullet either. One argument against it is that everything is
centralized and you might loose all your data (issues, pull request
discussions) over night.

There is also the argument that [Free Software Needs Free
Tools](https://mako.cc/writing/hill-free_tools.html). This article argues that
if we advocate that software should be free, using proprietary tools make us
less credible.

## Way forward

SourceForge has been a good home for Timeline so far, but we would also like to
have registration free discussions and pull requests. There is also a worry
that SourceForge will do something undesirable as described in the Notepad++
article.

Moving to GitHub does not add any value to us because it does not provide the
additional features we want and it is also a proprietary platform. As far as I
know, GitHub does not support anonymous users to send feedback via email, so if
we move, we loose that feature that works well today.

We could install an existing free platform like GitLab on our own servers. That
would get rid of the worry that the platform might do something undesirable.
But I've not yet seen a platform that provides registration free discussions
and pull requests.  Perhaps those features can be added to an existing
platform, but I think it might be difficult because most platforms require
registered users, and these two features have to work without it.

My feeling is that the approach needed for these two features might justify a
completely new platform. My suggested way forward is therefore to develop a new
platform whose core features are registration free discussions and pull
requests. In addition, it would need features common to many platforms like
hosting of releases and a project web page.

I've started prototyping what such a platform might look like. I might write
down what I think are the minimum set of features needed to create a platform
that is better for Timeline than the current alternatives.
