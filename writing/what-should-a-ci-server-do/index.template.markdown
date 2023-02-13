---
title: What should a Continuous Integration (CI) server do?
date: 2023-01-25
tags: draft
---

I think I have figured out what a Continuous Integration (CI) server *should*
do. It is very simple. Yet common CI tools like Jenkins make it hard or near
impossible.

## What is CI?

CI probably means different things to different people.

I've tried to find the root of the practice, and a lot of my thoughts here are
based on James Shore's descriptions in
[AOAD2](https://www.jamesshore.com/v2/books/aoad2/continuous_integration).

So with that in mind, CI to me is about two things:

1. Integrate often.
2. Promise to keep the main branch working at all times.

## What is integrate?

Integrate means to merge your changes into the main branch. This branch is
commonly also referred to as master or trunk.

## How often should we integrate?

From what I've read, the consensus seems to be that you should at least
integrate your code once a day. If you do it less frequently, you are not doing
*continuous* integration.

## When is it safe to integrate?

Every time we integrate code, we have to make sure that the main branch is
still working. (The second aspect of CI.) That is, we can not integrate code
that breaks functionality.

How can we do that?

The only way to do that (and still integrate often) is with an automatic test
suite.

Before you integrate your code, you want to run the test suite to make sure
everything still works.

**That test suite should give you confidence that when it's time to deploy the
software to production, it will just work.**

To gain that confidence, you probably need to include deploying to a test
environment in your test suite.

## Human aspect

James Shore points out that [you can do CI without a
tool](https://www.jamesshore.com/v2/blog/2006/continuous-integration-on-a-dollar-a-day),
and that CI is more about a way of working.

No tool can choose to integrate your code often. You have to change your way of
working so that you can integrate more often. This requires practice.

No tool can enforce that your main branch is always in a working state. You
have to have a mindset of working like that. This requires practice.

However, there are some things that a tool can help with. To make it easier to
work in this way.

## Tooling for CI should do the following

**A CI tool should merge changes to the main branch in a safe way.**

### Basics

Here is pseudo code for how a CI server should integrate changes from a branch
in a Git repo:

```python
def integrate(repo, branch):
    with lock(repo):
        sh("git clone {repo}")
        sh("git merge origin/{branch}")
        sh("<command to run test suite>")
        sh("git push")
```

The lock step ensures that only one integration can happen at a time. If you
have two branches that want to integrate, one has to wait for the other to be
integrated first.

A branch is then integrated by performing a `git merge` followed by a command
to run the test suite. This test suite should be defined in the repo somewhere.

If the test suite passes, a `git push` is performed to "promote" the changes to
the main branch.

This workflow ensures that every change that is integrated to the main branch
passes the test suite.

That is the basic function. Let's look at some variations.

### Clean environments

* Clean environments (no dev laptop with custom dependencies)
* The CI server should set up a clean environment before running the test suite

### Multiple environments

* Multiple environments (windows, linux, different python versions, etc)

### Communication / visibility

* Notify team on successful integration
* Show today's integrations in a dashboard
* Show success rate of integrations
* Present clear errors if pipeline fails

### Multistage

The lock step ensures that only one integration happens at a time.

In some situations you might have a longer running test suite that you don't
want to block further integrations.

A CI server could support that something like this:

```python
def integrate(repo, branch):
    with lock(repo):
        sh("git clone {repo}")
        sh("git merge origin/{branch}")
        sh("<command to run fast test suite>")
        sh("git push")
    sh("<command to run slow test suite>")
```

As a rule of thumb, the fast test suite should take more than 10 minutes.

And it is also probably more beneficial to try to speed up your test suite than
to have a separate slow test suite after integration.

However, multistage might still be useful:

* Improve: move stuff from expensive test command to self test command
* Special long running tests

## Problems with common workflow

### Run pipeline after commit

### Run pipeline on branch, then again after merge

## Benefit even if not "real" CI

## Why don't CI-severs work like this?

* Difficult with SVN?

## What about pull requests?

* PRs and CI don't play nice together.
    * In PR's, you merge by pressing a button
    * In CI, the CI server merges.

* Variants (although none of them "real" CI)
    * PRs can be a stage before CI
    * Review can be a step in CI
