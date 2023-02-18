---
title: 'DRAFT: What should a Continuous Integration (CI) server do?'
date: 2023-02-18
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

I think I have figured out what a Continuous Integration (CI) server *should*
do. It is very simple. Yet common tools used for CI, like Jenkins, make it hard
or near impossible.

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

## How often should you integrate?

From what I've read, the consensus seems to be that you should at least
integrate your code once a day. If you do it less frequently, you are not doing
*continuous* integration.

## When is it safe to integrate?

Every time you integrate code, you have to make sure that the main branch is
working. (The second aspect of CI.) That is, you can not integrate code that
breaks functionality.

How can you do that?

The only way to do that (and still integrate often) is with an automatic test
suite.

Before you integrate your code, you want to run the test suite to make sure
that everything still works.

**The test suite should give you confidence that when it's time to deploy the
software to production, it will just work.**

To gain that confidence, the test suite probably needs to include steps to
deploy the software to a test environment and do some smoke test on it. Not
only unit tests.

## Attitude, not a tool

James Shore writes that [Continuous Integration is an Attitude, Not a
Tool](https://www.jamesshore.com/v2/blog/2005/continuous-integration-is-an-attitude)
and points out that [you can do CI without a
tool](https://www.jamesshore.com/v2/blog/2006/continuous-integration-on-a-dollar-a-day).

No tool can choose to integrate your code often. You have to change your way of
working so that you can integrate more often. This requires practice.

No tool can enforce that your main branch is always working. You
have to have a mindset of working like that. This requires practice.

However, there are some things that a tool can help with. To make it easier to
work in this way.

## CI server functionality

**A CI server should merge changes to the main branch in a safe way.**

### Basic workflow

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

A branch is then integrated by performing a `git merge`.

To make sure the changes work, a test suite is then run. This test suite
should be defined in the repo.

If the test suite passes, a `git push` is performed to "promote" the changes to
the main branch.

This workflow ensures that every change that is integrated to the main branch
work. Where "work" is defined as passing the test suite.

That is the basic function that I think a CI server should perform. Let's look
at some directions where this design can be evolved to make a more full fledged
CI server.

### Clean environments

One thing that a dedicated CI server helps prevent is the problem that code
works on one developer's machine, but not another. Perhaps it is due to a
dependency missing on one developer's machine.

With a CI server, the one true environment is the CI server's environment.

Preferably, this should also be set up in the exact same way before every test
run so that two test runs have the exact same clean environment.

Clean environments make test runs more predictable and helps making
integrations safely.

Setting up a clean environment might look different in different languages. One
option would be to use Docker containers. In the Python world, virtual
environments could be set up for each test run.

Anything generic function that a CI server can do to help in this area is good.

### Multiple environments

Another advantage of a dedicated CI server is that you can make sure your code
works in an environment that you don't have access to on your development
machine.

You might write Python code that should work on both Windows and Linux, but
your laptop only runs Windows.

A CI server should have functionality to run code in different environments.

### Pipeline language

To take full advantage of the CI server, the "command to run the test suite"
should be written in a "pipeline language" that the CI server understands.

Consider this pseudo example:

    step('compile') {
        sh('make')
    }
    parallel {
        environment('unix') {
            sh('./test')
        }
        environment('windows') {
            sh('test.exe')
        }
    }

This script could not have been written as a Bash script, because then it could
not have taken advantage of the CI server functionality to run commands in
different environments.

### Communication / visibility

Another aspect of continuous integration is communication.

For example, when you integrate code, you want to tell your team members about
the change so that they can pull your changes and test their code against it.

A CI server can help communicate. It can

* notify a team on successful integration
* show today's integrations in a dashboard to make visible what is happening
* show success rate of integrations to give an idea of how we are doing
* present clear errors when an integration fails
* present clear view of pipeline and what steps were run

### Multistage

The lock step ensures that only one integration can happen at a time.

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

As a rule of thumb, the fast test suite should take no more than 10 minutes.

And it is also probably more beneficial to try to speed up your test suite than
to have a separate slow test suite after integration.

However, multistage might still be useful:

* Improve: move stuff from expensive test command to self test command
* Special long running tests

## Common "CI" workflows and their problems

I primarily have experience with using Jenkins as a CI server. And the two most
common patterns in Jenkins prevent you from doing continuous integration.
Let's have a look.

### Run pipeline after commit

This patterns runs a pipeline only after you have merged your changes to the
main branch.

If the test suite fails, you have broken code on the main branch, and everyone
who pulls your code will base their changes on something broken.

If you are serious about continuous integration, you fix this problem
immediately. Either by reverting the change or merging a fix. It might not be
too big a problem.

If you are not serious about continuous integration, you might leave the code
broken and hope that someone else fixes it.

With a CI server I describe in this article, it is simply not possible to merge
something broken.

### Run pipeline on branch, then again after merge

This patterns runs a pipeline on every branch so that you know that your
changes are good before you merge them. And when you merge them, the pipeline
is run again.

This is a slight improvement over the previous pattern, but it still has a
flaw. Consider this scenario:

    0---0
         \
          \---A
           \
            \---B

`A` and `B` are two branches that both have passing test suites, so they both
go ahead and merge, resulting in this:

    0---0-------A'---B'
         \     /    /
          \---A    /
           \      /
            \----B

`A'` has already been tested on the branch, but `B'` has never been tested.
That is, the combination of `A`'s and `B`'s changes have never been tested,
until they are both merged.

With a CI server I describe in this article, this problem is solved with
"synchronous integration" where multiple integrations have to wait for
each other.

If you choose to do "multistage integration", you still have this problem. But
with my CI server it is a choice. A trade off that you can make.

## Why don't CI severs work like this?

I have two speculations.

First, if your team is committed to continuous integration, broken code on the
main branch might not be too big a deal since everyone is committed to fixing
it fast.

Second, back in the day of using SVN (which was my fist version control
system), branching was expensive. The default behavior was to do trunk based
development. That is, push directly to the main branch. Having a CI tool do the
actual integration was probably technically more difficult. However, now with
Git, that is no longer true.

Do you know why CI servers don't work like this? Please let me know.

## What about pull requests?

Pull requests are a common way of working, but they don't play nicely together
with CI.

First of all, when working with pull requests, you integrate your code by
pressing a button that will perform the merge. With a proper CI tool, the CI
tool performs the merge. With the former, no tool can prevent broken code on
the main branch.

Second of all, pull requests, at least a blocking ones, add delay to the
process of integrating code, making it difficult to integrate often.
