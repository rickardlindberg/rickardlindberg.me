---
title: What should a Continuous Integration (CI) server do?
date: 2023-01-25
tags: draft
---

*After drafting this article, I asked for feedback on
[James'](https://www.jamesshore.com/) Discord.
[Emily](https://coding-is-like-cooking.info/) wrote back and said that this
sounded a lot like pre-tested integration that she had written about
([here](https://www.eficode.com/blog/pre-tested-integration) and
[here](https://www.eficode.com/blog/pre-tested-integration2)) earlier. She
describes almost the exact same workflow as I imagine with this CI server, and
there is also a Jenkins plugin to support that workflow. I encourage you the
check out her writing as well.*

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

## What does integrate mean?

Integrate means to merge your changes into the main branch. This branch is
commonly also referred to as master or trunk.

## How often should you integrate?

From what I've read, the consensus seems to be that you should integrate at
least once a day. If you do it less frequently, you are not doing *continuous*
integration.

## When is it safe to integrate?

Every time you integrate, you have to make sure that the main branch is
working. This is the second aspect of CI.

How can you do that?

The only way to do that, and still integrate often, is with an automatic test
suite.

Before you integrate your code, you want to run the test suite to make sure
that everything still works.

**The test suite should give you confidence that when it's time to deploy to
production, it will just work.**

To gain that confidence, the test suite probably needs to include steps to
deploy to a test environment and do some smoke test on it. Not only unit tests.

## Attitude, not a tool

James Shore writes that [Continuous Integration is an Attitude, Not a
Tool](https://www.jamesshore.com/v2/blog/2005/continuous-integration-is-an-attitude)
and points out that [you can do CI without a
tool](https://www.jamesshore.com/v2/blog/2006/continuous-integration-on-a-dollar-a-day).

No tool can choose to integrate your changes often. You have to change your way
of working so that you *can* integrate more often and also do so. This requires
practice.

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

The branch is then integrated by performing a `git merge`.

To make sure the new main branch works, a test suite is then run. This test
suite should be defined in the repo.

If the test suite passes, a `git push` is performed to "publish" the new main
branch.

This workflow ensures that every change that is merged into the main branch
works. Where "works" is defined as passing the test suite.

That is the basic function that I think a CI server should perform. Let's look
at some directions where this design can be evolved to make a more full fledged
CI server.

### Clean environments

One thing that a dedicated CI server helps prevent is the problem that code
works on one developer's machine, but not on another's. Perhaps it is due to a
dependency missing on one developer's machine.

With a CI server, the one true environment is the CI server's environment.

Preferably, this should also be set up in the exact same way before every test
run so that two test runs have the exact same clean environment.

Clean environments make test runs more predictable and helps make integrations
safe.

Setting up a clean environment looks different in different contexts. One
option would be to use Docker containers. In the Python world, virtual
environments could be set up for each test run.

Any function that a CI server can perform to help set up a clean environment is
useful.

### Multiple environments

Another advantage of a dedicated CI server is that you can make sure that your
code works in an environment that you don't have access to on your development
machine.

You might write Python code that should work on both Windows and Linux, but
your laptop only runs Windows.

A CI server should have functionality to run the test suite in different
environments.

### Pipeline language

To take full advantage of the CI server, the "command to run the test suite"
should be written in a "pipeline language" that the CI server understands.

Consider this pseudo example:

    step('compile') {
        sh('make')
    }
    parallel {
        step('test unix') {
            environment('unix') {
                sh('./test')
            }
        }
        step('test windows') {
            environment('windows') {
                sh('test.exe')
            }
        }
    }

This script could not have been written as a Bash script for example, because
then it could not have taken advantage of the CI server functionality to run
commands in different environments.

### Communication

Another aspect of continuous integration is communication.

For example, when you integrate, you want to tell your team members about the
change so that they can pull your changes and test their code against it.

A CI server can help communicate. It can for example do the following:

* Notify the team on a successful integration.
* Show today's integrations in a dashboard to visualize what's
  happening.
* Show success rate of integrations to give an idea of how the team is doing.
* Present clear errors when an integration fails.
* Present a clear view of a pipeline and what steps were run.

### Multiple test suites

The lock step in the basic workflow ensures that only one integration can
happen at a time.

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

Of course, when you do this, you risk breaking the main branch since all tests
are not run before the change is integrated.

One scenario where this could be useful is if you have a slow running test
suite today that you can't make instantly faster. You can start using this
pattern with the goal of making all your slow tests fast. As a rule of thumb,
the fast test suite should not take more than 10 minutes. If it takes longer for
an integration to complete, chances are that you start multitasking because you
don't want to wait for it.

Some tests might also be impossible to run in less than 10 minutes. In that
case, this pattern is also good. But make sure that all basic functionality is
tested in the fast test suite.

Writing fast tests indeed needs practice, but it is possible.

## Common "CI" workflows and their problems

When it comes to tools commonly used for CI, I primarily have experience with
Jenkins. And the two most common patterns in Jenkins, which a believe are not
unique to Jenkins, prevent you from doing continuous integration. Let's have a
look.

### Run pipeline after commit

This pattern runs a pipeline only after you have merged your changes to the
main branch.

If the test suite fails, your main branch is broken, and everyone who pulls
your changes will base their work on something broken.

If you are serious about continuous integration, you fix this problem
immediately. Either by reverting the change or merging a fix. It might not be
too big a problem.

If you are not serious about continuous integration, you might leave the main
branch broken and hope that someone else fixes it.

With a CI server I describe in this article, it is simply impossible to merge
something broken.

### Run pipeline on branch, then again after merge

This patterns runs a pipeline on every branch so that you know that your
changes work before you merge them. And when you merge them, the pipeline is
run again.

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

With a CI server I describe in this article, this problem is solved with the
lock where multiple integrations have to wait for each other.

If you use the multiple test suites pattern, you still have this problem. At
least for functionality only covered by the slow test suite. But then it's a
choice you make. You decide if the trade off is worth it for you or not.

## Why don't tools for CI work like this?

I think that tools for CI should help you do CI well. Why don't they?

I have two speculations.

First, if your team is committed to doing continuous integration, a broken main
branch might not be too big a deal since everyone is committed to fixing it
fast.

Second, back in the day of using SVN (which was my fist version control
system), branching was expensive. The default way to share changes was to push
directly to the main branch. Having a CI tool do the actual integration was
probably technically more difficult. However, now with Git, that is no longer
true.

Do you know why tools for CI don't work like I describe in this article? Please
let me know.

## What about pull requests?

Pull requests are a common way of working, but they don't play nicely together
with CI.

First of all, when working with pull requests, you integrate your code by
pressing a button that will perform the merge. With a CI tool like the one I
describe in this article, the CI tool performs the merge. With the former, no
tool can prevent broken code on the main branch. (The best they can do is test
the branch, then test again after merge.)

Second of all, pull requests, at least blocking ones, add delay to the
process of integrating code, making it difficult to integrate often.

Pull requests are often used to review changes before they are merged. In a CI
server that I describe in this article, there is nothing preventing you from
having a manual review step before the CI server is allowed to merge. However,
a manual review step adds delays and makes it difficult to integrate often.
