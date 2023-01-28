---
title: What should a Continuous Integration (CI) server do?
date: 2023-01-25
tags: draft
---

I think that I have figured out what a Continuous Integration (CI) server
*should* do. It is very simple. Yet common CI tools like Jenkins make it hard
or near impossible.

## What is CI?

CI probably means different things to different people.

I've tried to find the root of the practice, and a lot of my thoughts here are
based on James Shore's descriptions in
[AOAD2](https://www.jamesshore.com/v2/books/aoad2/continuous_integration).

So with that in mind, CI is about two things:

1. Integrate often.
    * At least once a day
    * Needs practice
    * No diverged truths

2. Promise to keep master green.
    * Needs self-testing code, needs practice.
    * Needs process.

## What is integrate?

* Merge to master.
* When do we want to merge to master?
    * When it is in a deployable/releasble state
* When is it that?
    * When we have done all tests and deployed to test environment / burned
      CD

## Tooling for CI should do the following

**A CI tool should integrate (merge commits to master) in a "safe" way.**

### Basics

Here is pseudo code for how a CI server should integrate changes from a branch
in a Git repo:

    def integrate(repo, branch):
        with lock(repo):
            sh("git clone {repo}")
            sh("git merge origin/{branch}")   # fail/warn if someone else merged before you
            sh("<self test command>")
            sh("git push")
        sh("<more expensive test command>")

This ensures the following:

* Every integration passes self-test
* Integration promoted after test pass
* Master is always green
* Lock ensure the correct code is tested

Notes:

* Integration step should take no more than 10 minutes
* Improve: move stuff from expensive test command to self test command

### Additional functions

* Clean environments (no dev laptop with custom dependencies)
* Multiple environments (windows, linux, different python versions, etc)
* Communication / visibility
    * Notify team on successful integration
    * Show today's integrations in a dashboard
    * Show success rate of integrations
    * Present clear errors if pipeline fails

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
