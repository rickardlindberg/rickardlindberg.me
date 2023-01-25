---
title: What should a Continuous Integration (CI) server do?
date: 2023-01-25
tags: draft
---

I think that I have figured out what a Continuous Integration (CI) server
*should* do. It is very simple. Yet common CI tools like Jenkins make it hard
or near impossible.

## What is CI?

A lot of my thoughts on CI here is based on [AOAD2](https://www.jamesshore.com/v2/books/aoad2/continuous_integration), and trying to find the roots
of the practice. CI is probably a name that has different meanings to different
people.

CI is about two things:

* Integrate often.
    * Needs practice

* Promise to keep master green.
    * Needs self-testing code, needs practice.
    * Needs process.

## What is integrate?

* Merge to master.
* When do we want to merge to master?
    * When it is in a deployable state
* When is it that?
    * When we have done all tests and deployed to test environment / burned
      CD

## Tooling for CI should do the following

**A CI tool should integrate (merge commits to master) in a "safe" way.**

### Basics

    lock {
        git checkout master
        git merge origin/X (fail/warn if someone else merged before you)

        <command to self tests> (<sha1 of integration comitt to test>)
        // commit build

        git push master
    }

    <post command>
    // secondary build (in mulistage integration build)

    // improve:
    //   move stuff from secondary bulild -> comitt build

This ensures the following:

* Every integration passes self-test
* Integration promoted after test pass
* Master is always green
* Lock ensure the correct code is tested

### Additional functions

* Clean environments (no dev laptop with custom dependencies)
* Multiple environments (windows, linux, different python versions, etc)
* Communication / visibility
    * Notify team on successful integration
    * Show today's integrations in a dashboard
    * Show success rate of integrations
    * Present clear errors if pipeline fails

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
