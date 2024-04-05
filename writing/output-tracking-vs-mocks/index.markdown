---
title: 'DRAFT: Output Tracking vs Mocks'
date: 2024-04-05
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

In this blog post we're going to explore how to write and test a Git client
using the [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
approach. Specifically we're going to focus on [Output
Tracking](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#output-tracking)
and explore how to apply it to this example.

## Example Git client

The example Git client is a CLI-application that provides a simplified
interface to Git. This represents a [real world scenario](https://gut-cli.dev/)
yet can be made small enough for an example.

For the purposes of this example, we will implement two commands:

```
myscm save  -> git commit

myscm share -> git push
```

## Architecture

```
App --+--> SaveCommand --+--> Process
      |                  |
      |                  +--> Filesystem
      |
      +--> ShareCommand ----> Process
      |
      +--> Args
      |
      +--> Terminal
```

## Notes

See also [How to test a router?](/writing/how-to-test-a-router/index.html)

See also [Favor real dependencies for unit
testing](https://stackoverflow.blog/2022/01/03/favor-real-dependencies-for-unit-testing/)

* Don't mock internal dependencies vs output tracking

* p.117

    * Event: the action that is performed.

    * Output tracking (invisible writes). Write to external system.

    * Track writes in terms of behaviors your callers care about.

        * Logger writes to stdout. (Write string.)

        * Callers care about data written. (Track data.)

* p.123

## Template

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">hello</span>
</pre></div>
</div></div>