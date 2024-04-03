---
title: Output Tracking vs Mocks
date: 2024-04-02
tags: draft
---

In this blog post I'm going to explore how to write and test a Git front-end
using the [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
approach. Specifically I want to focus on [Output
Tracking](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#output-tracking)
and explore how to apply it to this example.

## Git front-end

https://gut-cli.dev/

```
myscm save -> git commit

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

$:output:python:
def hello
$:END
