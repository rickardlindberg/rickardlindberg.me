---
title: Output Tracking vs Mocks
date: 2024-04-02
tags: draft
---

See also [How to test a router?](/writing/how-to-test-a-router/index.html)

See also [Favor real dependencies for unit
testing](https://stackoverflow.blog/2022/01/03/favor-real-dependencies-for-unit-testing/)

* App -> Command -> Coordination of -> Dep 1
                                    -> Dep ...
                                    -> Dep n    -> Dep n_1
                                                -> Dep n_2

* Don't mock internal dependencies vs output tracking

$:output:python:
def hello
$:END
