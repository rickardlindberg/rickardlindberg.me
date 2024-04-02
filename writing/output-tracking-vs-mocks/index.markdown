---
title: 'DRAFT: Output Tracking vs Mocks'
date: 2024-04-02
tags: draft
---

**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**

See also [How to test a router?](/writing/how-to-test-a-router/index.html)

See also [Favor real dependencies for unit
testing](https://stackoverflow.blog/2022/01/03/favor-real-dependencies-for-unit-testing/)

* App -> Command -> Coordination of -> Dep 1
                                    -> Dep ...
                                    -> Dep n    -> Dep n_1
                                                -> Dep n_2

* Don't mock internal dependencies vs output tracking

<div class="rliterate-code"><div class="rliterate-code-body"><div class="highlight"><pre><span></span><span class="k">def</span> <span class="nf">hello</span>
</pre></div>
</div></div>