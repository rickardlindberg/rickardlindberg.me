---
title: Discoverable tests
---

Where do you put your tests so that you can find them with ease? How do you
know if a piece of code is covered with tests?

I'm not satisfied with having test in a separate directory. I always struggle
with questions like "Should you mirror the source directory?" and "Where do you
put test that includes multiple pieces of the codebase?". It feels more natural
to me to store the test together with the implementation. But that doesn't feel
right for tests that include multiple pieces.

Perhaps the best way to find the tests for a piece of code is to change the
code and see which tests fail. That requires running the whole suit. So it must
be relatively fast. In that situation, the exact location of the test is
perhaps not that important.

--

* [Comments on Google+](https://plus.google.com/112175093836850283531/posts/HW2YnNppqhT)
