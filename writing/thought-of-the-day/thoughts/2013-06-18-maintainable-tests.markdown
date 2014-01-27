---
title: Maintainable tests
---

Today's thought is about how to write more maintainable tests.

I discussed this topic with a college after a coding dojo we had today.

Consider these tests from the bowling kata:

    assert score([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) == 0
    assert score([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]) == 20

What is wrong with these from a maintainable point of view? They are very tied
to the signature of `score`. They include information about the implementation.
And that information is duplicated in both tests cases.

A way to make these tests more maintainable is to create a small DSL for
bowling scoring. It might look something like this:

    when(rolling(0).times(20)).the_score_is(0)
    when(rolling(1).times(20)).the_score_is(20)

Here we have described the rules of bowling. They are always true no matter
what the implementation looks like.

Of course we need to implement this DSL in our test and somewhere call the
`score` function. But if we later decide to change the signature of the `score`
function, we only need to update the DSL. Our test cases are still valid and
don't need to change. They are expressed at a higher level. At the domain of
bowling scoring.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/bC3mXD3YNs4)
