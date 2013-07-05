Today's thought is about the refactoring step in TDD.

Today we practiced the bowling kata in the office. One thin I noticed was how
we continued to write and implement new tests without refactoring property. It
was not that we forgot the refactoring step. We did consider refactoring. And
we did improve some things. But somehow implementing each new test didn't come
easy. The change to make it work did not fit well.

So clearly we didn't refactor enough. But how can we know what is enough?

From today's session, I thought of the following conclusion.

We had one particular solution in mind as we went along. But that solution was
not clearly expressed in the code. The code contained pieces of different
solutions. In some parts we were thinking in terms of frames, and in other
parts we were thinking in terms of rolls. We never clearly expressed our
current intended solution in code.

So maybe that is a criteria for being done: when our current view of the
solution is clearly expressed in code.

But what if the current solution leads us down the wrong path? In that case we
have to consider a different solution. But such a rewrite should be easier if
the code is clearly written in one way instead of different solutions showing
up in different places.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/CiQxcHmoh5w)
