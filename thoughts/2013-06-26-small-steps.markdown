Today's thought is about taking small steps.

Yesterday I watched a presentation by J. B. Rainsberger where he gave an
introduction to TDD. At one point when he was about to write code to pass a
test, he explained that he likes to fake the implementation even though the
real implementation seems trivial. The reason behind it was that the number of
times he had thought he knew the solution and was wrong was enough to make
faking it and then passing it a faster process.

I noticed a similar feeling when I was programming on a project of mine
yesterday. I was refactoring a piece of code that I had tests for. I made small
changes and ran the tests frequently. But at one point they failed, and I
couldn't understand why. It was perhaps 30 seconds since I last ran the tests
and they passed, so the error must have been introduced in the past 30 seconds.

Because I was unable to undo to the last working test run, I had to think what
I had done that could have caused the error. After a few minutes a thinking, I
noticed that I had accidentally deleted a line of code that I shouldn't have.
The point is that in just 30 seconds, I managed to screw something up that took
me more than 30 seconds to resolve. So small steps indeed are important.

The reason I couldn't undo was because I had switched to a new file in Vim, and
somehow the undo history got lost. I should perhaps learn more about undo
history in Vim to prevent this from happening in the future.

Another thing I came to think of was to commit changes automatically after a
successful test run, but just temporarily. Perhaps using `git stash` or
something. What I want to be able to do is to roll back to the last point where
all tests passed.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/D1cANTio3zm)
* [TDD Presentation](http://www.jbrains.ca/permalink/the-worlds-best-intro-to-tdd-demo-video)
