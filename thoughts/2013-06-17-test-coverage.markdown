Today's though is about how to know when test coverage is good enough.

One benefit of having good test coverage is that you are less afraid to modify
code. It is powerful to be able to check the correctness of your implementation
by running a suit of tests with a single press of a button. But if you are
going to trust your tests that much, it is important that the coverage is
relatively good. But what is good enough?

I suggest two activities that can help you answer that question:

* Read the tests and see if they seem to cover enough
* Try to break the implementation in a way that still make the tests pass

In the first activity you familiarize yourself with the existing tests (and
also the implementation) and learn what is there. You might think of cases that
are not there that you can add. You might not get a good overview of what are
actually tested, so you refactor the tests to more clearly reflect that.

In the second activity you might be able to break the implementation in a way
that your tests do not catch. In that case, you have found a test case that you
might want to write. Otherwise, the test coverage is probably good enough.

I think you need different coverage in different areas of the code to feel
confident when making a change. Instead of saying that code coverage should be
a specific percentage, you can do these two activities to help you convince
yourself that coverage is good enough.
