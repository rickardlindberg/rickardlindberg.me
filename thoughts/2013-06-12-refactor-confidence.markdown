Today's thought is about how you can refactor code with higher confidence.

Imagine how you would change your attitude towards refactoring code if you knew
that the refactored version worked exactly like the original for a large
(automatically generated) set of inputs.

It is a common technique to cover a piece of code with tests before you change
it. That way you can change it and have a higher confidence that it still
works afterwards. But what tests should you write?

One thing you can do is to call the function you want to refactor and see what
the output is. Then record that output as the expected result in a test. If you
do this for a few inputs, you get some coverage. The tests might not serve as
great documentation, but they will serve as a safety net when refactoring.

You end up with a set of tests like this where xn are the different inputs and
yn are the different outputs:

    f(x1) = y1
    f(x2) = y2
    f(x3) = y3

Then when you refactor the function f, the tests should still pass.

What if you could automate this process? What if you could somehow generate the
different inputs automatically, would it then be possible to test the
refactored function automatically? Yes, it would. The following assertion
should hold where f' is the refactored version of f:

    assert f(x) = f'(x)

This says that for any input to the function f, it should produce the exact
same output as the refactored version f'.

This assertion depends on the inputs being somehow automatically generated and
on the two functions existing at the same time.

Tools can probably be developed to help write tests of this kind so that
automatically generating inputs becomes easies and so that duplicating the
functions does not become that tedious.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/4UBzckRvFNx)
