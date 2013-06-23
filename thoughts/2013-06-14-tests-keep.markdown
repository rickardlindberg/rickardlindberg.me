Today's thought is about what tests to keep to get the maximum benefit out of
them.

Tests have at least two purposes: One is to act as documentation for the system
(they show how functions are called and what the expected result is). Another
is to ensure that the system behaves correctly.

Typically when we do TDD we end up with many small examples. For example,
here are some tests for the prime factor kata:

    assert primeFactors(1) == []
    assert primeFactors(2) == [2]
    assert primeFactors(3) == [3]
    assert primeFactors(4) == [2, 2]
    assert primeFactors(5) == [5]
    assert primeFactors(6) == [2, 3]
    assert primeFactors(7) == [7]
    assert primeFactors(8) == [2, 2, 2]

Writing these many examples help us come up with the algorithm in incremental
steps. They also give pretty good test coverage. At least for small numbers.
But how about documentation? Can we understand how prime factors work by
reading these examples? Maybe. But we can do better.

At the end of the prime factor kata it is common to write a test like this to
ensure that our algorithm is correct:

    assert primeFactors(2*2*3*5*7) = [2, 2, 3, 5, 7]

Not only does this give us assurance that the algorithm works, it is also a
nice way to document how prime factors are calculated. Can we improve it even
more? What about this?

    primeFactors = generateListOfPrimeFactors()
    product = multiplyNumbers(primeFactors)
    assert primeFactors(product) = primeFactors

Given a list of prime factors, any list, we can multiply those factors (which
is a much simpler problem than doing the reverse), and assert that when we call
primeFactors with the product, we get back the original list of prime factors.

That is a quite nice description of what primeFactors does. It also gives us
better test coverage since we generate various random lists of prime factors
instead of only [2, 2, 3, 5, 7].

At this point we can question the value of keeping the original examples. After
all they are just specific examples of the generic test we wrote last. They
probably don't add much documentation. If there are edge cases in there, they
might add some test coverage.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/YRYw8gdUCqv)
