---
title: The danger with implicit if statements in Python
---

*Written 3 November 2014.*

In Python we can put an expression in an if statement that is not a boolean.
For example:

    a_list = [1, 2, 3]
    if a_list:
        # do something

The expression will evaluate to either true or false. Some examples of
expressions that will evaluate to false:

* `[]` (empty list)
* `""` (empty string)
* `0` (the number 0)
* `None` (the null value)

Some examples of expressions that will evaluate to true:

* `[1, 2]` (non-empty list)
* `"hello"` (non-empty string)
* `88` (the number 88)

So if we are only interested in knowing if a value is truthy, we do not need to
make an explicit comparison in the if statement. The first example with an
explicit comparison would look like this:

    if a_list != []:
        # do something

We can argue that the first example read better because there is less cruft in
the expression, but there is one real danger in being implicit. Consider a
function that returns either a number or None if no number could be returned.
We want to run some code only if we get a number back:

    number = give_me_a_number()
    if number:
        # do something

This works fine for most numbers:

* 1 -> we do something (expected)
* 2 -> we do something (expected)
* None -> we do not do something (expected)

Except for:

* 0 -> we do not do something (not expected)

The number 0 is a number, so we would like to do something with it. But on the
other hand, the number 0 evaluates to false. So with an implicit check, it is
not considered truthy, and we will not enter the if block. What we should have
done instead was this:

    if number is not None:
        # do something

I have made this mistake more than once, and I'm starting to think that
explicit if statements should always be used except in special cases.
