---
title: Bitten by Python generators
date: 2017-03-11
tags: python
---

Today I found a bug in a piece of Python code that I had written. The buggy
code was the result of not taking into consideration how generators in Python
work. It looked like this:

```python
def main():
    try:
        logging.info("Processing items")
        items = get_the_items()
    except:
        logging.exception("Could not get items")
    else:
        for item in items:
            process_item(item)
```

Can you spot the error? What could possible go wrong with this code?

When I was debugging it, it was printing the info message, but did not log an
exception. Execution continued in the else-clause, but suddenly an exception
was raised when looping over the items (outside of `process_item`). How can
that happen? The except-clause should catch all exceptions. And just iterating
over a collection should not raise an exception.

The answer is that `get_the_items` returned a generator. It looked like this:

```python
def get_the_items():
    for item in read_items_from_disk():
        if item.is_good():
            yield item
```

The exception actually came from `read_items_from_disk`, but since this code
creates a generator, it is not executed until the collection is accessed (which
happened in the else-clause). So the exception was actually raised when
starting looping over items.

## What about skipping the else-clause?

To ensure that an exception from the generator is caught, the `main` function
could be written like this:

```python
def main():
    try:
        logging.info("Processing items")
        for item in get_the_items():
            process_item(item)
    except:
        logging.exception("Could not get items")
```

I don't like this version because it also catches exceptions from
`process_item`. I like the try-except-else syntax because it allows narrower
exception regions in a nice looking way.

## Why did I use a generator?

I made `get_the_items` return a generator mainly because I thought it read
better. The alternative I came up with looked like this:

```python
def get_the_items():
    items = []
    for item in read_items_from_disk():
        if item.is_good():
            items.append(item)
    return items
```

I didn't like the temporary `items` variable. And it is two lines longer than
the generator version.

## What about list comprehensions?

Another way to write `get_the_items`, avoiding the temporary variable, is to
use list comprehensions. It would look like this:

```python
def get_the_items():
    return [
        item
        for item in read_items_from_disk()
        if item.is_good()
    ]
```

I find this code reads as good as the generator version (even though it is two
lines longer). This is what I ended up using. But `get_the_items` was a bit
more complicated than in the example, so I had to divide it into two list
comprehensions.

## What about memory consumption?

One argument for using generators is that they consume less memory. The whole
collection of items do not have to fit in memory at once, only the one
currently being processed.

In my case I had already read all items into memory in a previous step. The
`get_the_items` function was mainly used to transform and filter the items. So
I would not have used much less memory by using generators. Also, my
collections were small, so having them in memory was not a problem.

## Conclusion

Generators have some nice properties that can be useful. However, after being
bitten by them I now think they should only be used if those properties are
absolutely needed. I will favor list comprehensions over generators if I can
afford to keep the whole collection in memory.
