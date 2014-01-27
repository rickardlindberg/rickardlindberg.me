---
title: Visualizing hierarchy and grouping
---

Today's thought is about visualizing hierarchy and groupings in code.

One place that does this fairly good is documentation for Haskell libraries.
Have a look at the documentation for the [`Map` data structure](http://hackage.haskell.org/packages/archive/containers/0.5.0.0/doc/html/Data-Map-Lazy.html)
for example.

A map is like a dictionary or a hash table in other languages. All functions
described in the documentation has to do with maps, so they obviously belong
there. But in addition to that, the functions have been grouped under headers
depending on their function. For example, there is one header called
"Insertion" where all functions related to inserting elements in a map are
grouped together.

This additional grouping information helps when trying to understand how to use
a map.

I think it is worthwhile to think about how information of this kind can be
incorporated into our code bases to make them easier to understand.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/ArQAg7RqBKf)
