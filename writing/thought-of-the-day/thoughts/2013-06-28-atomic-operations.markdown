---
title: Atomic operations
---

Today's thought is about atomic operations.

I've thought about this before, and I came to think about it again today. What
I mean by an atomic operation is something like a rename of a variable. Either
that variable is renamed, or it is not. We never want a half renamed variable
where only some usages of it was renamed. Then we end up with code that does
not compile.

Automatic refactorings are the closest we come to atomic operations in our
development environments. They allow us to think at a higher level. When we
want to rename something, we can just do it. We don't have to execute several
different commands in our editor.

But we still work with text. And if refactoring support for an atomic operation
is not available, we are left with plain text to modify.

So what if code was not represented as text? Could we find another format that
more easily allow us to modify code at a higher level? Can we write functions
that perform edit operations on code?

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/M7EA1HCcPDN)
