---
title: Functional core
---

I continued reading about Erlang this weekend. I read about the sequential part
which is functional. It feels like Erlang programs are structured as small
functional processes that communicate with message passing. I have to read more
about the concurrent part of Erlang to know for sure though.

This feels similar to an approach I read about (I forgot where) when writing
classes in object oriented programming: have all private methods be free of
side effects. So each class might have a small amount of state that is only
modified at the top level, but all the private "helper" methods would be purely
functional.

Another thing it feels similar to is [functional core imperative shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell)
by Gary Bernhardt.

Because Erlang is a functional language, it is probably easier to enforce the
functional part of such a design. It feels like Erlang is a good fit for
writing programs the way some people like to write object oriented programs.
Which is a bit strange.

--

* [Comments on Google+](https://plus.google.com/112175093836850283531/posts/apLDPdPJbHe)
