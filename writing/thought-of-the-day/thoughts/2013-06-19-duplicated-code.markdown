---
title: Duplicated code
---

Today's thought is about duplicated code.

Duplicated code is seen as a bad thing that should be avoided at all cost. But is it sometimes ok to duplicate code?

In order to answer that question, I think it is important to make a distinction. A distinction between code that looks textually the same and code that duplicates logic. I believe only duplicated logic is a problem. Code that looks textually the same might be an indication of duplicated logic.

Let me show you what I mean with an example. Say you have a web page that displays article headers and the name of the authors who wrote them. You always want the first letter in the article header and the author name to be a capital. You might have code that looks something like this:

    article.getCapitalizedHeader()
    author.getCapitalizedName()

Those two functions might do the exact same thing in the implementation. So we should remove that duplication, right? Not necessarily. The logic to format an article header and the logic to format an author name are not related. They happen to be implemented in the same way, but it might not be so in the future. The rules might change for one, but not for the other. If the capitalizaion is complicated, you might want to put it in a function that both getCapitalizedHeader and getCapitalizedName can call.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/NhXPA4ezYyQ)
