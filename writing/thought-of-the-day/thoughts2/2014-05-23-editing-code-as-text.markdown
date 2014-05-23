---
title: Editing code as text
---

I remember thinking many years ago that text is a clumsy way to work with code.
Why do we operate at the level of characters when we are editing code that has
a more well defined structure?

Automatic refactoring tools raise the level a bit. With those, we can think in
terms of inline variable and have that be a single operation in our editor.
There is no gap between what we want to do and how we do it. If we are just
working with text, inline variable involves many steps, some of which are
delete occurrence of variable, copy and paste the assigned value to those
places, delete the declaration.

But refactorings limit us to the catalogue implemented by the tool. The other
operations we want to do, we have to do by editing at the character level.

Yesterday I saw an Emacs plugin that let you modify Lisp expressions at a
higher level. It was a plugin for modifying the AST directly. (Or at least it
felt like that.) It feels like an AST editor could be more powerful than a text
editor. Perhaps it would be easy to define our own editing commands/shortcuts
in terms of operations to an AST?

--

* [Comments on Google+](https://plus.google.com/112175093836850283531/posts/SmVdBLsUaUB)
