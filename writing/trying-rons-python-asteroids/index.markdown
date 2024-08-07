---
title: Trying Ron's Python Asteroids
date: 2023-04-18
tags: python
---

[Ron](https://www.ronjeffries.com/) is working on an Asteroids game in Python
and also [writing](https://www.ronjeffries.com/categories/python/) about it.
I'm interested in his workflow, so I follow along.

He recently published the code on
[Github](https://github.com/RonJeffries/python-asteroids-1).

I thought I would have a look.

## First look

I clone the repo and see a bunch of Python files and an `.idea` folder.

I've never been a fan of IDEs. Perhaps I should learn one properly. In any case
I find it useful to be able to run commands from the command line as well.

First, I want to see if I can get this game running:

    $ python game.py
    AttributeError: 'pygame.math.Vector2' object has no attribute 'copy'

I suspect I'm using a different version of pygame that lacks the copy method on
vectors.

I try to run the test to see if I get the same failure there. How to run the
tests? I think Ron mentioned that he uses [pytets](https://pytest.org/). I try:

    $ pytest
    ===================================================================== test session starts =====================================================================
    platform linux -- Python 3.9.10, pytest-6.2.2, py-1.11.0, pluggy-0.13.1
    rootdir: /home/rick/downloads/python-asteroids-1
    collected 3 items / 3 errors
    ...

I see the same error about the copy method of vector and some more in the same
style.

I read about the copy method in the pygame manual and conclude that it was
added in a later version.

## Upgrade pygame

I think I've installed pygame via Fedora's package manager. That doesn't have a
more recent version of pygame.

I try to install it using pip instead:

    $ pip install --user pygame
    Requirement already satisfied: pygame in /usr/lib64/python3.9/site-packages (2.0.3)

I add `--user` because I don't want to install anything globally using pip. I
suppose I should create a virtual environment, but I haven't worked much with
them. This will do.

It indeed tells me that I already have pygame installed. How do I upgrade? Ah,
the `--upgrade` flag:

    $ pip install --user --upgrade pygame
    Requirement already satisfied: pygame in /usr/lib64/python3.9/site-packages (2.0.3)
    Collecting pygame
      Downloading pygame-2.3.0-cp39-cp39-manylinux_2_17_x86_64.manylinux2014_x86_64.whl (13.8 MB)
         |████████████████████████████████| 13.8 MB 692 kB/s 
    Installing collected packages: pygame
    Successfully installed pygame-2.3.0

Sometimes I hesitate to install Python packages via pip. Especially when
they are not pure Python packages (like pygame which depends on SDL and C
libraries). Mostly because it hasn't worked so well for me in the past. Maybe
things are better now. And maybe it depends on the library. Let's see how
this works now.

## Testing the game again

I try running the game again:

    $ python game.py 
    pygame 2.3.0 (SDL 2.24.2, Python 3.9.10)
    Hello from the pygame community. https://www.pygame.org/contribute.html

Success! Or, I don't get any errors at least. But it exits right away. Am I
running the wrong file?

Ah, there is a `main.py`. Let's try that.

It works!

<center>
![Ron's Asteroids game.](game.png)
</center>

## Zero friction

I'm quite familiar with both Python and pygame, so it was not that difficult
for me to get started. But I think we can improve.

One idea that I got from James Shore's writing about a [zero
friction](https://www.jamesshore.com/v2/books/aoad2/zero_friction)
development is that you should have scripts for doing common tasks
like running your tests.

Let's [see](https://github.com/RonJeffries/python-asteroids-1/pull/1) if Ron
likes that as well. I add one script to test (`build.sh`)

```bash
#!/usr/bin/env bash

set -e

pytest
```

and one to run the application (`rundev.sh`)

```bash
#!/usr/bin/env bash

exec python main.py
```

Should the way to run tests or the application change, only those files need to
be changed, and the usage of the developer stays the same.
