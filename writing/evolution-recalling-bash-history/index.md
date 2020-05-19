---
title: Evolution of recalling Bash history
date: 2017-05-19
tags: rlselect,favourite
---

This article is about how I've become more efficient at using Bash, the
interactive UNIX shell.

When I work in Bash, I often want to execute a command again. In the beginning
I re-typed the command and pressed enter. This worked fine for short commands,
but became tedious for longer commands.

In some shells this is the only way to enter a new command. But Bash remembers
the recently executed commands and provides ways to recall them.

## Cycle with arrow keys

The first way I learned to recall history was with the arrow keys. If I pressed
<kbd>Up</kbd> the previous command was inserted at the prompt. I could continue
for as long as I wanted. If I pressed <kbd>Down</kbd> the next command was
inserted at the prompt:

    $ ls<Enter>
    bin         ...

    $ date<Enter>
    Wed May 10 08:14:46 CEST 2017

    $ <Up>

    $ date<Up>

    $ ls<Down>

    $ date<Enter>
    Wed May 10 08:14:59 CEST 2017

This worked fine for commands that I had executed recently, but tedious for
commands that I had executed long ago because I had to press <kbd>Up</kbd> many
times.  I ended up pressing and holding <kbd>Up</kbd> so that history scrolled
by and when I saw my command, I released the key and pressed <kbd>Down</kbd>
until it appeared again.

## Cycle with Ctrl-P/Ctrl-N

Later I learned that <kbd>Ctrl-P</kbd> (previous) had the same function as
<kbd>Up</kbd> and that <kbd>Ctrl-N</kbd> (next) had the same function as
<kbd>Down</kbd>.

These shortcuts were more comfortable for me because I like to keep my fingers
as close to the home row as possible.

## Searching with Ctrl-R

Then I learned about Bash's interactive history search command. If I pressed
<kbd>Ctrl-R</kbd> the prompt changed to this:

    (reverse-i-search)`':

This special prompt allowed me to type parts of a command that I had executed
previously. Say I wanted to execute the last find command again. I typed
"find" and the prompt changed to this:

    (reverse-i-search)`find': find -name '*.py' -a -type f

The text I typed, "find", was present before the colon. After the colon the
last command that I had executed that contained the string "find" was
displayed. In this case I did a search for Python files. If this was not the
match I was looking for, I could hit <kbd>Ctrl-R</kbd> again and the part to
the right of the colon would change to the next command in the history that
contained the string "find". Once I found the command I was looking for I had
two options: I could hit <kbd>Tab</kbd> to insert the command at the prompt:

    $ find -name '*.py' -a -type f

This way I could edit the command before I executed it. Or I could hit
<kbd>Enter</kbd> to execute the command directly.

Now I was able to recall commands that I had executed long ago. I almost
replaced all my usages of <kbd>Ctrl-P</kbd>/<kbd>Ctrl-N</kbd> with
<kbd>Ctrl-R</kbd>. Except for the cases where I knew that the command I wanted
to recall was only a few entries back.

## Frustrations with Ctrl-R

The interactive search worked great for me when I knew what I was looking for.
It did not work so great when I was more uncertain or when I mistyped the name
of a command.

The interactive search works by having a pointer to en entry in the history.
When I typed a command it would move that pointer to the next item in the
history that matched. But if I mistyped, the search might still match something
further back in history. But when I erased a few characters to correct my
mistake, the search would continue from there. Say this was my history:

1. `tac ~/.bash_history`
2. `echo frustration`
3. `echo with`
4. `echo bash`

I hit <kbd>Ctrl-R</kbd> to to begin searching for "bash":

    (reverse-i-search)`':

But I mistyped. Instead of "b" I typed "f":

    (reverse-i-search)`f': echo frustration

The search matched item 2. I erased the incorrectly typed character:

    (reverse-i-search)`': echo frustration

The match remained. I typed bash correctly:

    (reverse-i-search)`bash': tac ~/.bash_history

It now matched item 1 instead of item 4. The search continued from the previous
match. I would have wanted the search to always show the most recent match from
history. The easiest way I found to reset the search after a failure to find
what I was looking for was to just execute a dummy command. Usually I selected
`ls` because it was short to type and had no side effects.

## Interactively filtering with external program

Then I was introduced to [hstr](https://github.com/dvorka/hstr) by a colleague.
It worked like a replacement for <kbd>Ctrl-R</kbd>. When I invoked it, it
dropped into a text UI where my last history entries were shown. I could
also type part of a command to narrow down the list. If I changed the search
string, the narrowed down list changed accordingly.  When I found a match I
could similarly press <kbd>Tab</kbd> to insert the command at the prompt or
press <kbd>Enter</kbd> to execute it immediately. It looked like this:

[![Demo of hstr (from their website)](/writing/evolution-recalling-bash-history/hh-animated-01.gif)](https://github.com/dvorka/hstr)

This solved my frustrations with Bash's interactive search. For me, this was a
far easier way to find items from my history. The fact that it showed the last
commands also helped me. I could visually inspect them, and they would guide my
search.

hstr was so good that I wanted to use a similar selection mechanism for other
things, but hstr was only for Bash history. I ended up writing my own selection
program: [rlselect](/projects/rlselect/index.html). Partly because I wanted
such a program, but also because it seemed like a fun program to write. The
core selection program is called `rlselect` and then there are multiple
programs that use it to allow selecting specific things. `rlselect-history` is
a replacement for <kbd>Ctrl-R</kbd>/hstr:

[![Demo of rlselect](/writing/evolution-recalling-bash-history/rlselect_history_demo.gif)](/projects/rlselect/index.html)

There are some differences between hstr and `rlselect-history`. I took only the
parts I personally wanted from hstr and put them into `rlselect-history`.

If you want to improve your Bash usage, I suggest taking a look at
[hstr](https://github.com/dvorka/hstr) or
[rlselect](/projects/rlselect/index.html).
