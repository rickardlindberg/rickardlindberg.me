---
title: Search and replace in Vim
date: 2015-03-21
---

In this article I explain how I do search and replace in Vim.

As a programmer, I constantly rename variables, functions, and classes. The
easier it is to rename something, the more likely I am to do it. I think
renaming is probably one of the most important refactorings.

At the heart of my workflow is the substitute command. I'll show you how I
use it to do renames in a single file and then show you how I have extended it
to work across multiple files.

## Single file

The basic substitution pattern I use is the following:

    :%s/\<Foo\>/Bar/gc

* `%` makes the substitution work in the whole file.
* The brackets around `Foo` (`\<` and `\>`) ensure that it only matches `Foo`
  if it is not part of another word. For example, it matches `Foo`, but
  not `FooBar` or `GetFoo`. This is almost always what I want. Say I want to
  rename a function called `getCategory`, then I want to avoid renaming
  something called `getCategoryId`.
* `g` makes it replace all occurances on a line.
* `c` makes me confirm each substituation. I like that.

Ususally I want to rename the thing that is under the cursor. When I enter the
substitution command I can hit `<C-r><C-w>` to insert the word that is under
the cursor, like this:

    :%s/\<<C-r><C-w>\>/replacement/gc

This type of search and replace I do very often. I have a function that can
type this command for me:

    function! GetSubstituteCommand(range, term)
      return a:range . "s" . input(":s", "/\\<" . a:term . "\\>/" . a:term . "/gc\<C-f>F/F/l")
    endfunction

The call to `input` makes it possible for me to edit the substitution before I
proceed. Usually I just need to modify the replacement. The last bit in the
input `\<C-f>F/F/l` drops me into the command-line window and moves the cursor
to the first character of the replacement. That way I can quickly edit it. Say
I make the following call: `GetSubstituteCommand("%", "getCategory")`. The
command-line window shows the following with the cursor placed over `g` in the
replacement:

    /\<getCategory\>/getCategory/gc

Say I type `cwfetchCategory<CR>` (change word, type `fetchCategory`, hit
enter). The return value is then:

    %s/\<getCategory\>/fetchCategory/gc

I can execute that command and then I can say yes/no to all substitutions:

    function! SubstituteInFile(text)
        execute GetSubstituteCommand("%", a:text)
    endfunction

And bind it to a key like this where the word under the cursor is used as the
search text:

    map ,rw :call SubstituteInFile(expand("<cword>"))<CR>

That is the basics. Whenever I want to rename something in a file, I do the
following:

* Place the cursor somewhere on the word.
* Hit `,rw`.
* Edit the replacement.
* Hit enter.
* Say yes/no to all matched substitutions.

## Across multiple files

The problem with renames is that they are not always local to the file. If I
rename a function I also need to use the new name in all files where that
function is called.

Vim's substitute command does not work across files, so I use ack from within
Vim to find all matches. In my `.vimrc` I have this:

    set grepprg=ack

Then I can find all matches like this:

    :grep -w 'getCategory'

The `-w` flag is the equivalent of Vim's `\<` and `\>`.

For each line that matches, I can run the following substitute command:

    s/\<getCategory\>/fetchCategory/gc

Notice that I do not use the percent sign here. Now I want the substitute
command to run only on the line where ack found a match.

Then I can go to the next match and run the substitute command again and repeat
that process until all matches have been processed.

I have automated this process. Let's start with the grep command:

    function! GetGrepCommand(term)
      return "grep " . input(":grep ", "-w '" . a:term . "'\<C-f>F'F'l")
    endfunction

## Resouces

http://www.ibrahim-ahmed.com/2008/01/find-and-replace-in-multiple-files-in.html

http://vimcasts.org/episodes/project-wide-find-and-replace/

The latest version can be found here:
https://github.com/rickardlindberg/dotfiles/blob/master/.vim/vimrc_search_replace.vim
