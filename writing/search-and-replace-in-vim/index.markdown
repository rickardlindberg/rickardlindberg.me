---
title: Search and replace in Vim
date: 2015-03-21
---

**This article is a draft.**

In this article I explain how I do search and replace in Vim.

As a programmer, I constantly rename variables, functions, and classes. The
easier it is to rename something, the more likely I am to do it. I think
renaming is probably one of the most important refactorings.

At the heart of my workflow is the substitute command. I'll show you how I
use it to do renames in a single file and then show you how I have extended it
to work across multiple files.

## In a single file

The basic substitute pattern I use is the following:

    :%s/\<Foo\>/Bar/gc

* `%` makes the substitute work in the whole file.
* The brackets around `Foo` (`\<` and `\>`) ensure that it only matches `Foo`
  if it is not part of another word. For example, it matches `Foo`, but
  not `FooBar` or `GetFoo`. This is almost always what I want. Say I want to
  rename a function called `getCategory`, then I want to avoid renaming
  something called `getCategoryId`.
* `g` makes it replace all occurances on a line.
* `c` makes me confirm each substituation. Textual search and replace is not
  100% accurate, and therefore I like to manually confirm each substitution.

Usually I want to rename the thing that is under the cursor. When I enter the
substitute command I can hit `<C-r><C-w>` to insert the word that is under
the cursor, like this:

    :%s/\<<C-r><C-w>\>/replacement/gc

This type of search and replace I do very often. I have automated the process
with Vim script. Let's start with the mapping:

    map ,rw :call SubstituteInFile(expand("<cword>"))<CR>

The first thing that happens when I hit `,rw` (rename word) is that
`expand("<cword>")` is evaluated. It returns the word that is under the cursor.
(In the same way that typing `<C-r><C-w>` does.) That word gets passed to
`SubstituteInFile`:

    function! SubstituteInFile(text)
        execute GetSubstituteCommand("%", a:text)
    endfunction

It calls `GetSubstituteCommand` with the `%` range and the passed in text. The
search command that is returned gets executed. This is the equivalent of typing
the following:

    :%s/\<Foo\>/Bar/gc

Here is `GetSubstituteCommand`:

    function! GetSubstituteCommand(range, term)
      return a:range . "s" . input(":s", "/\\<" . a:term . "\\>/" . a:term . "/gc\<C-f>F/F/l")
    endfunction

The call to `input` makes it possible for me to edit the substitute command
before I proceed. Usually I just need to modify the replacement. The last bit
in the input `\<C-f>F/F/l` drops me into the command-line window and moves the
cursor to the first character of the replacement. That way I can quickly edit
it. Say I make the following call: `GetSubstituteCommand("%", "getCategory")`.
The command-line window shows the following with the cursor placed over `g` in
the replacement:

    /\<getCategory\>/getCategory/gc

Say I type `cwfetchCategory<CR>` (change word, type `fetchCategory`, hit
enter). The return value is then:

    %s/\<getCategory\>/fetchCategory/gc

That's the basics. Whenever I want to rename something in a file, I do the
following:

* Place the cursor somewhere on the word I want to rename.
* Hit `,rw`.
* Edit the replacement and customize the substitute command if needed.
* Hit enter.
* Hit y/n to confirm each substitution.

## Across multiple files

The problem with renames is that they are not always local to the file. If I
rename a function I also need to use the new name in all files where that
function is called.

Vim's substitute command does not work across files, so I use
[ack](http://beyondgrep.com/) from within Vim to find all matches. In my
`.vimrc` I have this:

    set grepprg=ack

Then I find all matches like this:

    :grep -w 'getCategory'

The `-w` flag is the equivalent of Vim's `\<` and `\>`.

When grep have run, the cursor is placed on the line of the first match. Then I
run the following substitute command:

    s/\<getCategory\>/fetchCategory/gc

This is the same substitute command as previously, except there is no percent
sign here. Here I want the substitute command to work only on the line where
ack found a match, not on the whole file.

Then I go to the next match and run the substitute command again and repeat
that process until all matches have been processed.

This is of course a lot of manual work that can be automated with Vim script.
Let's start with the mapping:

    map ,mrw :call SubstituteInCodebase(expand("<cword>"))<CR>

When I hit `,mrw` (multiple rename word), the word under the cursor is passed
to `SubstituteInCodebase`:

    function! SubstituteInCodebase(text)
        let grepCommand = GetGrepCommand(a:text)
        let substituteCommand = GetSubstituteCommand("", a:text)
        execute grepCommand
        call QuickfixDo(substituteCommand . " | update")
    endfunction

The first thing that happens here is that I build the grep command. It looks
like this:

    function! GetGrepCommand(term)
      return "grep " . input(":grep ", "-w '" . a:term . "'\<C-f>F'F'l")
    endfunction

Similar to `GetSubstituteCommand` it drops me into command-line window so that
I can customize the search. Typical customizations that I do:

* Add `--python` or equivalent to only search in Python files.
* Add a directory to only do the substitution in certain directories.

After the grep command is created, the search command is created in the same
way as before. But notice the lack of the `%` range. After both commands have
been created, the grep command is executed. This is the equivalent of typing
the following:

    :grep -w 'getCategory'

Now the quickfix lis is populated with the search results and I can step
through it and do the substitution on each matched line. That is what
`QuickfixDo` is for: It will run an arbitrary command on each line in the
quickfix list. In this case I pass the search command plus the update command.
That ensures that I save the file if the substitution did any changes before I
move on to the next match. `QuickfixDo` looks like this:

    function! QuickfixDo(command)
        let itemCount = len(getqflist())
        let itemNr = 1
        while itemNr <= itemCount
            exe "cc " . itemNr
            exe a:command
            let itemNr = itemNr + 1
        endwhile
    endfunction

That's it. Whenever I want to rename something across files, I do the
following:

* Place the cursor somewhere on the word I want to rename.
* Hit `,mrw`.
* Customize the grep command if needed.
* Hit enter.
* Edit the replacement and customize the substitute command if needed.
* Hit enter.
* Hit y/n to confirm each substitution.

## Resouces

The latest version of my search and replace configuration can be found at
[vimrc_search_replace.vim](https://github.com/rickardlindberg/dotfiles/blob/master/.vim/vimrc_search_replace.vim).

Other articles on the subject of search and replace in Vim that I found
interesting:

* [Vimcasts - Project-wide find and replace](http://vimcasts.org/episodes/project-wide-find-and-replace/)

* [Ibrahim Ahmed - Vim: Find and replace text across files](http://www.ibrahim-ahmed.com/2008/01/find-and-replace-in-multiple-files-in.html)

* [The Geek Stuff - Vi and Vim Editor: 12 Powerful Find and Replace Examples](http://www.thegeekstuff.com/2009/04/vi-vim-editor-search-and-replace-examples/)
