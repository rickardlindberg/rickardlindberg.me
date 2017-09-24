---
title: Search and replace in Vim
date: 2015-03-28
---

*24 September 2017: Fixed broken link to "Vim: Find and replace text across files."*

In this article I explain how I do search and replace in Vim.

As a programmer, I constantly rename variables, functions, and classes. The
easier it is to rename something, the more likely I am to do it. I think
renaming is probably one of the most important refactorings.

The advantage of being able to do it with Vim is that it is language
independent. If I don't have an IDE or a refactoring tool to help me, I can
always use Vim.

At the heart of my workflow is the substitute command. I'll show you how I
use it to do renames in a single file and then show you how I have extended it
to work across multiple files.

## In a single file

When I want to rename something in a single file, I do the following:

* Place the cursor somewhere on the word I want to rename.
* Hit `,rw` (rename word).
* Customize the pre-entered substitute command if needed. Usually I just need
  to edit the replacement.
* Hit enter.
* Hit y/n to confirm each substitution.

Now I will explain how I have built this workflow. Let me start with the
mapping:

    map ,rw :call SubstituteInFile(expand("<cword>"))<CR>

The first thing that happens when I hit `,rw` is that `expand("<cword>")` is
evaluated. It returns the word that is under the cursor. That word gets passed
to `SubstituteInFile`:

    function! SubstituteInFile(text)
        execute GetSubstituteCommand("%", a:text)
    endfunction

It calls `GetSubstituteCommand` with the `%` range and the passed in text. The
search command that is returned gets executed. Here is `GetSubstituteCommand`:

    function! GetSubstituteCommand(range, term)
      return a:range . "s" . input(":s", "/\\<" . a:term . "\\>/" . a:term . "/gc\<C-f>F/F/l")
    endfunction

The call to `input` makes it possible for me to edit the substitute command
before I proceed. The last bit in the input `\<C-f>F/F/l` drops me into the
command-line window and moves the cursor to the first character of the
replacement. That way I can quickly edit it. Say I make the following call:
`GetSubstituteCommand("%", "getCategory")`. The command-line window shows the
following with the cursor placed over `g` in the replacement:

    /\<getCategory\>/getCategory/gc

Say I type `cwfetchCategory<CR>` (change word, type `fetchCategory`, hit
enter). The return value is the following:

    %s/\<getCategory\>/fetchCategory/gc

Running execute on that command is the equivalent of typing the following:

    :%s/\<getCategory\>/fetchCategory/gc

Let me explain how this substitute command works:

* `%` makes the substitute work in the whole file.
* The brackets around `getCategory` (`\<` and `\>`) ensure that it only matches
  `getCategory` if it is not part of another word. For example, it matches
  `getCategory`, but not `getCategoryId` or `ungetCategory`. This is almost
  always what I want.
* `g` makes it replace all occurrences on a line. Not just the first.
* `c` makes me confirm each substitution. Textual search and replace is not
  100% accurate, and therefore I like to manually confirm each substitution.

So the Vim script I have written for renames in a single file is basically just
to help me type the substitute command I use most often faster.

## Across multiple files

The problem with renames is that they are not always local to a file. If I
rename a function I also need to use the new name in all files where that
function is called. My workflow involves both the grep command and the
substitute command. It looks like this:

* Place the cursor somewhere on the word I want to rename.
* Hit `,mrw` (multiple rename word).
* Customize the pre-entered grep command if needed.
* Hit enter.
* Customize the pre-entered substitute command if needed. Usually I just need
  to edit the replacement.
* Hit enter.
* Hit y/n to confirm each substitution.

Now I will explain how I have built this workflow. Let me start with the
mapping:

    map ,mrw :call SubstituteInCodebase(expand("<cword>"))<CR>

When I hit `,mrw`, the word under the cursor is passed to
`SubstituteInCodebase`:

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
I can customize the grep command. Say I make the following call:
`GetGrepCommand("getCategory")`. The command-line window shows the following
with the cursor placed over `g`:

    -w 'getCategory'

Say I just hit enter here. The return value is then the following:

    grep -w 'getCategory'

Running execute on that command is the equivalent of typing the following:

    :grep -w 'getCategory'

I use [ack](http://beyondgrep.com/) as my grep program. In my `.vimrc` I have
this:

    set grepprg=ack

The `-w` flag is the equivalent of Vim's `\<` and `\>`.

Typical customizations that I do:

* Add `--python` or equivalent to only search in Python files.
* Add a directory to only do the search in certain directories.

After the grep command is created, the search command is created in the same
way as before. But notice the lack of the `%` range. That is because now I only
want the substitute command to operate on a single line, and not the whole
file. After both commands have been created, the grep command is executed.

Now the quickfix list is populated with the search results and I can step
through it and do the substitution on each matched line. That is what
`QuickfixDo` is for: It will run an arbitrary command on each line in the
quickfix list. In this case I pass the substitute command (without the `%`
range) plus the update command.  That ensures that I save the file if the
substitution did any changes before I move on to the next match. `QuickfixDo`
looks like this:

    function! QuickfixDo(command)
        let itemCount = len(getqflist())
        let itemNr = 1
        while itemNr <= itemCount
            exe "cc " . itemNr
            exe a:command
            let itemNr = itemNr + 1
        endwhile
    endfunction

The workflow for renames across multiple files contains only an extra grep step
compared to the single file workflow. The defaults ensure that the matches
found by the grep command are also found by the substitute command.

## Further reading

The latest version of my search and replace configuration can be found at
[vimrc_search_replace.vim](https://github.com/rickardlindberg/dotfiles/blob/master/.vim/vimrc_search_replace.vim).

Other articles on the subject of search and replace in Vim that I found
interesting:

* [Vimcasts - Project-wide find and replace](http://vimcasts.org/episodes/project-wide-find-and-replace/)

* [Ibrahim Ahmed - Vim: Find and replace text across files](https://web.archive.org/web/20150928211530/http://www.ibrahim-ahmed.com/2008/01/find-and-replace-in-multiple-files-in.html)

* [The Geek Stuff - Vi and Vim Editor: 12 Powerful Find and Replace Examples](http://www.thegeekstuff.com/2009/04/vi-vim-editor-search-and-replace-examples/)
