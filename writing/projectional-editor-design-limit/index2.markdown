---
title: "DRAFT: How should I evolve the design of my projectional editor?"
date: 2022-11-23
tags: rlproject,draft
---

I am writing this blog post to help me get unstuck.

I tried to implement the next thing in my projectional editor and I felt like I
had hit the limit of what the design was capable of. There was no way I could
add this next thing.

I had to modify the design.

The problem, though, was that I was not able to figure out *how* to modify the
design. I didn't even know how to move it in the right direction.

So, now I'm writing this blog post to help me find a solution.

## Background

I am building a [projectional
editor](https://github.com/rickardlindberg/rlproject). Currently it looks like
this:

<center>
![My projectional editor.](rlproject-base.png)
</center>

A projectional editor differs from a text editor in that it can project the
data structure being edited in different ways. It is not limited to syntax
highlighted lines of text for example.

In the screenshot above, you see the same document being projected in two
different ways. The first one as multiple lines, and the second one as a
single-line string.

The whole editor is created like this:

```python
Editor.project(
    Split.project([
        ClipScroll.project(
            LinesToTerminal.project(
                StringToLines.project(
                    String.from_file(path)
                )
            ),
        ),
        ClipScroll.project(
            StringToTerminal.project(
                String.from_file(path)
            ),
        ),
    ])
)
```

The `Editor.project` creates a document that the user can interact with via
the GUI. So when the user presses a key on the keyboard, for example, the
document responds by modifying itself and returning a new version of itself
that the GUI can then render.

The driver for this in the GUI is implemented like this:

```python
def on_char(self, evt):
    self.terminal = self.terminal.keyboard_event(KeyboardEvent(
        unicode_character=chr(evt.GetUnicodeKey())
    ))
    self.repaint_bitmap()
```

The document is called `self.terminal` here because `Editor.project` creates a
document of type `Terminal`. It contains fragments at given positions that
could be displayed in a terminal-like GUI.

The drawing in the GUI looks like this:

```python
for fragment in self.terminal.fragments:
    ...
    memdc.DrawText(fragment.text, fragment.x*char_width, fragment.y*char_height)
```

The `Editor` draws the status bar that you see in the screenshot on the first
line. The rest of the window is filled with the document that is passed to the
editor which in turn is created by `Split.project`. And so on.

## The next thing I wanted my editor to do

The power of a projectional editor comes from projections, and being able to
combine projections in various ways to easily create custom editors for
different data structures and scenarios.

The next thing I wanted to try in my editor was to filter the lines in the top
split.

Put in other words, I wanted to insert a filter projection in the chain of
projections. Something like this (`Filter.project` added):

```python
Editor.project(
    Split.project([
        ClipScroll.project(
            LinesToTerminal.project(
                Filter.project(
                    StringToLines.project(
                        String.from_file(path)
                    )
                )
            ),
        ),
        ClipScroll.project(
            StringToTerminal.project(
                String.from_file(path)
            ),
        ),
    ])
)
```

This could be useful in a search and replace scenario for example. Instead of
scrolling through the document for matches, you could instead only show the
lines that match and make the edit right there. And now you can see how this
starts to differ from a plain text editor.

I started working on how to input the text to use in the filter. I came up with
this:

<center>
![New filter input field.](rlproject-filter.png)
</center>

The idea was that as a typed characters in the filter input field, the lines
that did not match would get excluded from the top split.

When I made the modification to add the input field, I had to force it in. I
didn't find any clean way to do it using the current design. Forcing it in at
first is fine. But I couldn't find a way to refactor toward a better design
either. Furthermore, I was unable to do the actual filtering part as well. I
just couldn't figure out how. I was stuck.

## A note on getting stuck

When I end up in a stuck situation like this, I wonder if I'm doing something
wrong.

Sometimes when I'm reading Agile literature, and maybe TDD literature in
particular, I get the feeling that it should always be possible to make
progress. Even if just a tiny step. And if you get stuck, it's because you
ignored earlier signs that you should have refactored or evolved your design in
a certain direction.

But I feel like you sometimes have to take a step backwards, rethink the
design, and then move forward again. Being able to always move forward seems
impossible to me.

Well, now I am stuck, and I can't figure out a test to write to make even tiny
progress. So I am turning to my blog for advice and guidance. By writing about
the problem, perhaps I can get some clarity and get an idea for how to move
forward.

## A previous problem I had noticed

One problem that I had noticed before starting working on filtering lines, but
thought was not significant, is partially seen in the creation of the editor:

```python
Editor.project(
    Split.project([
        ClipScroll.project(
            LinesToTerminal.project(
                StringToLines.project(
                    String.from_file(path)
                )
            ),
        ),
        ClipScroll.project(
            StringToTerminal.project(
                String.from_file(path)
            ),
        ),
    ])
)
```

The idea is to create an editor for a file at the given `path`. This editor
provides two projections of this file (one with lines, and one as a string).
But the idea is to edit a single file. The different projections can give
different views of the data structure, but the underlying data structure is the
same.

Except, in this case it isn't. You can see that `String.from_file(path)` is
called twice. And the `Split` projection forwards key events to both child
documents. The child documents are actually edited separately, but it looks
like it is the same document because they receive the same events and change in
the same way.

In most cases, it is probably not useful to have a split that forwards events
to all its children. It is probably mostly confusing. I thought that `Split`
would be temporarily useful as way to test multiple projections, and that it
could be replaced with something better later on.

But I might have been wrong.

Perhaps there was something more fundamentally wrong with the design here.

## Making the split work sensible

Say we want to modify the `Split` to only forward events to the top split and
have the bottom split be just another view. How would we do that?

First of all, there can be only one document, so something like this:

```python
document = String.from_file(path)
Editor.project(
    Split.project([
        ClipScroll.project(
            LinesToTerminal.project(
                StringToLines.project(
                    document
                )
            ),
        ),
        ClipScroll.project(
            StringToTerminal.project(
                document
            ),
        ),
    ])
)
```

But this doesn't help at all. It is because all documents in this design are
immutable. So even if we modify `Split` to only pass events along to the top
split, when the second split renders, it will do so with the original version
of the document.

What if we create a projection function like this:

```python
def create_editor(document):
    Editor.project(
        Split.project([
            ClipScroll.project(
                LinesToTerminal.project(
                    StringToLines.project(
                        document
                    )
                ),
            ),
            ClipScroll.project(
                StringToTerminal.project(
                    document
                ),
            ),
        ])
    )
```

That way, the same document would always be used.

But creating projections in this way is not possible with the current design.

Let's look at the driver again to see why:

```
def on_char(self, evt):
    self.terminal = self.terminal.keyboard_event(KeyboardEvent(
        unicode_character=chr(evt.GetUnicodeKey())
    ))
    self.repaint_bitmap()
```

In this version, the terminal document returns a new version of itself in the
response to a key event. So there is no way to apply the new style projection
because it is currently embedded in the document.

What if we write the driver like this instead:

```
def on_char(self, evt):
    new_document = self.terminal.keyboard_event(KeyboardEvent(
        unicode_character=chr(evt.GetUnicodeKey())
    ))
    self.terminal = create_editor(new_document)
    self.repaint_bitmap()
```

In this design, we need to supply two things to the driver

1. The document to edit
2. A projection function that projects the document to a terminal document

In this design, we probably could modify `Split` to behave as we wanted.

Can we do it?

## How do projections hold state?

In the current design, there is a slight distinction between a document and a
projection, but they are related. Let's have a look.

Here is what a `Terminal` document looks like:

```python
class Terminal(
    namedtuple("Terminal", "fragments cursors"),
):

    def print_fragments_and_cursors(self):
        ...
```

It has a list of fragments, a list of cursors, and methods that operate on the
document.

Now, let's look at a projection from lines to terminal:

```python
class LinesToTerminal(
    namedtuple("LinesToTerminal", "projection lines"),
    Projection
):

    @staticmethod
    def project(lines):
        ...
        return LinesToTerminalText(
            ...
        )
```

It has a projection (the terminal document), a lines document (which was used
as input), and a `project` function.

It is used something like this:

```python
terminal = LinesToTerminal.project(lines)
```

It projects a lines document to a terminal document. So the input to the
`project` function is a `Lines` document and the output is a `Terminal`
document.

Except the `project` function returns an instance of itself, a
`LinesToTerminal`.  What's going on?

Notice the second base class, `Projection`. It looks like this:

```python
class Projection:

    @property
    def fragments(self):
        return self.projection.fragments

    ...
```

It adds methods to `LinesToTerminal` so that it behaves like `Terminal`
document even though it is not. (It assumes that the `Terminal` document is
stored in the `projection` field.)

So why can't we just return a `Terminal` document?

Because the projection needs to hold some state. In this case it holds a copy
of the `Lines` document that was passed as input. It needs that to be able to
handle events appropriately. (I think this state is what
[ProjecturEd](https://github.com/projectured/projectured/wiki/Projection) calls
an IO map.)

Needing this wrapper `Projection` to make "projection objects" behave as
document objects annoys me.

Can we invert it?  What if all documents had an extra field, called `meta`
maybe, that projections could use to store whatever they need to appropriately
handle events? That would require all documents to have such a field, but then
the wrapper would not be needed and code would be a bit more clean.

## Ideas to move forward with

Writing this blog post has yielded some results.

Now I have two ideas to move forward with:

1. Move projection state to documents
2. Change event driver to make events return a new version of the source
   document

How can I make tiny progress to any of the two ideas?

Changing how the driver work seems like a big task that is hard to do in small
steps. But moving projection state to documents seems like something that could
quite easily be done.

## Move projection state into documents

There are currently 3 types of documents:

* String
* Lines
* Terminal

I decide to start with `String` to test out this idea. I change

```python
class String(
    namedtuple("String", "string selections")
):
```

to

```python
class String(
    namedtuple("String", "meta string selections")
):
```

Immediately tests brake because I have not supplied the `meta` field anywhere.
I supply `None` as a value in all cases, and now everything is back to
green.

Unfortunately, there are no projections to `String`, so there are no
projections that would have use for this new `meta` field.

I do the same change to `Lines` instead.  It turns out to be even easier since
I already had factory method and only needed to modify that.

There is only one projection that projects to `Lines`, and that is
`StringToLines`. I modify it to store its state in `meta` field of the `Lines`
document instead of in the projection by changing this

```pyton
class StringToLines(
    namedtuple("StringToLines", "projection string"),
    Projection
)

    ...
    @staticmethod
    def project(string):
        return StringToLines(
            projection=Lines.create(
                lines=(line[0] for line in lines),
                selections=selections
            ),
            string=string,
        )
```

to this

```pyton
class StringToLines(Lines):

    ...
    @staticmethod
    def project(string):
        return StringToLines.create(
            lines=(line[0] for line in lines),
            selections=selections,
            meta=Meta(string=string)
        )
```

The `Meta` class is defined like this:

```pyton
class Meta(
    namedtuple("Meta", "string")
):
    pass
```

Notice the change in base class. A `StringToLines` projection is now of type
Lines. It *is* a `Lines` document. It does not need to add wrappers to act like
one.

Why return a `StringToLines` at all? Why not just return `Lines`?  Because it
needs to override method to handle events. Projections need to implement unique
event handles because they handle events differently. But the data the event
handlers need to properly handle events are now stored in the `meta` field of
the document.

I realized that storing state in the document or in the projection wouldn't
matter. Different event handlers would need to be associated with different
projections.

I'm not sure we are much closer to solving the problem of a sane split view,
but I think storing state in documents provides a slightly cleaner design
([complete
diff](https://github.com/rickardlindberg/rlproject/compare/1ab0ca6f57f33318fc87aa9c9913189cf08c99d3...df15b3f663855cd5e54c3b711e9a042afeee96fa),
so it should help us thing a bit more clearly.

## New problem again

The next thing to work on is change the behavior of event handlers to return
the document being edited instead of a new version of itself. Then call a
project function on this new document to render it.

The new `meta` field might come in handy here.

Let's explore.

So in our target design, the driver looks like this:

```
def on_char(self, evt):
    new_document = self.terminal.keyboard_event(KeyboardEvent(
        unicode_character=chr(evt.GetUnicodeKey())
    ))
    self.terminal = create_editor(new_document)
    self.repaint_bitmap()
```

Say that the document is a `String` and that it is read from a file. The
`create_editor` function is completely stateless, so all information about how
to render this document through a series of projections must be contained in
the document. But a `String` document has no information about the editor. And
the editor would for sure need some state unrelated to the document. One such
state would be if the filter input dialog should be shown or not. Another might
be which projection to use. Imagine that you can press a key to cycle through
different projections for example. The one chosen must be stored somewhere.

The new `meta` field might come in handy here.

## How long is this going to take?

I feel like I've done a lot of work, and I'm still not confident that this is a
good way forward. I feel like I keep trying things and just run into new
problems.

I'm trying to think ahead 10 steps to see if the new design will serve all the
things that I potentially want to do. I end up just thinking without doing
anything. Perhaps I shouldn't. Perhaps I should just focus on the next thing,
and then, eventually, I will have reached my end goal. I guess that is the TDD
way of doing it. One tiny test at a time. At least now, I have a direction to
try.

But I'm frustrated that I can't clearly see how this new design will solve even
my immediate problems.

But my confidence is starting to grow that this is a promising direction.

## Doing the switch...

How to implement it in small steps?

I think I have to do it in parallel...

commit af8a0513a1cf14de372748d51d6bfe21009047fd
    First tiny step towards new driver/event interface.

commit 2584a428a6bf481df924234c59bf98bd36d864e5 (HEAD -> main)
    Test drive new_size_event. Make sure correct type is returned.

Maybe this is test driving. Just one dumb test after another. Just learn what
you need, one tiny test at a time.

Am I learning the design aspect of TDD just now?!

Maybe not according to jbrains.
https://mastodon.social/@jbrains/109406661608224141

Maybe I just sneaked up on a solution slowly.

Was able to implement parallel solution and both write tests for it and try it
out in the GUI. Fleshed out one detail after another.

Then I felt that this would actually work out nicely.

## Cleaning up

Now I had two parallel versions.

The new one worked almost as good as the old, except

* popup
* ms

The popup never really worked in the old version. And was also the main reason
this design change started. I can see how it can be better implemented in this
new design, so I think I can just remove that functionality for now and do it
again in the new design.

Or now, I should try...

ms worked so so before. Dind't show complete truth. Better solution is needed.

## Was a blog post necessary?

The blog post forced me to explain things so that it would be clear to someone
else. It forced me to think clearly.

I don't think I could have done that using TDD only.

Some kind of prose or diagram would have been needed. Does that have a place in
the code base? Most likely not. I'm explaining the current state (with all its
flaws) so that I can improve it.

Would a code base benefit from having blog posts and discussions attached to
it? So you can see how it has evolved? Maybe.

Making systems explainable is also something that [GToolkit is working
on](https://gtoolkit.com/usecases/software-assessment/).

These things popped into my mind as I was writing this blog post.

Writing helps in thinking. The process works. The end result might be
discarded.
