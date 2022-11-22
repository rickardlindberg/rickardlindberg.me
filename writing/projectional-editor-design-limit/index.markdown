---
title: "DRAFT: I hit the limit of the design of my projectional editor"
date: 2022-11-20
tags: draft
---

I am writing this blog post to help me get unstuck.

I tried to implement the next thing in my projectional editor and I felt like I
had hit the limit of what the design was capable of.  There was no way I could
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

The power of a projectional editor comes from projections, and being able to
combine projections in various ways to easily create custom editors for
different data structures and scenarios.

The `Editor` above creates the status bar that you see in the screenshot on the
first line. The rest of the window is filled with the document that is passed
to the editor which in turn is created by `Split.project`. And so on.

## The next thing I want my editor to do

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

If the top split only shows lines matching a certain pattern, but are still
editable, you can see how this starts to differ from a plain text editor.

This could be useful in a search and replace scenario for example.  Instead of
scrolling through the document for matches, you could instead only show the
lines that match and make the edit there.

First, I started working on how to input the text to use in the filter. I came
up with this:

<center>
![New filter input field.](rlproject-filter.png)
</center>

The idea was that as a typed characters in the filter input field, the lines
that did not match would get excluded from the top split below.

When I made the modification to add the input field, I had to force it in. I
didn't find any clean way to do it using the current design. Forcing it in at
first is fine. But I couldn't find a way to refactor toward a better design
either. And I was unable to do the actual filtering part as well. I just
couldn't figure out how. I was stuck.

## A note on getting stuck

When I end up in a stuck situation like this, I wonder if I'm doing something
wrong.

Sometimes when I'm reading Agile literature, and maybe TDD literature in
particular, I get the feeling that it should always be possible to make
progress. Even if just a tiny step. And if you get stuck, it's because you
ignored earlier signs that you should have refactored or evolved your design in
a certain direction.

But I feel like you sometimes have to take a step backwards, modify the design,
and then move forward again. Being able to always move forward seems impossible
to me.

Well, now I am stuck, and I can't figure out a test to write to make even tiny
progress. So I am turning to my blog for advice and guidance. By writing about
the problem, perhaps I can get some clarity and get an idea for how to move
forward.

## A previous problem I had noticed

One problem that I had noticed before starting working on filtering lines (but
though was not significant) is partially seen in the creation of the editor:

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
documents.  The child documents are actually edited separately, but it looks
like it is the same document because they receive the same events and change in
the same way.

In most cases, it is probably not useful to have a split that forwards events
to all its children. It is probably mostly confusing. I thought that `Split`
was a useful way to test multiple projections for now, and that it could be
replaced with something better later on.

But I might have been wrong.

Perhaps there was something more fundamentally wrong with the design here.

## Trying to modify split

Say I wanted to modify the `Split` to only forward events to the top split and
have the bottom split be just another view. How would I do that?

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

What if we created a projection function like this:

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

But creating projections in this is not possible with the current design.

Let's look at the driver again to see why:

```
def on_char(self, evt):
    self.terminal = self.terminal.keyboard_event(KeyboardEvent(
        unicode_character=chr(evt.GetUnicodeKey())
    ))
    self.repaint_bitmap()
```

In this version, the terminal document returns a new version of itself in the
response of a key event. So there is no way to apply the new style projection
because it is embedded in the document.

What if we wrote the driver like this instead:

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

## Can projections hold state?

In the current design, there is a slight distinction between a document and a
projection, but they are a bit intertwined. Let's have a look.

Here is how a `Lines` document looks like:

```python
class Lines(
    namedtuple("Lines", "lines selections")
):

    def print_lines_selections(self):
        ...
```

It has a list of lines, a list of selections, and methods that operate on the
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

It is used something like this:

```python
terminal = LinesToTerminal.project(lines)
```

It projects a lines document to a terminal document. So the input to the
project function is a `Lines` document and the output is a `Terminal` document.

Except the project method returns an instance of itself, a `LinesToTerminal`.
What's going on?

Notice the second base class, `Projection`. It looks like this:

```python
class Projection:

    @property
    def fragments(self):
        return self.projection.fragments

    ...
```

It adds methods so that `LinesToTerminal` should behave like `Terminal`
document even though it is not. (It assumes that the `Terminal` document is
stored in the `projection` field.)

So why can't we just return a `Terminal` document?

Because the projection needs to hold some state. In this case it holds a copy
of the `Lines` document that was passed as input. It needs that to be able to
handle events appropriately.

In the proposed way forward, the state of projections are no longer present.

Having to have this wrapper `Projection` to make "projection objects" behave as
documents annoyed me, and blurred the line of what a document is and what a
projection is.

Then I thought about inverting this.

What if all documents had an extra field, called `meta` maybe, that projections
could use to store whatever they needed to appropriately handle events? That
would require all documents to have such a field, but then the wrapper would
not be needed and projections could just be pure functions converting one
document to another.

## Things to try

* Move projection state to documents
* Make events return a new stat structure of the source document

So now I have an idea of where I want to go that I think will solve some of my
problems.

How do I get there?

## Making changes in tiny steps

Changing how the drivers work seems like a big task that is hard to do in small
steps. But moving projection state to documents seems like something that could
quite easily be done.

There are currently 3 types of documents:

* String
* Lines
* Terminal

I decided to start with `String` to test out this idea.

### Adding meta argument

I went from

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

Immediately tests broke because I had not supplied the `meta` field anywhere.

I supplied `None` as a value in all cases, and now everything was back to
green.

Step 1 DONE: [Add `meta` argument to `String` in an attempt to move projection
state into the document.](2e4e32e7917313d4a3db01030d8e597ea8ddcb5d)

### Bad choice

There are no projections to `String`, currently.

So that was a bad choice.

I do the same change to `Lines`.

It turned out to be event easier since I had used a factory method:

Step 1b DONE: [Add `meta` argument to `Lines` in an attempt to move projection
state into the document.](d278896843ebda1c1203422b3a8ad0caf73a41d3)

### Using it in a projection

There is only one projection that projects to `Lines`, and that is
`StringToLines`. Let's see if we can modify it to store its state in the
`Lines` document instead of in the projection.

I ran into a problem where the key handler was no longer associated with the
projection.

Diff:

    -        return StringToLines(
    -            projection=Lines.create(
    -                lines=(line[0] for line in lines),
    -                selections=selections
    -            ),
    -            string=string,
    +        return Lines.create(
    +            lines=(line[0] for line in lines),
    +            selections=selections,
    +            meta=Meta(string=string)

Error:

    AttributeError: 'NotImplementedError' object has no attribute 'lines'

### Realisation

I realized that storing state in the document or in the projection wouldn't
matter. Different event handlers would need to be associated with different
projections.

However, I think storing state in documents provides a slightly cleaner design,
so I decide to go ahead with it anyway.

Even if I know it won't help solve my problem, it might be more clear and I
might learn something going through with it.

It turned out a lot cleaner.

git diff 1ab0ca6f57f33318fc87aa9c9913189cf08c99d3 df15b3f663855cd5e54c3b711e9a042afeee96fa

### New problem again

My first endeavor didn't really move me closer to the solution. Or it might
have, I'm not sure yet. But it made the code a little cleaner.

The next thing to work on is change the behavior of event handlers to return
the document being edited instead of a new version of itself.

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

Say that the document is a `String` read from a file. The `create_editor`
function is completely stateless, so all information about how to render this
document through a series of projections must be contained in the document. But
a `String` document has no information about the editor. But now with the
`meta` field, it might.

The editor itself might have state that is unrelated to the document. It might
for example be which projection to use to present the document. Imagine that
you can press a key to cycle through different projections. Which
projection to use is editor state.

I feel like this approach might work.

How to implement it in small steps?

I think I have to do it in parallel...
