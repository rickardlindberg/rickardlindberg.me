---
title: 'DevLog 003: Clarify GUI separation'
date: 2023-07-29
tags: devlog,rlvideo
devlog: true
---

In the [video editor](/projects/rlvideo/index.html), there is the idea that we
want to isolate the GTK code and have as few classes as possible depend on it.
However, this idea is not clearly expressed in the code. So new readers of the
code base will not necessarily understand that this separation is intentional
and something that we want to do.

In this episode I want to refactor the code to make that more clear.

## Current state

The current layout of the Python files looks like this:

$:output:text:
.
├── rlvideolib
│   ├── asciicanvas.py
│   ├── debug.py
│   ├── domain
│   │   ├── cut.py
│   │   ├── __init__.py
│   │   ├── project.py
│   │   ├── region.py
│   │   ├── section.py
│   │   └── source.py
│   ├── events.py
│   ├── graphics
│   │   ├── __init__.py
│   │   └── rectangle.py
│   ├── __init__.py
│   ├── jobs.py
│   └── testing.py
└── rlvideo.py
$:END

There is the "main" file `rlvideo.py` and the `rlvideolib` package.

The main file is sort of the default place where new things go that don't fit
anywhere else. It currently has a mix of classes with different areas of
responsibility:

$:output:python:
class FakeGui:
class GtkGui:
class MenuItem(namedtuple("MenuItem", "label,action")):
class App:
class MltPlayer:
class Timeline:
class Scrollbar(namedtuple("Scrollbar", "content_length,one_length_in_pixels,ui_size,content_desired_start")):
$:END

Some of these classes deal with GTK. Others with GUI code that does not depend
on GTK directly.

I would like to create a new `rlvideolib.gui` package that has three modules:

* gtk
* generic
* testing

## Testing

I extract a new testing module like this:

$:output:shell:
$ mkdir rlvideolib/gui
$ touch rlvideolib/gui/__init__.py
$ touch rlvideolib/gui/testing.py
$:END

Then I move the `FakeGui` class to that module and also rename it to `TestGui`
as I think that is a more descriptive name. I also make sure to import it from
`rlvideo.py`.

Let's commit:

$:output:shell:
$ ./make.py commit -m 'Extract rlvideolib.gui.testing.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.931s

OK
[main 91b63c2] Extract rlvideolib.gui.testing.
 4 files changed, 14 insertions(+), 12 deletions(-)
 create mode 100644 rlvideolib/gui/__init__.py
 create mode 100644 rlvideolib/gui/testing.py
$:END

## Generic

Let's do the same thing for generic GUI code:

$:output:shell:
$ touch rlvideolib/gui/generic.py
$:END

I move over the following classes:

$:output:python:
class Timeline:
class Scrollbar(namedtuple("Scrollbar", "content_length,one_length_in_pixels,ui_size,content_desired_start")):
class MenuItem(namedtuple("MenuItem", "label,action")):
$:END

If we look at the imports for the generic GUI module, we see this:

$:output:python:
from collections import namedtuple

import cairo
import mlt

from rlvideolib.debug import timeit
from rlvideolib.domain.project import Project
from rlvideolib.graphics.rectangle import RectangleMap
from rlvideolib.graphics.rectangle import Rectangle
from rlvideolib.events import Event
from rlvideolib.domain.region import Region
from rlvideolib.gui.testing import TestGui
from rlvideolib.domain.cut import Cut
$:END

We can see that it depends on Cairo. That is ok. The drawing of generic GUI
components is done with Cairo. I don't see as big a reason to abstract that
compared to GTK. Cairo can also most likely be used with other GUI frameworks.
The important thing here is that there is no import of GTK.

Let's commit this:

$:output:shell:
$ ./make.py commit -m 'Extract timelinelib.gui.generic.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.937s

OK
[main e392173] Extract timelinelib.gui.generic.
 3 files changed, 220 insertions(+), 819 deletions(-)
 rewrite rlvideo.py (64%)
 copy rlvideo.py => rlvideolib/gui/generic.py (65%)
$:END

We should probably also run the application to see that I didn't mess up
anything major. I get this:

$:output:text:
Traceback (most recent call last):
  File "/home/rick/rlvideo/rlvideo.py", line 213, in <module>
    App().run()
  File "/home/rick/rlvideo/rlvideo.py", line 156, in run
    self.timeline = Timeline(project=self.project, player=mlt_player)
NameError: name 'Timeline' is not defined
$:END

Woopsie.

I add the missing import to `rlvideo.py`:

$:output:python:
from rlvideolib.gui.generic import Timeline
$:END

If we want to catch this error in the test suite, we must run the whole
application which incudes starting the GTK main loop.

I'm thinking that we can separate the construction of the GUI from the actual
main loop, something like this:

$:output:python:
class App:

    def run(self):
        self.init()
        Gtk.main()

    ...
$:END

And then we can run `App().init()` in a test to make sure that construction of
the GUI works.

I try this, but run into all kinds of issues.

I decide to leave this as is for now.

## GUI

All classes that are left in `rlvideo.py` are now related to GTK. Let's move
them to its own module, leaving only this:

$:output:python:
from rlvideolib.gui.gtk import App

if __name__ == "__main__":
    App().run()
$:END

Let's commit:

$:output:text:
$ ./make.py commit -m 'Extract timelinelib.gui.gtk.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.941s

OK
[main c4702cc] Extract timelinelib.gui.gtk.
 3 files changed, 5 insertions(+), 217 deletions(-)
 rewrite rlvideo.py (99%)
 copy rlvideo.py => rlvideolib/gui/gtk.py (99%)
$:END

## Summary

The new structure looks like this:

$:output:python:
rlvideolib/gui/testing.py
1:class TestGui:

rlvideolib/gui/generic.py
17:class Timeline:
312:class Scrollbar(namedtuple("Scrollbar", "content_length,one_length_in_pixels,ui_size,content_desired_start")):
400:class MenuItem(namedtuple("MenuItem", "label,action")):

rlvideolib/gui/gtk.py
16:class GtkGui:
41:class App:
163:class MltPlayer:
$:END

I think this structure tells the reader that there is a clear separation
between GTK related GUI code and generic GUI code and that this separation is
intentional.

The `MltPlayer` that we see in the GTK module has only very little to do with
GTK. Most of it just works with an MLT producer. GTK is needed for the
embedding of the video display from the MLT consumer inside a GTK window.

I think part of `MltPlayer` should probably be extracted to a new class and be
put in the generic GUI module or somewhere else. This refactoring to separate
the different GUI modules revealed that to me. I find that is often the case.
You make one refactoring to clarify something and you discover something else
that is unclear.
