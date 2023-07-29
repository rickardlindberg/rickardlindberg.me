---
title: 'DevLog 002: Change mix strategy for cuts in GUI'
date: 2023-07-29
tags: devlog,rlvideo
devlog: true
---

In the [previous devlog](/writing/devlog-001-jcut-lcut/index.html) we worked on
adding the concept of a cut type to a clip in the [video
editor](/projects/rlvideo/index.html). That is, how should two overlapping
clips be mixed together? Which one should be on top?  Should it be a
[J-cut](https://en.wikipedia.org/wiki/J_cut)?

I've since added support for two types of cuts: over and under. So we can
(programmatically) set this property on clips and they will render in the
correct order.

The default cut is under so that later clips will be mixed under the previous
clips:

<p>
<center>
![Default under cut.](under.png)
</center>
</p>

There is not yet a way to change this default from the GUI, so that's what we
will work on in this episode.

## Aside: clips and cuts

When writing about this and when looking at the source code, I'm a bit confused
by the terminology. I write about clips och cuts, but in the source code there
is no concept of a clip, only a cut and a source.

A source represents a file on disk or some generator of frames. A cut
represents a region of a source.

A cut has a property called `cut` which is how to mix this cut with the
previous cut on the timeline. That confuses things further.

Let's rename it to `mix_strategy` to lessen the confusion.

$:output:shell:
$ ./make.py commit -m 'Rename Cut.cut to Cut.mix_strategy.'
...............................................
----------------------------------------------------------------------
Ran 47 tests in 1.918s

OK
[main 425909a] Rename Cut.cut to Cut.mix_strategy.
 1 file changed, 11 insertions(+), 11 deletions(-)
$:END

That's better.

## Two possible ways

I can think of two possible ways to set the mix strategy for a cut in the GUI.
Either we can right click on a cut and have a context menu pop up where we can
select the mix strategy. Or we can select a clip and have a "set mix strategy"
operation applied to the selected clip.

Currently, there is no concept of a selected clip. You can't select anything.
But there is a concept of clicking and dragging a clip. Therefore I think a
context menu is easier to get started with.

## Reviewing mouse click code

Let's look at how click and drag is handled today.

I see this GTK event is connected:

$:output:python:
timeline.connect("button-press-event", timeline_button)
$:END

And `timeline_button` is defined like this:

$:output:python:
def timeline_button(widget, event):
    self.timeline.mouse_down(*timeline.translate_coordinates(
        main_window,
        event.x,
        event.y
    ))
$:END

This code does not seem to distinguish between left and right mouse button
click. Interesting.

Does that mean that we can move a cut on the timeline by clicking and dragging
with the right mouse button? I try it in the application. And it indeed works.
That was really not my intention. Let's see if we can fix that.

I add a debug print to see what properties of the event might indicate the
button:

$:output:python:
def timeline_button(widget, event):
    print(dir(event))
    ...
$:END

I find this:

$:output:text:
[..., 'button', ..., 'get_button', ...]
$:END

Let's try this:

$:output:python:
def timeline_button(widget, event):
    print(event.button)
$:END

When I press the left button, it prints 1, and when I press the right button,
it prints 3. There must be some constants for these. I search the GTK
documentation and find
[this](https://docs.gtk.org/gdk3/struct.EventButton.html):

> The button which was pressed or released, numbered from 1 to 5. Normally
> button 1 is the left mouse button, 2 is the middle button, and 3 is the right
> button.

Maybe there are no constants?

Let's codify our new knowledge like this:

$:output:python:
def timeline_button(widget, event):
    if event.button == 1:
        self.timeline.left_mouse_down(*timeline.translate_coordinates(
            main_window,
            event.x,
            event.y
        ))
    elif event.button == 3:
        self.timeline.right_mouse_down(*timeline.translate_coordinates(
            main_window,
            event.x,
            event.y
        ))
$:END

We rename the previous `mouse_down` to `left_mouse_down` and add a new empty
method for `right_mouse_down`.

$:output:shell:
$ ./make.py commit -m 'Timeline receives both left and right mouse down events.'
...............................................
----------------------------------------------------------------------
Ran 47 tests in 1.923s

OK
[main 0fc6fb1] Timeline receives both left and right mouse down events.
 1 file changed, 17 insertions(+), 7 deletions(-)
$:END

## Review

It's a little unnclear to me what the translation of coordinates are doing. I
think the coordinates received in the event are relative to the whole GTK
window and the timeline expects coordinates relative to itself.

I don't really want to focus on this now, but I add a TODO in the code that I
should clarify this.

In this project I've tried to keep my "backlog" in the source code in the form
of TODO comments. Some I will probably never get back to, and others will serve
as a reminder. But so far I kind of like this approach.

## Separation of timeline and GTK

The timeline component is unaware of GTK. So when it receives the right mouse
down event, it can find the cut that we clicked on, but it doesn't have the
ability to show a context menu, because it needs to use GTK for that.

This separation is intentional. I've tried to isolate GTK code to the
outermost layer to keep the inner layers free from those details and make them
easier to test.

But this presents a problem now.

The only solution that comes to mind if we want to maintain this separation is
to create some kind of abstraction. Something like this:

$:output:python:
class GtkGuiAbstraction:

    def show_context_menu(self, generic_menu_description):
        # Create GTK context menu from generic_menu_description
$:END

And then pass that to the timeline so that it can do something like this:

$:output:python:
def right_mouse_down(self, x, y):
    self.gui.show_context_menu([
        MenuItem(label="over", action=lambda: ...),
        MenuItem(label="under", action=lambda: ...),
    ])
$:END

I'm not sure what I think about this. On the one hand it feels like a complex
extra layer. On the other hand I really want to isolate GTK code. My experience
tells me that GUI code can easily leak in to every part of the application and
it just makes everything more messy.

I will try to create the simplest possible solution of this design and see what
it feels like.

## GTK GUI abstraction

Let's start with a test:

$:output:python:
class GtkGui:

    def show_context_menu(self, menu):
        """
        >>> GtkGui().show_context_menu([
        ...     MenuItem(label="over", action=lambda: print("over")),
        ...     MenuItem(label="under", action=lambda: print("under")),
        ... ])
        """
$:END

This fails with

$:output:text:
NameError: name 'MenuItem' is not defined
$:END

I define `MenuItem` and we are green:

$:output:python:
class MenuItem(namedtuple("MenuItem", "label,action")):
    pass
$:END

Let's commit:

$:output:shell:
$ ./make.py commit -m 'The start of GtkGui and its show_context_menu method.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.923s

OK
[main e64b93e] The start of GtkGui and its show_context_menu method.
 1 file changed, 13 insertions(+)
$:END

The test so far does not assert anything. It just checks that the code does not
crash. But that is enough to experiment with the GTK API. Let's try to create
the menu and the test will tell is if we use the GKT API wrong.

I try this:

$:output:python:
menu = gtk.Menu()
$:END

Test immediately fails:

$:output:text:
NameError: name 'gtk' is not defined
$:END

Ah, it should be `Gtk` I see in the imports at the top of the file. Thank you
test.

I search the web for examples how to show a context menu in GTK. After a bit of
reading and trying, I end up with this:

$:output:python:
def show_context_menu(self, menu):
    """
    >>> event = namedtuple("FakeEvent", "button,time")(3, 0)
    >>> GtkGui(event).show_context_menu([
    ...     MenuItem(label="over", action=lambda: print("over")),
    ...     MenuItem(label="under", action=lambda: print("under")),
    ... ])
    """
    def create_gtk_handler(menu_item):
        def handler(widget):
            menu_item.action()
        return handler
    gtk_menu = Gtk.Menu()
    for menu_item in menu:
        gtk_menu_item = Gtk.MenuItem(label=menu_item.label)
        gtk_menu_item.connect("activate", create_gtk_handler(menu_item))
        gtk_menu_item.show()
        gtk_menu.append(gtk_menu_item)
    gtk_menu.popup(None, None, None, None, self.event.button, self.event.time)
$:END

The `Gtk.Menu` seems to need an event to show itself according to examples. So
I pass that along to `GtkGui` and use a fake one in the test. The event handler
looks like this:

$:output:python:
def right_mouse_down(self, x, y, gui):
    gui.show_context_menu([
        MenuItem(label="over", action=lambda: print("over")),
        MenuItem(label="under", action=lambda: print("under")),
    ])
$:END

I decide to pass the `gui` along in the method call. That way, it can more
easily be constructed with the right click event in the outer layer:

$:output:python:
elif event.button == 3:
    self.timeline.right_mouse_down(*timeline.translate_coordinates(
        main_window,
        event.x,
        event.y
    ), GtkGui(event))
$:END

This works fine and when I click the menu items, the corresponding text is
shown in the console:

<p>
<center>
![Context menu popup.](popup.png)
</center>
</p>

Let's commit:

$:output:shell:
$ ./make.py commit -m 'Show a context menu when right clicking in timeline.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.954s

OK
[main 4201621] Show a context menu when right clicking in timeline.
 1 file changed, 22 insertions(+), 4 deletions(-)
$:END

## Modify cut

I continue to modify `right_mouse_down` to this:

$:output:python:
def right_mouse_down(self, x, y, gui):
    cut = self.rectangle_map.get(x, y)
    if isinstance(cut, Cut):
        gui.show_context_menu([
            MenuItem(label="over", action=lambda: print("over")),
            MenuItem(label="under", action=lambda: print("under")),
        ])
$:END

I want to show the context menu only if we right click on a cut. We can use
the rectangle map for that.

All tests pass, but when I try this in the application, it fails with this:

$:output:text:
NameError: name 'Cut' is not defined
$:END

I should have started with a test. Let's move a little slower.

I add this line in a larger timeline test where we have some objects setup:

$:output:python:
"""
>>> timeline.right_mouse_down(5, 25, FakeGui(click_context_menu="over"))
"""
$:END

And here comes one benefit of the GUI abstraction: easier testing. In the test
we can pass a `FakeGui` that will simulate that we click a context menu item.
We implement it like this:

$:output:python:
class FakeGui:

    def __init__(self, click_context_menu=None):
        self.click_context_menu = click_context_menu

    def show_context_menu(self, menu):
        for menu_item in menu:
            if menu.label == self.click_context_menu:
                menu.action()
                return
$:END

Now we get the same error about `Cut` not being defined. But this time, we get
it when running the test suite. Success!

I import `Cut` and get an error that 'list' object has no attribute 'label'.
Ah. I made a mistake in the fake GUI. `label` and `action` should be accessed
on the item, not the menu.

The current context menu just prints its label, so to make the test pass, let's
assert on that:

$:output:python:
"""
>>> timeline.right_mouse_down(5, 25, FakeGui(click_context_menu="over"))
over
"""
$:END

And we are back to green. Let's commit:

$:output:shell:
$ ./make.py commit -m 'Test right clicking a cut.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.957s

OK
[main 7bf3e14] Test right clicking a cut.
 1 file changed, 33 insertions(+), 7 deletions(-)
$:END

But we don't want to print the menu item label. We want to change the
`mix_strategy` of the clicked cut. Let's assert on that instead:

$:output:python:
"""
>>> timeline.right_mouse_down(5, 25, FakeGui(click_context_menu="over"))
>>> timeline.get_cut(cut_id).mix_strategy
'over'
"""
$:END

This fails because `right_mouse_down` still prints its label. I remove the
print and it now fails because `Timeline.get_cut` is not defined. I add it and
get the correct failure:

$:output:text:
Failed example:
    timeline.get_cut(cut_id).mix_strategy
Differences (ndiff with -expected +actual):
    - 'over'
    + 'under'
$:END

The original mix strategy is `over` and this test should have changed it to
`under`, but it didn't. Let's fix that. As I try to get this test to pass, I
get many test failures. The failures guide me what to do next. This method is
not defined. Define it. This name does not exist. Fix spell error. I eventually
end up with this:

$:output:python:
def right_mouse_down(self, x, y, gui):
    cut = self.rectangle_map.get(x, y)
    if isinstance(cut, Cut):
        def mix_strategy_updater(value):
            def update():
                with self.project.new_transaction() as transaction:
                    transaction.modify(cut.id, lambda cut:
                        cut.with_mix_strategy(value))
            return update
        gui.show_context_menu([
            MenuItem(label="over", action=mix_strategy_updater("over")),
            MenuItem(label="under", action=mix_strategy_updater("under")),
        ])
$:END

This makes the test pass and also works beautifully in the application. Let's
commit:

$:output:shell:
$ ./make.py commit -m 'Can change mix strategy of clip with context menu.'
................................................
----------------------------------------------------------------------
Ran 48 tests in 1.956s

OK
[main 776171a] Can change mix strategy of clip with context menu.
 3 files changed, 26 insertions(+), 4 deletions(-)
$:END

## Summary

We now have a way to change the mix strategy of a cut in the GUI. The
application is a little more useful now.

Working with third party frameworks, like GTK, I find often slows you down. You
need to learn the details of it and it is often difficult to write tests.
Therefore I'm quite happy with the abstraction that we created. I want to keep
as many classes as possible away from messy GTK code.
