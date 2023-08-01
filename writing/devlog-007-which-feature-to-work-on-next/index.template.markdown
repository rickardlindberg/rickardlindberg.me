---
title: 'DevLog 007: Which feature to work on next?'
date: 2023-08-01
tags: devlog,rlvideo,refactoring
devlog: true
---

In this session I will select what to work on next in my [video
editor](/projects/rlvideo/index.html) by trying to use it to edit some footage
and see where I get stuck.

I've previously managed to create a project which has some footage imported and
proxy clips generated. I can open that project like this:

$:output:text:
$ rlvideo my-project.rlvideo
$:END

When I do that, two things happen that annoy me.

First of all, there are lots of exceptions printed to the console:

$:output:text:
Traceback (most recent call last):
  File "/home/rick/rlvideo/rlvideolib/gui/gtk.py", line 80, in timeline_draw
    self.timeline.draw_cairo(
  File "/home/rick/rlvideo/rlvideolib/gui/generic.py", line 200, in draw_cairo
    self.draw_scrollbar(context, area, playhead_position)
  File "/home/rick/rlvideo/rlvideolib/gui/generic.py", line 287, in draw_scrollbar
    self.rectangle_map.add(Rectangle(
  File "/home/rick/rlvideo/rlvideolib/graphics/rectangle.py", line 19, in __init__
    raise ValueError("Width must be > 0.")
ValueError: Width must be > 0.
$:END

And second of all, it seems like it's loading proxy clips again even though
they are already generated:

<p>
<center>
![Loading.](loading.png)
</center>
</p>

Which one should I work on? Should I work on something else? What is most
important?

## Analysis

Let's do an analysis of why the two problems occur.

The exception when drawing the scrollbar happens because there are too many
clips in a too small window, so the width of the scrollbar handle gets smaller
than 1 pixel. It can be worked around by zooming out a bit so that a larger
portion of the timeline is visible.

This is obviously not good, but not the end of the world.

The fix probably involves setting a minimum width on the handle.

What about proxies?

Actually, proxies are not created again, but in order to find the correct proxy
for a clip, the clip's md5 sum has to be calculated. This is much faster than
generating the proxy, but still takes some time, delaying me when I want to
edit clips.

The fix probably involves storing the path of the proxy clip in the project
file.

It is also not the end of the world. I can open the editor, go make some
coffee, and maybe when I'm back, it's done.

## Strategy

So which should I work on?

If you work in an agile fashion, doing evolutionary design, what should happen
is that it should get easier and easier to work with the code base and add new
features. I learned that from [James
Shore](https://www.jamesshore.com/v2/books/aoad2/design).

Say I start working on the scrollbar exception now. When I'm done with that,
it should be easier to fix the proxy loading issue than it was before, assuming
that the areas that need change overlap.

With that kind of thinking, it doesn't matter that much what we choose to work
on as long as we think it is somewhat important. Just pick one and the next
thing will be easier.

It almost sounds too good to be true, but I believe in it. For this to work
though, we need to practice evolutionary design. We'll do that today.

Let's pick the scrollbar issue.

## Review

The error happens in `draw_scrollbar` which looks like this:

$:output:python:
def draw_scrollbar(self, context, area, playhead_position):
    x_start = self.scrollbar.region_shown.start / self.scrollbar.whole_region.length * area.width
    x_end = self.scrollbar.region_shown.end / self.scrollbar.whole_region.length * area.width
    playhead_x = playhead_position / self.scrollbar.whole_region.length * area.width

    # TODO: add callback mechanism in rectangle map
    x, y, w, h = (
        area.x+x_start,
        area.y,
        x_end-x_start,
        area.height
    )
    rect_x, rect_y = context.user_to_device(x, y)
    rect_w, rect_h = context.user_to_device_distance(w, h)
    self.rectangle_map.add(Rectangle(
        x=int(rect_x),
        y=int(rect_y),
        width=int(rect_w),
        height=int(rect_h)
    ), "position")

    context.rectangle(area.x, area.y, area.width, area.height)
    context.set_source_rgba(0.4, 0.9, 0.4, 0.5)
    context.fill()

    scroll_box = Rectangle(x, y, w, h)
    context.rectangle(scroll_box.x, scroll_box.y, scroll_box.width, scroll_box.height)
    context.set_source_rgba(0.4, 0.9, 0.4, 0.5)
    context.fill()

    # Playhead
    context.set_source_rgb(0.1, 0.1, 0.1)
    context.move_to(playhead_x, area.top)
    context.line_to(playhead_x, area.bottom)
    context.stroke()

    context.set_source_rgb(0.1, 0.1, 0.1)
    scroll_box.draw_pixel_perfect_border(context, 2)
$:END

When I look at this, it's difficult for me to see what is going on. It is just
too long and does too much. It doesn't clearly represent what I had in mind
when I wrote it.

If we are going to do evolutionary design, we have to pay more attention to
design. All the time.

It's fine that I didn't pay too much attention last time I modified this
method, but now that we are here again, let's give it some extra love so that
it is easier to work with next time.

## Further review

The error happens when creating the rectangle in the following piece of code:

$:output:python:
# TODO: add callback mechanism in rectangle map
x, y, w, h = (
    area.x+x_start,
    area.y,
    x_end-x_start,
    area.height
)
rect_x, rect_y = context.user_to_device(x, y)
rect_w, rect_h = context.user_to_device_distance(w, h)
self.rectangle_map.add(Rectangle(
    x=int(rect_x),
    y=int(rect_y),
    width=int(rect_w),
    height=int(rect_h)
), "position")
$:END

Look, there is even a TODO comment there. Now that we are touching this piece
of code again, perhaps it's time to deal with it.

## The rectangle map

The rectangle map is used to store areas of the screen that the user can
interact with. You can put objects at a given rectangle and retrieve them by
position. Here is an example:

$:output:python:
"""
>>> r = RectangleMap()
>>> r.add(Rectangle(x=0, y=0, width=10, height=10), "item")
>>> r.get(5, 5)
'item'
>>> r.get(100, 100) is None
True
"""
$:END

In the timeline area, each cut puts itself in a rectangle, allowing a context
menu to be shown when it is right clicked like this:

$:output:python:
def right_mouse_down(self, x, y, gui):
    cut = self.rectangle_map.get(x, y)
    if isinstance(cut, Cut):
        # show context menu
$:END

The TODO comment that I wrote suggests that we should instead store objects
that can handle `right_mouse_down` events for example so that we don't need to
check instances at the outermost event handler.

Let's see if we can do it.

## Action test

I sketch this:

$:output:python:
class Action:

    def left_mouse_down(self, x, y):
        pass

    def right_mouse_down(self, x, y, gui):
        pass

    def mouse_move(self, x, y):
        pass

    def mouse_up(self):
        pass

class ScrollbarDragAction(Action):

    def __init__(self, timeline, scrollbar):
        self.timeline = timeline
        self.scrollbar = scrollbar
        self.mouse_up()

    def left_mouse_down(self, x, y):
        self.x = x

    def mouse_up(self):
        self.x = None

    def mouse_move(self, x, y):
        if self.x is not None:
            self.timeline.set_scrollbar(
                self.scrollbar.move_scrollbar(
                    x - self.x
                )
            )
$:END

Let's see if we can use it.

I modify `right_mouse_down` and all the other event handlers to this:

$:output:python:
def right_mouse_down(self, x, y, gui):
    item = self.rectangle_map.get(x, y)
    if isinstance(item, Action):
        item.right_mouse_down(x, y, gui)
        self.down_item = item
        return
    ...
$:END

This is special handling for the case where the entry in the rectangle map is
an `Action`. Eventually, we want there to be only actions in there, and then
the instance check can be removed.

Next I change what we put into the rectangle map for the scrollbar to this:

$:output:python:
self.rectangle_map.add(Rectangle(
    x=int(rect_x),
    y=int(rect_y),
    width=int(rect_w),
    height=int(rect_h)
), ScrollbarDragAction(self, self.scrollbar))
$:END

Boom! Test failure:

$:output:text:
Failed example:
    timeline.rectangle_map # doctest: +ELLIPSIS
Differences (ndiff with -expected +actual):
    ...
      Rectangle(x=0, y=0, width=300, height=20):
        scrub
      Rectangle(x=0, y=77, width=300, height=23):
    -   position
    +   <rlvideolib.gui.generic.ScrollbarDragAction object at 0x7fd1f8891d00>
$:END

There is now another object in the rectangle map. Let's modify the test to
assert that instead.

The question now is, will it work in the application?

This behavior I think lacks tests, so let's try.

Nothing happens.

I review the code and find that I had forgotten the `mouse_move` event:

$:output:python:
def mouse_move(self, x, y):
    if self.down_item:
        self.down_item.mouse_move(x, y)
        return
    ...
$:END

And that actually works!

$:output:text:
$ ./make.py commit -m 'Add a ScrollbarDragAction instead of position string.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.405s

OK
[main 87c9b07] Add a ScrollbarDragAction instead of position string.
 1 file changed, 59 insertions(+), 2 deletions(-)
$:END

I make the same change for the remaining actions.

Here is the one for scrubbing the timeline:

$:output:python:
class ScrubAction(Action):

    def __init__(self, player, scrollbar):
        self.player = player
        self.scrollbar = scrollbar
        self.mouse_up()

    def left_mouse_down(self, x, y):
        self.x = x

    def mouse_up(self):
        self.x = None

    def mouse_move(self, x, y):
        if self.x is not None:
            self.player.scrub(
                int(round(
                    self.scrollbar.content_start
                    +
                    x/self.scrollbar.one_length_in_pixels
                ))
            )
$:END

And here is the one for moving a cut and opening the context menu for a cut:

$:output:python:
class CutAction(Action):

    def __init__(self, project, cut, scrollbar):
        self.project = project
        self.cut = cut
        self.scrollbar = scrollbar
        self.mouse_up()

    def left_mouse_down(self, x, y):
        self.transaction = self.project.new_transaction()
        self.x = x

    def right_mouse_down(self, x, y, gui):
        def mix_strategy_updater(value):
            def update():
                with self.project.new_transaction() as transaction:
                    transaction.modify(self.cut.id, lambda cut:
                        cut.with_mix_strategy(value))
            return update
        gui.show_context_menu([
            MenuItem(label="over", action=mix_strategy_updater("over")),
            MenuItem(label="under", action=mix_strategy_updater("under")),
        ])

    def mouse_up(self):
        self.transaction = None
        self.x = None

    def mouse_move(self, x, y):
        if self.transaction is not None:
            self.transaction.rollback()
            self.transaction.modify(self.cut.id, lambda cut:
                cut.move(int((x-self.x)/self.scrollbar.one_length_in_pixels)))
$:END

## Clean up

At this point, we only put actions into the rectangle map, and we can simplify
the event handlers to this:

$:output:python:
def left_mouse_down(self, x, y):
    self.down_action = self.rectangle_map.get(x, y, Action())
    self.down_action.left_mouse_down(x, y)

def right_mouse_down(self, x, y, gui):
    self.down_action = self.rectangle_map.get(x, y, Action())
    self.down_action.right_mouse_down(x, y, gui)

def mouse_move(self, x, y):
    if self.down_action:
        self.down_action.mouse_move(x, y)
    else:
        self.rectangle_map.get(x, y, Action()).mouse_move(x, y)

def mouse_up(self):
    if self.down_action:
        self.down_action.mouse_up()
        self.down_action = None
$:END

$:output:text:
$ ./make.py commit -m 'Timeline assumes there are Actions in rectangle map.'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 3.381s

OK
[main 3c8e9b9] Timeline assumes there are Actions in rectangle map.
 Date: Mon Jul 31 14:32:06 2023 +0200
 2 files changed, 14 insertions(+), 64 deletions(-)
$:END

## Transaction problem

Everything seems to work fine. However, I notice that the committing of the
transaction has disappeared.

This is not tested anywhere, missed my manual tests, and is pretty severe.

Let's see if we can make the code a little more reliable. I write this test:

$:output:python:
"""
>>> project = Project.new()
>>> transaction = project.new_transaction()
>>> transaction = project.new_transaction()
Traceback (most recent call last):
  ...
ValueError: transaction already in progress
"""
$:END

I make it pass, and I am now more confident that this error will show up when
testing in the application.

$:output:text:
$ ./make.py commit -m 'Ensure there can be only one transaction active at a time.'
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.402s

OK
[main 2b36bdb] Ensure there can be only one transaction active at a time.
 2 files changed, 34 insertions(+), 7 deletions(-)
$:END

And sure enough, it does. The second time I try to drag a cut, I get the
"transaction already in progress" error.

Nice!

The fix:

$:output:diff:
     def mouse_up(self):
+        if self.transaction:
+            self.transaction.commit()
         self.transaction = None
         self.x = None
$:END

$:output:text:
$ ./make.py commit -m 'Ensure CutAction transaction is commited at mouse_up.'
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.406s

OK
[main 5fd460d] Ensure CutAction transaction is commited at mouse_up.
 1 file changed, 4 insertions(+), 1 deletion(-)
$:END

Normally you use a transaction like this:

$:output:python:
with project.new_transaction() as transaction:
    _ = transaction.add_text_clip("hello", length=30)
    x = transaction.add_text_clip("world", length=35)
    _ = transaction.add_text_clip("end", length=20)
    _ = transaction.add_text_clip("end", length=20)
    transaction.modify(x, lambda cut: cut.move(-10))
$:END

In that case a commit/rollback is guaranteed.

However, when dealing with mouse events, we can not use the context manager and
instead have to deal with mouse events.

The new check that prevents multiple transactions ensures that everything stops
working if we forget to close a transaction.

But I would like to come up with a nicer pattern for ensuring that transactions
close.

I'll add a TODO for it and maybe we can come up with a nicer solution later.

## Further cleanup

In order to satisfy Python's import mechanism, I put `Action` and `MenuItem` in
the `rlvideolib.domain.cut` module.

They obviously don't belong there.

Here is what the gui package looks like now:

$:output:text:
rlvideolib/gui/
├── generic.py
├── gtk.py
├── __init__.py
└── testing.py
$:END

Previously `Action` and `MenuItem` were defined in `generic`. That makes sense.
But now we have a dependency on them from `rlvideolib.domain.cut`. Should a
domain object depend on GUI? Maybe that is ok.

I think what I'll do is create another module inside the gui package called
`framework`. It will contain generic GUI elements that do not depend on GTK or
our application.

$:output:text:
$ ./make.py commit -m 'Move generic framework GUI code to new rlvideolib.gui.framework.'
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.393s

OK
[main b1a8f5d] Move generic framework GUI code to new rlvideolib.gui.framework.
 5 files changed, 23 insertions(+), 19 deletions(-)
 create mode 100644 rlvideolib/gui/framework.py
$:END

## Progress?

Back to this code where we started:

$:output:python:
# TODO: add callback mechanism in rectangle map
x, y, w, h = (
    area.x+x_start,
    area.y,
    x_end-x_start,
    area.height
)
rect_x, rect_y = context.user_to_device(x, y)
rect_w, rect_h = context.user_to_device_distance(w, h)
self.rectangle_map.add(Rectangle(
    x=int(rect_x),
    y=int(rect_y),
    width=int(rect_w),
    height=int(rect_h)
), ScrollbarDragAction(self, self.scrollbar))
$:END

Ah, the TODO is actually done now.

$:output:text:
$ ./make.py commit -m 'Remove completed TODO about callback mechanism for rectangle map.'
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.392s

OK
[main b757e3a] Remove completed TODO about callback mechanism for rectangle map.
 1 file changed, 1 deletion(-)
$:END

We still haven't made any progress on the exception problem though. But we have
fixed design issues in related areas.

Let's focus again on the exception.

## A common pattern

I think the following pattern exists in all places where we add actions to the
rectangle map:

$:output:python:
rect_x, rect_y = context.user_to_device(x, y)
rect_w, rect_h = context.user_to_device_distance(w, h)
self.rectangle_map.add(Rectangle(
    x=int(rect_x),
    y=int(rect_y),
    width=int(rect_w),
    height=int(rect_h)
), ...)
$:END

What about if we add a method to `RectangleMap` like this:

$:output:python:
def add_from_context(self, x, y, w, h, context, item):
    rect_x, rect_y = context.user_to_device(x, y)
    rect_w, rect_h = context.user_to_device_distance(w, h)
    self.add(Rectangle(
        x=int(rect_x),
        y=int(rect_y),
        width=int(rect_w),
        height=int(rect_h)
    ), item)
$:END

We can use that method to add both the scroll action and the scrub action.

However, the cut action looks slightly different:

$:output:python:
rect_x, rect_y = context.user_to_device(rectangle.x, rectangle.y)
rect_w, rect_h = context.user_to_device_distance(rectangle.width, rectangle.height)
if int(rect_w) > 0 and int(rect_h) > 0:
    rectangle_map.add(Rectangle(
        x=int(rect_x),
        y=int(rect_y),
        width=int(rect_w),
        height=int(rect_h)
    ), CutAction(project, self.get_source_cut(), scrollbar))
$:END

It actually has the check that we also need for the scrollbar. That is, we only
add the rectangle to the map if it has a width and height.

Let's add those checks to `add_from_context`:

$:output:python:
def add_from_context(self, x, y, w, h, context, item):
    rect_x, rect_y = context.user_to_device(x, y)
    rect_w, rect_h = context.user_to_device_distance(w, h)
    if int(rect_w) > 0 and int(rect_h) > 0:
        self.add(
            Rectangle(
                x=int(rect_x),
                y=int(rect_y),
                width=int(rect_w),
                height=int(rect_h)
            ),
            item
        )
$:END

$:output:text:
$ ./make.py commit -m 'Extract RectangleMap.add_from_context which does width/height checks.'
....................................................
----------------------------------------------------------------------
Ran 52 tests in 3.498s

OK
[main cd38e3e] Extract RectangleMap.add_from_context which does width/height checks.
 3 files changed, 17 insertions(+), 25 deletions(-)
$:END

And this actually resolves the exception problem when I open my project.

# Summary

I don't have much experience doing evolutionary design. My feeling right now is
that I need to spend much more time designing than what I am currently doing. I
feel like I need to do at least 60% designing and only 40% adding new
features. If you are reading this and have any experience with evolutionary
design, feel free to share it with me. I should probably also re-read the
chapters in James' book to refresh my memory.
