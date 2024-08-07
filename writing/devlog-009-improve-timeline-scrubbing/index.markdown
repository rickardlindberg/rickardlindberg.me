---
title: 'DevLog 009: Improve timeline scrubbing'
date: 2023-08-03
tags: devlog,rlvideo
devlog: true
---

As a try to edit some footage with my [video
editor](/projects/rlvideo/index.html), I get annoyed by a timeline scrubbing
issue.

Scrubbing the timeline means clicking and dragging the playhead and then the
frame at that position will play. This works fine today if you click and drag,
but if you only click, nothing happens:

<p>
<center>
![Scrub problem.](scrub-problem.png)
</center>
</p>

Sometimes I just want to place the playhead at a certain position. And then I
just want to click.

That's what we'll work on fixing today.

## Reviewing the scrub action

Here is the scrub action:

```python
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
```

We can see that the scrubbing is happening only when we move the mouse, not
if we just left click.

The solution seems obvious: make sure to scrub on the click as well.

Let's see how we can move slowly and carefully and pay attention to design as
we go along. Let's start with a test.

## Testing new functionality

This is the test that I come up with:

```python
"""
I scrub the player when clicked:

>>> class MockPlayer:
...     def scrub(self, position):
...         print(f"scrub {position}")
>>> class MockScrollbar:
...     content_start = 0
...     one_length_in_pixels = 1
>>> action = ScrubAction(player=MockPlayer(), scrollbar=MockScrollbar())
>>> action.simulate_click(x=10)
scrub 10
"""
```

The `left_mouse_down` currently takes both the x and y coordinates. In this
test, we only care about the x coordinate. That's why I introduced
`Action.simulate_click`. The idea is that it should simulate the calls that
GTK does when a left click happens. My idea is to extend this further with
something like `Action.simulate_drag` which will fire `left_mouse_down`,
`mouse_move`, and `mouse_up` in the same way that GTK would do it.

I implement it like this:

```python
def simulate_click(self, x=0, y=0):
    self.left_mouse_down(x=x, y=y)
```

To make the test pass, I call `self.player.scrub` in the `left_mouse_down`
event as well. I extract it to a common method to remove the duplication.

This passes the tests, and when I try it in the application, it works as
intended.

Are we done?

## A concern

Let's take a moment to think about some design issues.

One thing that worry me is that `Action.simulate_click` does not actually
simulate clicks in the right way. That is, when we hook this up with GTK, the
same kinds of events will not be generated.

Let's have a look at how it works today.

Here is how `*_mouse_down` is handled:

```python
timeline = Gtk.DrawingArea()
timeline.connect("button-press-event", timeline_button)
timeline.add_events(
    timeline.get_events() |
    Gdk.EventMask.SCROLL_MASK |
    Gdk.EventMask.BUTTON_PRESS_MASK |
    Gdk.EventMask.BUTTON_RELEASE_MASK |
    Gdk.EventMask.POINTER_MOTION_MASK
)
def timeline_button(widget, event):
    # TODO: clarify what translate_coordinates do
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
        ), GtkGui(event))
```

This code exists in a method which has a bunch of other GTK setup code and is
quite long.

Let's see if we can extract a GTK widget that has all the mechanisms for custom
drawing and event handling.

I slowly start to extract pieces, and eventually end up with this:

```python
class CustomDrawWidget(Gtk.DrawingArea):

    def __init__(self, main_window, custom_draw_handler):
        Gtk.DrawingArea.__init__(self)
        self.add_events(
            self.get_events() |
            Gdk.EventMask.SCROLL_MASK |
            Gdk.EventMask.BUTTON_PRESS_MASK |
            Gdk.EventMask.BUTTON_RELEASE_MASK |
            Gdk.EventMask.POINTER_MOTION_MASK
        )
        self.connect("draw", self.on_draw)
        self.connect("button-press-event", self.on_button_press_event)
        self.connect("button-release-event", self.on_button_release_event)
        self.connect("motion-notify-event", self.on_motion_notify_event)
        self.rectangle_map = RectangleMap()
        self.custom_draw_handler = custom_draw_handler
        self.down_action = None
        self.main_window = main_window

    def on_draw(self, widget, context):
        self.rectangle_map.clear()
        self.custom_draw_handler(context, self.rectangle_map)

    def on_button_press_event(self, widget, event):
        x, y = self.get_coordinates_relative_self(event)
        if event.button == 1:
            self.down_action = self.rectangle_map.get(x, y, Action())
            self.down_action.left_mouse_down(x, y)
        elif event.button == 3:
            self.down_action = self.rectangle_map.get(x, y, Action())
            self.down_action.right_mouse_down(GtkGui(event))

    def on_motion_notify_event(self, widget, event):
        x, y = self.get_coordinates_relative_self(event)
        if self.down_action:
            self.down_action.mouse_move(x, y)
        else:
            self.rectangle_map.get(x, y, Action()).mouse_move(x, y)

    def on_button_release_event(self, widget, event):
        if self.down_action:
            self.down_action.mouse_up()
            self.down_action = None

    def get_coordinates_relative_self(self, event):
        return self.translate_coordinates(
            self.main_window,
            event.x,
            event.y
        )
```

The timeline is then created like this:

```python
timeline = CustomDrawWidget(
    main_window=main_window,
    custom_draw_handler=timeline_draw,
)
```

This part of the code base does not have many tests. I therefore moved slowly
and tested my changes manually after each small step.

Let's discuss some aspects of this and what we have done:

* The `CustomDrawWidget` now owns the rectangle map. (The timeline gets a
  reference to it, but there it only one instance, and it is created by
  `CustomDrawWidget`.)

* The `CustomDrawWidget` can handle clearing of the rectangle map on redraw,
  something that the timeline previously did.

* The `CustomDrawWidget` can handle mouse events and take the appropriate
  action by using the rectangle map.

* The timeline widget no longer knows about mouse events. It just has a
  rectangle map that it can fill with actions to be performed.

When I look at this, I feel like there are so many more things to improve.
However, I will practice stopping here and think that I made a bit of
improvement.

We can now see a bit more clearly the connection between GTK events, the
rectangle map, and what methods are called on the action. And, if we need a
second component that does custom drawing and handles events with a rectangle
map, we can re-use `CustomDrawWidget` and do not need to duplicate as much.

## Summary

We improved the application a tiny bit by allowing click on the timeline to
position the playhead. We also cleaned up the code base in the area we touched.
It now reflects a little better the ideas that we have about the code. I'm
happy with this progress.
