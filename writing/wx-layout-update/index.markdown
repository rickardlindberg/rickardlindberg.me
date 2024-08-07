---
title: 'Layout/Update problem in wxPython'
date: 2020-04-03
tags: wxpython,rliterate
---
When working on [RLiterate](/projects/rliterate/index.html) I wanted to
speed up drawing to make characters appear faster on the screen after a
key press. The
[Update](https://docs.wxpython.org/wx.Window.html#wx.Window.Update)
method seemed to be what I wanted:

> Calling this method immediately repaints the invalidated area of the
> window and all of its children recursively (this normally only happens
> when the flow of control returns to the event loop).

However, using it turned out to be problematic. In this post I present a
test program that demonstrates the problem and then discuss a solution.

-   [Overview](#d72b284e97a449ed9a66e227cd8c3a57)
-   [Test program](#a4dfb24232ad4cbba0231c02ee01ac05)
-   [Results](#7bc0a753f4b74abca1ce3667a620d138)
-   [Discussion](#82457fb8605149e6a5e1b079971cbf9e)
-   [Complete test program](#3e79fd9904854dc2bd273d633931b05d)

[]{#d72b284e97a449ed9a66e227cd8c3a57}Overview
---------------------------------------------

The test program lays out a few widgets vertically. First a button that
triggers a change in layout, then a text widget surrounded by two panels
to more clearly indicate the size of the text widget. It looks like
this:

![](image1.png)

<!-- image text -->
<center>
Tests program.
</center>

When the button is clicked, the size of the text widget is increased
followed by a call to `Layout` and `Update` on the frame. I would expect
the frame to redraw immediately at this point, but it doesn\'t. At least
not correctly.

[]{#a4dfb24232ad4cbba0231c02ee01ac05}Test program
-------------------------------------------------

The structure of the test program looks like this:

```
1.  testlayout.py
```

```py
import wx
import time

class TestFrame(wx.Frame):

    <<TestFrame>>

class Text(wx.Panel):

    <<Text>>

if __name__ == "__main__":
    app = wx.App()
    frame = TestFrame()
    frame.Show()
    app.MainLoop()
```

If you prefer the read the complete test program, go to [*Complete test
program*](#3e79fd9904854dc2bd273d633931b05d).

The `__init__` method of the test frame lays out the widgets using a
vertical sizer and hooks up the click event.

```
1.  testlayout.py
2.  [TestFrame]{.cp}
```

```py
def __init__(self):
    wx.Frame.__init__(self, None)
    self.button = wx.Button(self, label="increase text size")
    self.button.Bind(wx.EVT_BUTTON, self._on_increase_size_click)
    self.top = wx.Panel(self, size=(-1, 20))
    self.top.SetBackgroundColour("yellow")
    self.text = Text(self)
    self.bottom = wx.Panel(self, size=(-1, 20))
    self.bottom.SetBackgroundColour("pink")
    self.Sizer = wx.BoxSizer(wx.VERTICAL)
    self.Sizer.Add(
        self.button,
        border=5, proportion=0, flag=wx.EXPAND|wx.ALL
    )
    self.Sizer.Add(
        self.top,
        border=5, proportion=0, flag=wx.EXPAND|wx.ALL
    )
    self.Sizer.Add(
        self.text,
        border=5, proportion=0, flag=wx.EXPAND|wx.ALL
    )
    self.Sizer.Add(
        self.bottom,
        border=5, proportion=0, flag=wx.EXPAND|wx.ALL
    )
```

The click event handler increases the size of the text widget and tries
to do an immediate update. The sleep is there to see if the update is
delayed or not.

```
1.  testlayout.py
2.  [TestFrame]{.cp}
```

```py
def _on_increase_size_click(self, event):
    print("")
    print("Click")
    self.text.increase_size()
    self.Layout()
    print("Update")
    self.Update()
    time.sleep(2)
    print("Update done")
```

The `__init__` method of the text widget sets an initial size and hooks
up paint and size events.

```
1.  testlayout.py
2.  [Text]{.cp}
```

```py
def __init__(self, parent):
    wx.Panel.__init__(self, parent)
    self.min_h = 0
    self.increase_size()
    self.Bind(wx.EVT_PAINT, self._on_paint)
    self.Bind(wx.EVT_SIZE, self._on_size)
```

The `increase_size` method increases the height and sets a new min size.

```
1.  testlayout.py
2.  [Text]{.cp}
```

```py
def increase_size(self):
    self.min_h += 10
    self.SetMinSize((-1, self.min_h))
```

The paint event handler draws a bunch of lines of text and also prints
the rectangles that were invalidated.

```
1.  testlayout.py
2.  [Text]{.cp}
```

```py
def _on_paint(self, event):
    print("repaint text")
    upd = wx.RegionIterator(self.GetUpdateRegion())
    while upd.HaveRects():
        print("  {}".format(upd.GetRect()))
        upd.Next()
    dc = wx.PaintDC(self)
    y = 0
    for x in range(20):
        line = "line {}".format(x)
        dc.DrawText(line, 5, y)
        y += dc.GetTextExtent(line)[1]
    event.Skip()
```

The size event just prints the new size.

```
1.  testlayout.py
2.  [Text]{.cp}
```

```py
def _on_size(self, event):
    print("resize text {}".format(event.GetSize()))
    event.Skip()
```

[]{#7bc0a753f4b74abca1ce3667a620d138}Results
--------------------------------------------

Here is the output of a button click:

```text
Click
resize text (396, 20)
Update
repaint text
  (0, 0, 396, 10)
Update done
repaint text
  (0, 0, 396, 20)
```

-   First, the click event hander is entered.
-   The text widget properly gets a resize event. (My guess is as a
    result of the `Layout` call.)
-   Then `Update` is called.
-   A repaint of the text widget is then done, but the invalidated
    rectangle is only 10px high. The new height is 20.
-   After the `sleep` call, a repaint of the text widget is done again,
    now with the correct rectangle.

So in practice, nothing is changed on the screen until after the sleep
call.

It seems to me that the `Layout` call depends on something happening by
following events. Adding a `Refresh` call immediately after `Layout`
does not seem to have any effect.

**Why does the immediate repaint don\'t get the correct size?**

[]{#82457fb8605149e6a5e1b079971cbf9e}Discussion
-----------------------------------------------

I asked about this problem in the [wxPython discussion
forum](https://discuss.wxpython.org/t/problem-updating-widget-immediately-with-layout-and-update)
and got the following response from Robin Dunn:

> Almost all of time using `Update` is not needed, and sometimes it can
> be a little detrimental. For example, on OSX there is a pipeline of
> operations to move display details from all of the running
> applications to the screen. This is highly optimized and works to keep
> the screen updated and appearing very smooth and crisp and keeping the
> applications efficient. When an application does something like
> drawing to a DC without a paint event, or forcing a paint event at the
> wrong moment, then that pipeline is interrupted to do that drawing and
> then has to be restarted again to redo processing what was interrupted
> before.

> So, in other words, in almost all cases using `Refresh` or
> `RefreshRect` when needed, and letting the drawing happen in naturally
> occurring paint events is usually the best choice. The platforms are
> already highly optimized for this and have been fine tuned for decades
> to make it the best it can be.

So it seems like `Update` is platform dependant and it\'s best to just
don\'t use it.

My solution for a faster redraw instead became to only draw the part
where the cursor is first, and draw everything else a few milliseconds
later if there are no additional input events. For example, if a title
is edited in the workspace, the table of contents might have to be
redrawn as well. But it can wait, since the focus is not on it right
now.

As a bonus, I found this wiki page that was quite useful:
[WhenAndHowToCallLayout](https://wiki.wxpython.org/WhenAndHowToCallLayout).

[]{#3e79fd9904854dc2bd273d633931b05d}Complete test program
----------------------------------------------------------

```text
import wx
import time

class TestFrame(wx.Frame):

    def __init__(self):
        wx.Frame.__init__(self, None)
        self.button = wx.Button(self, label="increase text size")
        self.button.Bind(wx.EVT_BUTTON, self._on_increase_size_click)
        self.top = wx.Panel(self, size=(-1, 20))
        self.top.SetBackgroundColour("yellow")
        self.text = Text(self)
        self.bottom = wx.Panel(self, size=(-1, 20))
        self.bottom.SetBackgroundColour("pink")
        self.Sizer = wx.BoxSizer(wx.VERTICAL)
        self.Sizer.Add(
            self.button,
            border=5, proportion=0, flag=wx.EXPAND|wx.ALL
        )
        self.Sizer.Add(
            self.top,
            border=5, proportion=0, flag=wx.EXPAND|wx.ALL
        )
        self.Sizer.Add(
            self.text,
            border=5, proportion=0, flag=wx.EXPAND|wx.ALL
        )
        self.Sizer.Add(
            self.bottom,
            border=5, proportion=0, flag=wx.EXPAND|wx.ALL
        )

    def _on_increase_size_click(self, event):
        print("")
        print("Click")
        self.text.increase_size()
        self.Layout()
        print("Update")
        self.Update()
        time.sleep(2)
        print("Update done")

class Text(wx.Panel):

    def __init__(self, parent):
        wx.Panel.__init__(self, parent)
        self.min_h = 0
        self.increase_size()
        self.Bind(wx.EVT_PAINT, self._on_paint)
        self.Bind(wx.EVT_SIZE, self._on_size)

    def increase_size(self):
        self.min_h += 10
        self.SetMinSize((-1, self.min_h))

    def _on_paint(self, event):
        print("repaint text")
        upd = wx.RegionIterator(self.GetUpdateRegion())
        while upd.HaveRects():
            print("  {}".format(upd.GetRect()))
            upd.Next()
        dc = wx.PaintDC(self)
        y = 0
        for x in range(20):
            line = "line {}".format(x)
            dc.DrawText(line, 5, y)
            y += dc.GetTextExtent(line)[1]
        event.Skip()

    def _on_size(self, event):
        print("resize text {}".format(event.GetSize()))
        event.Skip()

if __name__ == "__main__":
    app = wx.App()
    frame = TestFrame()
    frame.Show()
    app.MainLoop()
```
