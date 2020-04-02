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
