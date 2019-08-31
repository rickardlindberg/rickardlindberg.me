import contextlib
import unittest

import wx

class WxTest(unittest.TestCase):

    def test_wx(self):
        with self.wxapp() as app:
            # Test something that requires a wx.App
            pass

    @contextlib.contextmanager
    def wxapp(self):
        app = wx.App()
        try:
            yield app
        finally:
            app.Destroy()
