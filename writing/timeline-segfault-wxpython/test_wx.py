from unittest.mock import Mock
import unittest

import wx
import wx.lib.newevent

CustomEvent, EVT_CUSTOM = wx.lib.newevent.NewEvent()

class WxTest(unittest.TestCase):

    def test_wx(self):
        mock = Mock()
        mock.PostEvent(CustomEvent())

if __name__ == "__main__":
    unittest.main()
