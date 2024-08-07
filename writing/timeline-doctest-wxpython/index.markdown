---
title: 'Doctest fails in Python 3 with wxPython'
date: 2019-08-31
tags: timeline,python
---
When working on porting [Timeline](/projects/timeline/index.html) to
Python 3, I ran into a problem where a
[doctest](https://docs.python.org/3/library/doctest.html) failed under
certain circumstances. I managed to create a small example that
reproduces the failure. I describe the example below and show how I
solved the test failure.

The example consists of a test runner and two test cases. The test
runner is a slimmed down version of the one used in Timeline:

```
1.  testrunner.py
```

```py
import doctest
import sys
import unittest

def load_test_cases_from_module_name(suite, module_name):
    __import__(module_name)
    module = sys.modules[module_name]
    module_suite = unittest.defaultTestLoader.loadTestsFromModule(module)
    suite.addTest(module_suite)

def load_doc_tests_from_module_name(suite, module_name):
    __import__(module_name)
    module = sys.modules[module_name]
    try:
        module_suite = doctest.DocTestSuite(module)
    except ValueError:
        # No tests found
        pass
    else:
        suite.addTest(module_suite)

if __name__ == "__main__":
    suite = unittest.TestSuite()
    load_test_cases_from_module_name(suite, "test_wx")
    load_doc_tests_from_module_name(suite, "test_doc")
    print(unittest.TextTestRunner().run(suite))
```

It creates a test suite with test cases from two modules: one with a
unit test and one with a doctests. It then runs the tests.

The first test is a unit test that needs an instance of `wx.App`:

```
1.  test\_wx.py
```

```py
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
```

This example doesn\'t test anything, but is enough to reproduce the
failure.

The second test is a doctest that asserts that a function prints a
string:

```
1.  test\_doc.py
```

```py
"""
>>> print_fun_stuff()
This is fun!
"""

def print_fun_stuff():
    print("This is fun!")
```

When I run this example, I get the failure:

```text
$ python3 testrunner.py
.This is fun!
F
======================================================================
FAIL: test_doc ()
Doctest: test_doc
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/usr/lib64/python3.7/doctest.py", line 2196, in runTest
    raise self.failureException(self.format_failure(new.getvalue()))
AssertionError: Failed doctest test for test_doc
  File "test_doc.py", line 0, in test_doc

----------------------------------------------------------------------
File "test_doc.py", line 2, in test_doc
Failed example:
    print_fun_stuff()
Expected:
    This is fun!
Got nothing


----------------------------------------------------------------------
Ran 2 tests in 0.074s

FAILED (failures=1)
<unittest.runner.TextTestResult run=2 errors=0 failures=1>
```

What appears to happen is that the expected string in the doctest is
written to the console (or perhaps stderr) instead of being captured by
doctest. When I run the doctest in isolation, it passes, so there is
nothing wrong with the test itself. It is the sequence of these two
tests that causes the problem.

My guess is that something in the wx test interferes with the doctest.
Perhaps instantiating a `wx.App` has some effects on streams and
redirection. But shouldn\'t the `app.Destroy()` call reset any such
effects? It would seem reasonable. But what if the `wx.App` is not
completely destroyed when the doctest is run? To test this, I modify the
example to force a garbage collection after the `app.Destroy()` call
like this:

```text
import gc; gc.collect()
```

This gets rid of the failure and the tests pass consistently. This is
also the solution that I adopted for Timeline.

The machine I run the example on is running Fedora 30, Python 3.7.3, and
wxPython 4.0.4:

```text
$ uname -a
Linux localhost.localdomain 5.0.9-301.fc30.x86_64 #1 SMP Tue Apr 23 23:57:35
UTC 2019 x86_64 x86_64 x86_64 GNU/Linux
$ python3
Python 3.7.3 (default, Mar 27 2019, 13:36:35)
[GCC 9.0.1 20190227 (Red Hat 9.0.1-0.8)] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import wx
>>> wx.version()
'4.0.4 gtk3 (phoenix) wxWidgets 3.0.4'
```

But the example doesn\'t always fail. On the Fedora 30 machine, it fails
most of the time, but sometimes it succeeds. When I run the example on a
machine that is running Fedora 26, Python 3.6.5, and wxPython 4.0.1, it
always succeeds:

```text
$ uname -a
Linux x220 4.16.11-100.fc26.x86_64 #1 SMP Tue May 22 20:02:12 UTC 2018 x86_64 x86_64 x86_64 GNU/Linux
rick@x220 | ~/rickardlindberg.me/writing/draft-timeline-doctest-wxpython
$ python3
Python 3.6.5 (default, Apr  4 2018, 15:09:05)
[GCC 7.3.1 20180130 (Red Hat 7.3.1-2)] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import wx
>>> wx.version()
'4.0.1 gtk3 (phoenix)'
```

Also, if I change the test so that it doesn\'t use a context manager, it
always succeeds:

```text
def test_wx(self):
    app = wx.App()
    try:
        # Test something that requires a wx.App
        pass
    finally:
        app.Destroy()
```

Perhaps the context manager has some effect on when objects are garbage
collected.

If you have any idea why this example sometimes fails, I would be
interested to know. It seems illogical that a forced garbage collection
should be needed to get a correct program.
