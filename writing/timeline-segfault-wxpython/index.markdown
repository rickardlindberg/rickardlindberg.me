---
title: 'Segfault with custom events in wxPython'
date: 2019-09-28
tags: timeline,python
---
When working on porting [Timeline](/projects/timeline/index.html) to
Python 3, I ran into a problem where a test caused a segfault. I managed
to create a small example that reproduces the failure. I describe the
example below and show how I solved the test failure.

The example consists of a test that stores an instance of a custom wx
event in a
[mock](https://docs.python.org/3.5/library/unittest.mock.html#the-mock-class)
object:

```
1.  test\_wx.py
```

```py
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
```

When I run this example, I get the following error:

```text
$ python3 test_wx.py
.
----------------------------------------------------------------------
Ran 1 test in 0.001s

OK
Segmentation fault (core dumped)
```

If I instead run it through gdb, I can see the C stacktrace where the
error happens:

```text
$ gdb python3
GNU gdb (GDB) Fedora 8.2.91.20190401-23.fc30
...
(gdb) run test_wx.py 
...
.
----------------------------------------------------------------------
Ran 1 test in 0.001s

OK

Program received signal SIGSEGV, Segmentation fault.
dict_dealloc (mp=0x7fffe712f9d8) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/dictobject.c:1901
1901  /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/dictobject.c: No such file or directory.
Missing separate debuginfos, use: dnf debuginfo-install fontconfig-2.13.1-6.fc30.x86_64 libXcursor-1.1.15-5.fc30.x86_64 libgcrypt-1.8.4-3.fc30.x86_64 libxkbcommon-0.8.3-1.fc30.x86_64 lz4-libs-1.8.3-2.fc30.x86_64 python3-sip-4.19.17-1.fc30.x86_64
(gdb) bt
#0  dict_dealloc (mp=0x7fffe712f9d8) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/dictobject.c:1901
#1  0x00007fffea02e1cc in wxPyEvtDict::~wxPyEvtDict (this=0x5555556f3ee8, __in_chrg=<optimized out>) at ../../../../src/pyevent.h:48
#2  wxPyEvent::~wxPyEvent (this=0x5555556f3e90, __in_chrg=<optimized out>) at ../../../../src/pyevent.h:96
#3  sipwxPyEvent::~sipwxPyEvent (this=0x5555556f3e90, __in_chrg=<optimized out>) at ../../../../sip/cpp/sip_corewxPyEvent.cpp:56
#4  0x00007fffea02e24d in sipwxPyEvent::~sipwxPyEvent (this=0x5555556f3e90, __in_chrg=<optimized out>) at ../../../../sip/cpp/sip_corewxPyEvent.cpp:56
#5  0x00007fffea02dfd2 in release_wxPyEvent (sipCppV=0x5555556f3e90, sipState=<optimized out>) at ../../../../sip/cpp/sip_corewxPyEvent.cpp:261
#6  0x00007fffe72ff4ce in ?? () from /usr/lib64/python3.7/site-packages/sip.so
#7  0x00007fffe72ff51d in ?? () from /usr/lib64/python3.7/site-packages/sip.so
#8  0x00007ffff7c14869 in subtype_dealloc (self=<_Event at remote 0x7fffea4ddd38>) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/typeobject.c:1256
#9  0x00007ffff7b8892b in tupledealloc (op=0x7fffe712cda0) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/tupleobject.c:246
#10 0x00007ffff7b8892b in tupledealloc (op=0x7fffea4d7620) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/tupleobject.c:246
#11 0x00007ffff7c14869 in subtype_dealloc (self=<_Call at remote 0x7fffea4d7620>) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/typeobject.c:1256
#12 0x00007ffff7b882ae in list_dealloc (op=0x7fffe70ba228) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/listobject.c:324
#13 0x00007ffff7c14869 in subtype_dealloc (self=<_CallList at remote 0x7fffe70ba228>) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/typeobject.c:1256
#14 0x00007ffff7b8c813 in free_keys_object (keys=0x555555855080) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/gcmodule.c:776
#15 dict_dealloc (mp=0x7fffe712f630) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/dictobject.c:1913
#16 subtype_clear (self=<optimized out>) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Objects/typeobject.c:1101
#17 delete_garbage (old=<optimized out>, collectable=<optimized out>) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/gcmodule.c:769
#18 collect (generation=2, n_collected=0x7fffffffd230, n_uncollectable=0x7fffffffd228, nofail=0) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/gcmodule.c:924
#19 0x00007ffff7c4ac4e in collect_with_callback (generation=generation@entry=2) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/gcmodule.c:1036
#20 0x00007ffff7ca7331 in PyGC_Collect () at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/gcmodule.c:1581
#21 0x00007ffff7caaf03 in Py_FinalizeEx () at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Python/pylifecycle.c:1185
#22 0x00007ffff7cab048 in Py_Exit (sts=sts@entry=0) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Python/pylifecycle.c:2278
#23 0x00007ffff7cab0ff in handle_system_exit () at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Python/pythonrun.c:636
#24 0x00007ffff7cab1e6 in PyErr_PrintEx (set_sys_last_vars=1) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Python/pythonrun.c:646
#25 0x00007ffff7cab651 in PyRun_SimpleFileExFlags (fp=<optimized out>, filename=<optimized out>, closeit=<optimized out>, flags=0x7fffffffd410) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Python/pythonrun.c:435
#26 0x00007ffff7cad864 in pymain_run_file (p_cf=0x7fffffffd410, filename=<optimized out>, fp=0x5555555a20d0) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/main.c:427
#27 pymain_run_filename (cf=0x7fffffffd410, pymain=0x7fffffffd520) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/main.c:1627
#28 pymain_run_python (pymain=0x7fffffffd520) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/main.c:2877
#29 pymain_main (pymain=0x7fffffffd520) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/main.c:3038
#30 0x00007ffff7cadc0c in _Py_UnixMain (argc=<optimized out>, argv=<optimized out>) at /usr/src/debug/python3-3.7.3-1.fc30.x86_64/Modules/main.c:3073
#31 0x00007ffff7e12f33 in __libc_start_main (main=0x555555555050 <main>, argc=2, argv=0x7fffffffd678, init=<optimized out>, fini=<optimized out>, rtld_fini=<optimized out>, stack_end=0x7fffffffd668) at ../csu/libc-start.c:308
#32 0x000055555555508e in _start ()
```

Somewhere in the middle, there is a call to `PyGC_Collect` followed, a
bit higher up, by a call to `release_wxPyEvent`. This indicates that the
error occurs during garbage collection of the custom wx event.

The machine I run the example on is running Python 3.7.3, and wxPython
4.0.4:

```text
$ python3
Python 3.7.3 (default, Mar 27 2019, 13:36:35)
[GCC 9.0.1 20190227 (Red Hat 9.0.1-0.8)] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import wx
>>> wx.version()
'4.0.4 gtk3 (phoenix) wxWidgets 3.0.4'
```

To solve this problem, I replaced the mock function `PostEvent` with one
that simply discards its input like this:

```text
mock.PostEvent = lambda x: None
```

This way there is no custom wx event to garbage collect. In Timeline\'s
case, it was not important to store the event in the mock object anyway.

If you have any idea why this example causes a segfault, I would be
interested to know. It feels like an error in the wxPython wrapper.
