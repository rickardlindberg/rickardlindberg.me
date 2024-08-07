---
title: 'DevLog 008: How to overcome lack of motivation?'
date: 2023-08-02
tags: devlog,rlvideo,motivation
devlog: true
---

As I sit down this morning to continue work on my [video
editor](/projects/rlvideo/index.html), I don't feel motivated at all. I browse
some code that I worked on yesterday, see problems with it, but can't really
see how to improve it. Everything feels complicated, and I don't feel like
programming at all.

What to do?

Well, this is just a hobby project of mine. I could just do something else
today. But let's pretend that it's not. After all, I still have a desire to
make progress on this project.

## Options

Generally speaking, I can make two types of changes to the code:

* Refactoring to improve the design.
* Adding a feature to improve the product.

Yesterday, I spent most of the day refactoring and designing. I still feel that
the design needs improvements in the area that I worked on, but I find that
extra hard to motivate myself to work on today.

And perhaps that is also the wrong thing to do? Yesterday I improved the design
a little to the point where fixing an actual problem was easier. Shouldn't that
be enough?

If we keep improving a little bit for every feature we work on, we never have
to exclusively work on refactoring.

When I write that, it makes sense to me. I should practice feeling content with
having made some improvements. I should practice not striving for perfection.

## How to improve the product?

So how can we improve the product? What is something that we can add that makes
it easier, more pleasant, or more efficient for me to edit footage?

Yesterday I was annoyed by proxy clip loading time. I am still annoyed by that,
but I have a feeling it will be a little difficult to fix. And I don't feel up
for it this morning.

Is there something easier that we can work on?

Yes, there is!

One common thing that happens when I shoot is that some clips turn out to be
complete garbage. I might have pressed the record button by mistake or I might
have an out of focus shot. In those cases I just want to discard the clip.

Say that clip `C0015.MP4` below is out of focus.

<p>
<center>
![Out of focus clip that I want to discard.](discard.png)
</center>
</p>

I want to open up the context menu for that cut and choose "ripple delete". It
should remove that cut from the timeline and move all cuts to the right of it
left to fill up the space.

If we add this feature, I can actually start editing some footage. Because that
is how I usually edit videos. I drop all clips on the timeline and then I cut
things apart and make it shorter. With this new feature, I still can't make any
cuts, but I can discard clips.

## Clips and cuts again

I keep mixing the words clip and cut. A clip means a file on disk. When a clip
is added to the timeline, a cut is created that spans the whole region of the
clip. So in the beginning, the clip and the cut is of equal length. However,
the cut can change in and out points of the clip, making it shorter.

## Approach

I will try to go slowly when working on this feature and pay attention to the
design as I go along.

I will try to make small refactorings to improve the design along the way, but
the focus will still be to implement this feature.

Let's get started.

## Starting point

Let's start with the context menu for a cut.

```python
class CutAction(Action):

    ...

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
```

Aha, we are back to the `CutAction` that we worked on in the previous DevLog.
This is an opportunity to make design improvements to it while still focusing
on the new ripple delete feature.

Making design improvements is always easier when we have tests, and it is many
times my preferred way of adding new functionality. So let's start there. This
is what I come up with:

```python
"""
I show a menu item for ripple delete:

>>> project = None
>>> cut = None
>>> scrollbar = None
>>> action = CutAction(project, cut, scrollbar)
>>> gui = TestGui()
>>> action.right_mouse_down(x=None, y=None, gui=gui)
>>> gui.print_context_menu_items()
over
under
ripple delete
"""
```

At this point, I just want to assert that we have a ripple delete menu item.

I null out any parameters that are not used.

To make this test run, I also have to extend `TestGui` with
`print_context_menu_items`:

```diff
diff --git a/rlvideolib/gui/testing.py b/rlvideolib/gui/testing.py
index aaba74d..0df0dda 100644
--- a/rlvideolib/gui/testing.py
+++ b/rlvideolib/gui/testing.py
@@ -4,7 +4,12 @@ class TestGui:
         self.click_context_menu = click_context_menu

     def show_context_menu(self, menu):
+        self.last_context_menu = menu
         for item in menu:
             if item.label == self.click_context_menu:
                 item.action()
                 return
+
+    def print_context_menu_items(self):
+        for item in self.last_context_menu:
+            print(item.label)
```

Let's make it pass:

```diff
@@ -290,6 +305,7 @@ class CutAction(Action):
         gui.show_context_menu([
             MenuItem(label="over", action=mix_strategy_updater("over")),
             MenuItem(label="under", action=mix_strategy_updater("under")),
+            MenuItem(label="ripple delete", action=lambda: None),
         ])

     def mouse_up(self):
```

Just enough to make the test pass. We now have a context menu item that will do
nothing when we click on it.

```text
$ ./make.py commit -m 'New ripple delete context menu item that does nothing.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 2.893s

OK
[main 2659383] New ripple delete context menu item that does nothing.
 2 files changed, 21 insertions(+)
```

## Moving slowly

The `TestGui` that we had to modify for this test lives in the
`rlvideolib.gui.testing` module. The `rlvideolib.gui` package looks like this:

```text
rlvideolib/gui
├── framework.py
├── generic.py
├── gtk.py
├── __init__.py
└── testing.py
```

We recently extracted the framework module. It contains framework related GUI
code that does not depend on GTK and does not depend on our application. It
makes sense for a framework to include facilities to help testing, right?

Let's get rid of the testing module and move its contents to the framework
module.

```text
$ ./make.py commit -m 'Move TestGui to rlvideolib.gui.framework and get rid of the testing module.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 2.896s

OK
[main 3ea95fc] Move TestGui to rlvideolib.gui.framework and get rid of the testing module.
 4 files changed, 21 insertions(+), 3 deletions(-)
 rename rlvideolib/gui/{testing.py => framework.py} (58%)
```

We have made small progress towards the ripple delete feature and made the code
base a little cleaner by indicating that test helpers are part of the GUI
framework. Nice!

I feel much more motivated now than when I got started. But before I move on, I
will take a break and have some breakfast.

## Back to the test

Let's go back to the test. This is what we have:

```python
"""
>>> project = None
>>> cut = None
>>> scrollbar = None
>>> action = CutAction(project, cut, scrollbar)
>>> gui = TestGui()
>>> action.right_mouse_down(x=None, y=None, gui=gui)
>>> gui.print_context_menu_items()
over
under
ripple delete
"""
```

That feels like a lot of set up to me. And many of the parameters are `None`.

I take a closer look at the x and y coordinates. As far as I can tell, no
action is using those in the `right_mouse_down` method. Let's get rid of them.

```text
$ ./make.py commit -m 'Get rid of x and y coordinates in Action.right_mouse_down since they are never used.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 3.403s

OK
[main 2c4c80e] Get rid of x and y coordinates in Action.right_mouse_down since they are never used.
 3 files changed, 4 insertions(+), 4 deletions(-)
```

Let's further refactor the test to this:

```python
"""
I show cut menu items on right click:

>>> gui = TestGui()
>>> action = CutAction(project=None, cut=None, scrollbar=None)
>>> action.right_mouse_down(gui=gui)
>>> gui.print_context_menu_items()
over
under
ripple delete
"""
```

This indicates that the showing of the menu does not depend on the project,
cut, or scrollbar. I think that it reads quite nicely.

```text
$ ./make.py commit -m 'Change cut action test to be assertion for menu items shown.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 3.411s

OK
[main 1708e0c] Change cut action test to be assertion for menu items shown.
 1 file changed, 2 insertions(+), 5 deletions(-)
```

Now we need a new test for clicking the ripple delete menu item.

## Ripple delete test

I write this test:

```python
"""
I ripple delete:

>>> gui = TestGui(click_context_menu="ripple delete")
>>> action = CutAction(project=None, cut=None, scrollbar=None)
>>> action.right_mouse_down(gui=gui)
do ripple delete
"""
```

That is, I assert that "do ripple delete" is printed when we press that menu
item. Baby steps.

I make it pass like this:

```diff
@@ -299,10 +306,12 @@ class CutAction(Action):
                     transaction.modify(self.cut.id, lambda cut:
                         cut.with_mix_strategy(value))
             return update
+        def ripple_delete():
+            print("do ripple delete")
         gui.show_context_menu([
             MenuItem(label="over", action=mix_strategy_updater("over")),
             MenuItem(label="under", action=mix_strategy_updater("under")),
-            MenuItem(label="ripple delete", action=lambda: None),
+            MenuItem(label="ripple delete", action=ripple_delete),
         ])
```

```text
$ ./make.py commit -m 'Add non-empty action for ripple delete.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 2.909s

OK
[main 4c4e272] Add non-empty action for ripple delete.
 1 file changed, 10 insertions(+), 1 deletion(-)
```

## The next step

Let's take the next step and assert that it actually does a ripple delete.

I modify the test to this:

```python
"""
I ripple delete:

>>> from rlvideolib.domain.project import Project
>>> project = Project.new()
>>> with project.new_transaction() as transaction:
...     hello_id = transaction.add_text_clip("hello", length=10, id="A")
...     _        = transaction.add_text_clip("there", length=10, id="B")
>>> project.split_into_sections().to_ascii_canvas()
|<-A0-----><-B0----->|

>>> CutAction(
...     project=project,
...     cut=project.project_data.get_cut(hello_id),
...     scrollbar=None
... ).right_mouse_down(
...     gui=TestGui(click_context_menu="ripple delete")
... )
>>> project.split_into_sections().to_ascii_canvas()
|<-B0----->|
"""
```

This got quite messy. Let's see if we can break it down. First we setup a new
project with two clips next to each other. Then we simulate that the ripple
delete menu item is clicked and assert that the first clip is removed and the
second clip is moved to the beginning.

The setup of the project is kind of messy. For example, we have to do the
import in the doctest to prevent a circular import. And we reach in to grab the
project data to get the cut.

There are many things to improve here.

But I think I want to move on and get it to pass. We'll get back to the issues
above. I promise.

I make this change:

```diff
         def ripple_delete():
-            print("do ripple delete")
+            self.project.ripple_delete(self.cut.id)
         gui.show_context_menu([
```

That tells me that 'Project' object has no attribute 'ripple_delete'.

I add it like this:

```python
class Project:

    ...

    def ripple_delete(self, cut_id):
        with self.new_transaction() as transaction:
            transaction.ripple_delete(cut_id)
```

That tells med that 'Transaction' object has no attribute 'ripple_delete'.

We're getting closer.

I can't come up with the general solution for ripple delete, so I hard code a
solution for the particular case where we only have two cuts in the project:

```python
class Transaction:

    ...

    def ripple_delete(self, cut_id):
        data = self.project.project_data
        data = data.remove_cut(cut_id)
        data = data.modify_cut(list(data.cuts.cut_map.keys())[0], lambda cut: cut.move(-10))
        self.project.set_project_data(data)
```

And also make a quick and dirty version of `remove_cut`.

```text
$ ./make.py commit -m 'Quick and dirty version of ripple delete that works in one case.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 3.902s

OK
[main 7ba45c9] Quick and dirty version of ripple delete that works in one case.
 2 files changed, 43 insertions(+), 5 deletions(-)
```

Here is the example project:

<p>
<center>
![Before ripple delete.](ripple1.png)
</center>
</p>

If we try to ripple delete the first clip in the GUI, we get this:

<p>
<center>
![After ripple delete.](ripple2.png)
</center>
</p>

Not quite right, but it shows progress in the right direction.

I think we can leave the cut action test alone for a while now. It is fine. Now
we need to turn our attention to the ripple delete method in the transaction
and make it work as intended.

## Generalizing ripple delete

The project has a hierarchy of classes representing the different parts. The
ripple delete only affects the cuts. I make that clear by just forwarding
`ripple_delete` calls until we get to `Cuts`.

Transaction forwards to project data:

```python
class Transaction:

    ...

    def ripple_delete(self, cut_id):
        self.project.set_project_data(self.project.project_data.ripple_delete(cut_id))
```

And project data forwards to cuts:

```python
class ProjectData(namedtuple("ProjectData", "sources,cuts")):

    ...

    def ripple_delete(self, cut_id):
        return self._replace(cuts=self.cuts.ripple_delete(cut_id))
```

And finally, the hard coded ripple delete is here:

```python
class Cuts(namedtuple("Cuts", "cut_map,region_to_cuts,region_group_size")):

    ...

    def ripple_delete(self, cut_id):
        data = self
        data = data.remove(cut_id)
        data = data.modify(list(data.cut_map.keys())[0], lambda cut: cut.move(-10))
        return data
```

```text
$ ./make.py commit -m 'Move ripple_delete down to Cuts.'
.....................................................
----------------------------------------------------------------------
Ran 53 tests in 3.397s

OK
[main c5deb77] Move ripple_delete down to Cuts.
 2 files changed, 10 insertions(+), 7 deletions(-)
```

If we can just get that one working properly, I think our feature is done.

## Endless sidetracks?

Implementing the ripple delete was much more difficult than I expected. I feel
like I hit problem after problem that I need to solve before I can actually get
to the ripple delete. I get tired and demotivated again.

Let's recap what's left on our imaginary TODO list.

First there is the issue of the messy cut action test, where it was
particularly painful to setup a project.

And then there is the ripple delete in cuts that is not fully implemented.

I actually think that is it.

Not as much as I felt it was. Writing it down helped me realize that.

I think both of them might be a little difficult and take some time. Instead of
documenting them in detail, I will just report on the status once done.

## Report

I managed to generalize ripple delete and add some tests for it.

I'm not sure it's perfect, but we can improve it later with the help of test.
I'm sure it is not harmful at least.

However, it brings up another question. What if we do a ripple delete on the
wrong clip? How to recover from that? The answer right now is that we can't.
Therefore, I think an undo function is high on the priority list. It should be
relatively straight forward to implement thanks to the immutable data
structures.

When it comes to the project setup, I just didn't have the energy to do
anything about it. I know this project setup is done in a few test, so I'm sure
we will come across it later. Hopefully I have a better idea for how to improve
it then. And some more energy. I'm OK leaving it like this. I don't think we
have made things worse. So much for a promise to get back to it.

## Summary

This change took longer than expected in part because the design was not clean
enough in a few places, and in part because the project was lacking methods for
modifying cuts because there had been no need for it.

We cleaned up the design in a few places add added a bit more functionality for
project editing operations.

Next time we work in this area, I think we can move faster.

Is this evolutionary design?

This also gets me thinking about stories and estimating stories and how that
does not make sense in this context. If things get easier and easier to
implement over time, that would also mean that time to complete a story takes
less and less time. So you can't really estimate multiple stories, because the
estimate changes once the previous story is completed. At least if stories
somewhat overlap it terms of changes in the code base.
