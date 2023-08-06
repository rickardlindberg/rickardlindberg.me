---
title: 'DevLog 011: Modifying cut out point'
date: 2023-08-06
tags: devlog,rlvideo
devlog: true
---

I've added a few more timeline edit operations to the [video
editor](/projects/rlvideo/index.html). For example, it is now possible to
change the speed of a cut with ctrl+drag on the right hand side and modify the
in point with drag on the left hand side.

<p>
<center>
![Move right.](move-right.png)
</center>
</p>

However, changing the out point of a cut by dragging the right hand side does
not yet work. It prints the following in the console:

$:output:text:
TODO: implement move_right!
$:END

It is a bit trickier to get working than changing the in point as we will see
in a second.

## The call chain

Here is roughly what happens when you drag the right hand side of a cut:

$:output:python:
transaction = project.new_transaction()
transaction.modify(cut_id, lambda cut: cut.move_right(delta))
transaction.commit()
$:END

Here is `Transaction.modify`:

$:output:python:
def modify(self, cut_id, fn):
    self.project.set_project_data(self.project.project_data.modify_cut(cut_id, fn))
$:END

Here is `ProjectData.modify_cut`:

$:output:python:
def modify_cut(self, cut_id, fn):
    return self._replace(cuts=self.cuts.modify(cut_id, fn))
$:END

Here is `Cuts.modify`:

$:output:python:
def modify(self, cut_id, fn):
    ...
    old_cut = self.cut_map[cut_id]
    new_cut = fn(old_cut)
    new_cuts = dict(self.cut_map)
    new_cuts[cut_id] = new_cut
    return self._replace(
        cut_map=new_cuts,
        region_to_cuts=...,
    )
$:END

And it is here that the lambda gets called to modify the cut.

The problem is that when we modify the out point, we can't place it outside the
length of the source. And the cut itself does not know how long the source is.
It just has a source id where it can be looked up, but only in the
`ProjectData` structure, which is two levels above.

## Data structure consistency

Let's have a look at the data structures and what they contain:

$:output:python:
class ProjectData(namedtuple("ProjectData", "sources,cuts")):
class Sources(namedtuple("Sources", "id_to_source")):
class FileSource(namedtuple("FileSource", "id,path,length")):
class TextSource(namedtuple("TextSource", "id,text")):
class Cuts(namedtuple("Cuts", "cut_map,region_to_cuts,region_group_size")):
class Cut(namedtuple("Cut", "source,in_out,position,id,mix_strategy,volume,speed")):
$:END

Put in a more hierarchical format:

* ProjectData
    * sources (Sources)
    * cuts (Cuts)
* Sources
    * id_to_source (id -> source)
* Source
    * FileSource
    * TextSource
* Cuts
    * cut_map (id -> Cut)
    * region_to_cuts
    * region_group_size
* Cut
    * source (id)
    * in_out
    * position
    * id
    * mix_strategy
    * volume
    * speed

To make sure that a cut's out point does not exceed the length of the source,
we have to make the check in ProjectData since that is the only structure that
has both the source information and the cut information.

## Modify cut

Let's have a look at `ProjectData.modify_cut` again:

$:output:python:
def modify_cut(self, cut_id, fn):
    return self._replace(cuts=self.cuts.modify(cut_id, fn))
$:END

How about if we did something like this:

$:output:python:
def modify_cut(self, cut_id, fn):
    def wrapper(cut):
        return self.sources.get(cut.source).limit_out_point(fn(cut))
    return self._replace(cuts=self.cuts.modify(cut_id, wrapper))
$:END

That is, we let the original lambda  modify the out point beyond the length of
the source. Then in the wrapper above we get the source of the clip and have it
adjust the out point to not exceed the length.

I think this will actually work.

## Reflections

When first thinking about this problem I had a much more complicated solution
in mind. I was annoyed that the cut itself did not know about the maximum
length. I was thinking that `Cut.modify` somehow has to be passed a length so
that it could do the limiting itself.

Then I started writing about it, and I thought that each data structure should
be responsible for validating itself. Since a cut has no information about
length, it is ok to specify any length. But when a cut is put into a
`ProjectData` and is associated with a source, the validation must happen.

This makes a lot of sense to me, and I feel like a made a breakthrough.

You could argue that the design of the data structure is wrong. Perhaps a cut
should have more information about its source so that it can do more
validation.

But when it looks as it does, I think this will be fine.

Let's see if we can test this.

## Testing limiting out point

Here is the test that I come up with:

$:output:python:
"""
A cut's out point is adjusted if going outside the limit:

>>> data = ProjectData.empty()
>>> data = data.add_source(FileSource(id="source_a", path="a.mp4", length=5))
>>> data = data.add_cut(Cut.test_instance(name="source_a", start=0, end=3, id="cut_a"))
>>> data = data.modify_cut("cut_a", lambda cut: cut.move_right(10))
>>> data.get_cut("cut_a").in_out
Region(start=0, end=5)
"""
$:END

We create project data with one source and one cut. The source is of length 5
and the cut is of length 3. We can extend it two more frames before we have
reached the end of the source.

Then we modify the cut by trying to extend it by 10 frames.

Then we assert that the end point is limited to 5.

This fails with this:

$:output:text:
Failed example:
    data = data.modify_cut("cut_a", lambda cut: cut.move_right(10))
Differences (ndiff with -expected +actual):
    + TODO: implement move_right!
$:END

I implement `Cut.move_right` like this:

$:output:python:
def move_right(self, amount):
    return self._replace(
        in_out=self.in_out.move_end(amount),
    )
$:END

Then we get this failure:

$:output:text:
Failed example:
    data.get_cut("cut_a").in_out
Differences (ndiff with -expected +actual):
    - Region(start=0, end=5)
    ?                     ^
    + Region(start=0, end=13)
    ?                     ^^
$:END

I expected this. We don't do any limiting yet.

Let's modify `ProjectData.modify_cut` to what we had in mind. I write this:

$:output:python:
def modify_cut(self, cut_id, fn):
    def wrapper(cut):
        return self.get_source(cut.source.source_id).limit_in_out(fn(cut))
    return self._replace(cuts=self.cuts.modify(cut_id, wrapper))
$:END

We now get this error:

$:output:text:
AttributeError: 'TextSource' object has no attribute 'limit_in_out'
$:END

This is also to be expected. Now we need to implement `limit_in_out` on every
type of source. At the moment those are `TextSource` and `FileSource`. Let's
see if we have coverage for both. We get a failure for `TextSource` now, so
let's start there.

A text source does not have a length. It is infinite. So `limit_in_out` just
becomes this:

$:output:python:
def limit_in_out(self, cut):
    return cut
$:END

Now we get the same error for the file source.

I implement `FileSource.limit_in_out` like this:

$:output:python:
def limit_in_out(self, cut):
    return cut.limit_out(self.length)
$:END

The test now complains about this:

$:output:text:
AttributeError: 'Cut' object has no attribute 'limit_out'
$:END

I implement it like this:

$:output:python:
def limit_out(self, max_out):
    return self._replace(in_out=self.in_out.limit_end(max_out))
$:END

And `Region.limit_end` like this:

$:output:python:
    def limit_end(self, max_end):
        return self._replace(end=min(self.end, max_end))
$:END

And wow, that actually works.

$:output:text:
$ ./make.py commit -m 'ProjectData.modify_cut ensures that in_out is withing source limit.'
...................................................................
----------------------------------------------------------------------
Ran 67 tests in 3.925s

OK
[main a9eb857] ProjectData.modify_cut ensures that in_out is withing source limit.
 4 files changed, 29 insertions(+), 3 deletions(-)
$:END

## Improving design

Right above `modify_cut` I see `add_cut` which also has a TODO:

$:output:python:
def add_cut(self, cut):
    # TODO: assert that source id exists (even for json loading)
    return self._replace(cuts=self.cuts.add(cut))
$:END

Now that we have touched this area of the code, let's have a closer look if we
can make something cleaner with our new insights.

The `add_cut` could probably also benefit from having the in and out points
limited.

However, it is not used by the JSON loading mechanism.

I move the JSON loading part of the comment to here:

$:output:python:
@staticmethod
def from_json(json):
    # TODO: validate the cuts point to valid sources and that they have
    # valid in/out points.
    return ProjectData(
        sources=Sources.from_json(json["sources"]),
        cuts=Cuts.from_json(json["cuts"])
    )
$:END

I'm not sure that we want to adjust cuts that are invalid. We could remove cuts
that don't have a corresponding source, and we could adjust in and out points
of cuts with valid sources. But that would change the project. So a load +
save will save something else without the user having done any changes. Unless
manually modified, a JSON export should never have these problems. So
validation should be ok. But I said **should**. If we make a mistake somewhere,
we could export invalid JSON. So a load that fixes bad input it probably a good
idea. However, in such cases the user should probably be informed about the
changes made and a backup file with the old contents should probably be
written. I think this work is for a later time. Not really prioritized now.

Let's go back to `ProjectData.add_cut`. It is only used when the user actively
adds a cut somehow. At that point the cut does not exists yet, and if we modify
the in and out points, there is no obvious change.

Let's modify it guided by this test:

$:output:python:
"""
In/Out is modified according to source:

>>> ProjectData.empty(
... ).add_source(
...     FileSource(id="source_a", path="a.mp4", length=5)
... ).add_cut(
...     Cut.test_instance(name="source_a", start=0, end=10, id="cut_a")
... ).get_cut("cut_a").in_out
Region(start=0, end=5)
"""
$:END

It fails with this message:

$:output:text:
Differences (ndiff with -expected +actual):
    - Region(start=0, end=5)
    ?                     ^
    + Region(start=0, end=10)
    ?                     ^^
$:END

We fix it in a similar way to before:

$:output:python:
return self._replace(cuts=self.cuts.add(self.get_source(cut.source.source_id).limit_in_out(cut)))
$:END

That passes all the tests.

## Speed issue

I also noticed an issue with the limiting for cuts that had a changed speed. I
modify `FileSource.limit_in_out` to take speed into account:

$:output:python:
def limit_in_out(self, cut):
    """
    >>> source = FileSource(id="source_a", path="a.mp4", length=5)

    >>> source.limit_in_out(Cut.test_instance(start=0, end=10)).in_out
    Region(start=0, end=5)

    >>> source.limit_in_out(Cut.test_instance(start=0, end=20, speed=0.5)).in_out
    Region(start=0, end=10)
    """
    return cut.limit_out(int(self.length/cut.speed))
$:END

## Summary

We added a feature to the application. It is now possible to move the out point
of a cut and it is properly limited to not exceed the length of the underlying
source.

I was surprised at how elegant the solution came out. The realisation that made
this possible was that validation should happen at the point where all data
exists. Each data entity validates itself. If parent attributes are needed for
the validation, do the validation higher up the hierarchy.

This also makes me wonder if the limit of in point should also be done by the
source. Right now the cut assumes that in point >= 0 is ok. It doesn't need to
know anything about the source. But it makes assumptions about the source. I
think this assumption is always correct, but I don't think it hurts to not
assume anything and let the source do the decision.

I will probably try that refactoring out. My suspicion is that the code base
will be a little cleaner then.

But not in this session. This is it for now. See you next time!
