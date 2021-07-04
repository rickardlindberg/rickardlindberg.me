---
title: Dogfooding Literate Programming Support in Smart Notes (June 2021 Update)
date: 2021-07-04
tags: newsletter,rlselect
---

This is what I've been up to in June 2021.

<p><center>
<iframe width="560" height="315" src="https://www.youtube.com/embed/yhMm1q3Na1Q" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</center></p>

## Laptop Upgrade

I switched my main laptop and also upgraded it to use the latest version of
Fedora.

That led me to revisited and tweak my
[dotfiles](https://github.com/rickardlindberg/dotfiles/) for the new setup.

One tool that I rely on heavily is [rlselect](/projects/rlselect/index.html),
and it only works with Python 2.  The new version of Fedora doesn't ship Python
2 with all the libraries that rlselect needs. And Python 2 is deprecated
anyway. That gave me the urge to port rlselect to Python 3. But I also wanted
to write the port using a literate programming tool. I had previously thought
about trying to use [Smart
Notes](https://github.com/rickardlindberg/smartnotes) as a literate programming
tool, but it lacked support now.

## Literate Programming Support in Smart Notes

Because of my laptop upgrade, I had a new top priority for Smart Notes:
implement some kind of literate programming support.

I started by adding a script to convert
[RLiterate](/projects/rliterate/index.html) (my previous attempt at a literate
programming tool) documents to Smart Notes documents. That way I could convert
my projects that use RLiterate to Smart Notes so that I could continue editing
them from within Smart Notes.

I converted the Smart Notes RLiterate document to a Smart Notes document.
Smart Notes was now written in itself. But there was no way to edit the code
notes created by the convert script from within Smart Notes. The only way to
edit the code was by editing the generated files. But if I did that, the code
in the Smart Notes document and the generated files would be out of sync. So I
decided to write a script that would examine changes in the external files and
merge them back into the Smart Notes document. I got the idea that this was
possible by reading about a similar approach used in
[Leo](https://leoeditor.com/appendices.html#the-mulder-ream-update-algorithm).

I got the merge script working well enough. I was able to edit `smartnotes.py`
and merge the changes back into `smartnotes.notes`. So that when
`smartnotes.py` was generated from `smartnotes.notes` it would include the
changes.

I used this workflow to add support to Smart Notes to create and edit code
notes. At this point I was no longer in need of the merge script. I could do
all the editing from within Smart Notes. However, as I talked about in the
[previous update](/writing/newsletter/may-2021/index.html), such a merge script
would be crucial for working with others not using Smart Notes. And I believe
it will also come in handy to make editing external files from within Smart
Notes more convenient. For example, it would allow a whole class to be edited
even though that class is split into multiple code notes. When the whole class
is saved, the merge script will merge changes back into the corresponding code
notes.

## Port of rlselect to Python 3

At this point I could use Smart Notes as a literate programming tool to create
a new version of rlselect that worked with Python 3.

I started by importing all the existing code into a new Smart Notes document. I
did that by creating a minimal set of code notes. At this point, the code notes
were large and didn't really benefit from the literate approach.

Then I started making changes to make rlselect work with Python 3 and got it
working quite easily.

The next step was to split the code notes into smaller chunks to be able to
annotate and document smaller portions of the code. For that, Smart Notes
needed some additional features. At least to make the process convenient.

## Literate Programming Improvements

I added support in Smart Notes for splitting code notes and tried it on the
Smart Notes document itself. I didn't have time to use this feature to improve
the rlselect document, but I plan to.

I also added virtual links so that code notes that are related are
automatically linked.

This process of
[dogfooding](https://en.wikipedia.org/wiki/Eating_your_own_dog_food) is really
useful and fun. It answers the question very clearly what to work on next.

## Demo

To see a demo of the literate programming features in Smart Notes, have a look
at [5:14](https://youtu.be/yhMm1q3Na1Q?t=314).

## Reflections on Literate Programming

I thought about how the literate programming in Smart Notes feels different
from the literate programming in RLiterate. In Smart Notes you don't create a
single linear document. You connect pieces of information that are related to
each other. It will therefore also be difficult to extract a single linear
document from Smart Notes. So this kind of literate programming will not result
in "a book" that is the whole application with source code explained.  It will
rather be annotated source code. Linear documents might help with understanding
sometimes. So I'm not quite sure how it fits into the Smart Notes literate
programming approach. One possibility is to generate documentation files using
regular code notes. For example, a `README.md` could be generated by assembling
code notes from Smart Notes. But that `README.md` would not contain the whole
program with documentation. Just a small part.  Perhaps that is ok. By using
Smart Notes as my main literate programming tool, I will experiment and see
what it feels like.

I am really happy with the progress I made on the literate programming support
in Smart Notes. The script to convert RLiterate documents to Smart Notes
documents and the script to merge changes from external files back into Smart
Notes documents were killer features that allowed me to progress very fast.

## Videography

Aside from programming, I also continued to learn about my new hobby of
videography:

* I learned more about how to properly use my camera when it comes to white
  balance and exposure. I also ordered white balance cards to help me with
  that.

* I tried to figure out how to use different frame rates and shutter speeds to
  eliminate flicker from light sources. I found a good tool from RED to list
  good combinations: [flicker free
  video](https://www.red.com/flicker-free-video). I don't fully understand the
  logic behind it, but at least I can now get rid of some flicker in
  practice.

* I tested what happens to sound clips when cut in Kdenlive vs. in Ardour.
  Ardour adds short fade ins and fade outs, but Kdenlive doesn't. So probably
  cuts in Kdenlive can create unwanted sounds if they don't match up.

* I thought that my Rode Vide Mic NTG sounded noisy. I tried a firmware update,
  but it didn't help.  I tried various methods of recording, and all the
  results were similar. Perhaps I just need to be closer to the microphone so
  that the main audio is much louder than the background noise. The other
  option would be to reduce background noise in post.

## Reflections on Last Year

This time [last year](/writing/newsletter/june-2020/index.html) I was working
on projectional editing support for RLiterate. I am no longer convinced that
projectional editing is a good idea.  It might just be that I am very
comfortable with my current tools (Vim + command line tools) that I feel
frustrated when I can't use them. Or it might be that I haven't used any good
enough projectional editing system. My focus has at least shifted to build
tools that are more practical and comfortable for *me* to use.

This time last year I also read about compilers. It is a topic that I am still
interested in, and I hope to work more and write more about compiler related
stuff in the future.
