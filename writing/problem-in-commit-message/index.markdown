---
title: Problem statements in commit messages
date: 2015-10-23
---

I've been influenced by [Pieter Hintjens](http://hintjens.com/) recently. He
talks, among other things, about a software development process where each
patch must solve a single identified problem. He talks about a trick to start
your commit messages with "Problem:". The reason behind it is that you should
only be solving problems, not implement features (that someone might or might
not need later).

One day I made a connection between that idea and [this
tweet](https://twitter.com/scichelli/status/556154967002578944):

> "Commit messages should say why you made a change, not what you changed."
> ~@brndnpugh

If you implement Pieter's trick and start each commit message with "Problem:"
you are more likely to do what the tweet suggest. The commit message becomes
the problem statement. The why. The reason you are writing this patch. The
diff itself becomes the solution. The what.

Most of the time you can probably figure out the solution by looking at the
diff. Here is an example of that:

    # HG changeset patch
    # User Rickard Lindberg <ricli85@gmail.com>
    # Date 1445582758 -7200
    #      Fri Oct 23 08:45:58 2015 +0200
    # Node ID 8c9046e688c23fc0eab2e830df962988edf11518
    # Parent  ca24accb9910a5080b3fcf3925110cb1ce9637b3
    Problem: Controls are not properly aligned in duplicate event dialog.
    
    diff -r ca24accb9910 -r 8c9046e688c2 source/timelinelib/wxgui/dialogs/duplicateevent/view.py
    --- a/source/timelinelib/wxgui/dialogs/duplicateevent/view.py   Fri Oct 23 08:39:16 2015 +0200
    +++ b/source/timelinelib/wxgui/dialogs/duplicateevent/view.py   Fri Oct 23 08:45:58 2015 +0200
    @@ -31,20 +31,44 @@
         <BoxSizerVertical>
     
             <BoxSizerHorizontal border="ALL" >
    -            <StaticText label="$(nbr_of_duplicates_text)" />
    +            <StaticText
    +                label="$(nbr_of_duplicates_text)"
    +                align="ALIGN_CENTER_VERTICAL"
    +            />
                 <Spacer />
    -            <SpinCtrl width="50" name="sc_nbr_of_duplicates" />
    +            <SpinCtrl
    +                width="50"
    +                name="sc_nbr_of_duplicates"
    +                align="ALIGN_CENTER_VERTICAL"
    +            />
             </BoxSizerHorizontal>
     
    -        <RadioBox label="$(period_text)" name="rb_periods" choices="$(period_choices)" border="LEFT|RIGHT|BOTTOM" />
    +        <RadioBox
    +            label="$(period_text)"
    +            name="rb_periods"
    +            choices="$(period_choices)"
    +            border="LEFT|RIGHT|BOTTOM"
    +        />
     
             <BoxSizerHorizontal border="LEFT|RIGHT|BOTTOM" >
    -            <StaticText label="$(frequency_text)" />
    +            <StaticText
    +                label="$(frequency_text)"
    +                align="ALIGN_CENTER_VERTICAL"
    +            />
                 <Spacer />
    -            <SpinCtrl width="50" name="sc_frequency" />
    +            <SpinCtrl
    +                width="50"
    +                name="sc_frequency"
    +                align="ALIGN_CENTER_VERTICAL"
    +            />
             </BoxSizerHorizontal>
     
    -        <RadioBox label="$(direction_text)" name="rb_direction" choices="$(direction_choices)" border="LEFT|RIGHT|BOTTOM" />
    +        <RadioBox
    +            label="$(direction_text)"
    +            name="rb_direction"
    +            choices="$(direction_choices)"
    +            border="LEFT|RIGHT|BOTTOM"
    +        />
     
             <DialogButtonsOkCancelSizer
                 border="LEFT|RIGHT|BOTTOM"

(The changes to `RadioBox` are not related to solving the problem of alignment.
They are just style changes to improve readability. They should have been in a
separate commit.)

If you think it will be hard, you can perhaps spell out the solution in the
commit message. But after the problem statement. Here is one example of that:

    # HG changeset patch
    # User Rickard Lindberg <ricli85@gmail.com>
    # Date 1445583177 -7200
    #      Fri Oct 23 08:52:57 2015 +0200
    # Node ID c2b3d89a28a64112d2a8ca0545a8c8a32230b4bb
    # Parent  8c9046e688c23fc0eab2e830df962988edf11518
    Problem: Whole spin control is not visible.
    
    This is a problem on Linux using wxPython 3.
    
    Solution: Use default size.
    
    diff -r 8c9046e688c2 -r c2b3d89a28a6 source/timelinelib/wxgui/dialogs/duplicateevent/view.py
    --- a/source/timelinelib/wxgui/dialogs/duplicateevent/view.py   Fri Oct 23 08:45:58 2015 +0200
    +++ b/source/timelinelib/wxgui/dialogs/duplicateevent/view.py   Fri Oct 23 08:52:57 2015 +0200
    @@ -37,7 +37,6 @@
                 />
                 <Spacer />
                 <SpinCtrl
    -                width="50"
                     name="sc_nbr_of_duplicates"
                     align="ALIGN_CENTER_VERTICAL"
                 />
    @@ -57,7 +56,6 @@
                 />
                 <Spacer />
                 <SpinCtrl
    -                width="50"
                     name="sc_frequency"
                     align="ALIGN_CENTER_VERTICAL"
                 />


Another post about commit messages:

* [https://arialdomartini.wordpress.com/2012/09/03/pre-emptive-commit-comments/](https://arialdomartini.wordpress.com/2012/09/03/pre-emptive-commit-comments/)
