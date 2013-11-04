Today's thought is about doing only one thing per commit.

Yesterday when I was making a change to a piece of code, I didn't make sure to
start from a clean state with no changes in my files. I was working on
something before that and those changes were still there. I thought to myself
that the new change I was going to make was only going to affect a few files,
and that those files were not related to the previous change.

I was wrong.

The new changed turned out to involve a rename of a function that was used in
several different places. When it was time to commit, I had changes in maybe 15
files, and only about 12 of them should be committed. The others contained
changes from previously. I had to look at the diff carefully and make sure to
only commit what was supposed to be committed.

Had I been using git, I would have stashed my changes before starting a new
one. This time I was using subversion. But that is no excuse. I should figure
out how to enable stashing easily or just revert my previous change before
starting a new one.

--

* [Comments on Google+](https://plus.google.com/u/0/112175093836850283531/posts/arr1yE7HdZH)
