Today's thought is about using tests to give you feedback about your code.

I remember watching a screencast series with Kent Beck where he showed how he
did TDD. I also remember being disappointed at the end because I felt that the
tests at the last part was unfinished.

The reason I found the tests unfinished was that they depended on a server
being started manually. Maybe there were other manual steps involved, I can't
remember. So what I would have liked to see in the last part was the
development of some kind of setup code that enabled the tests to run fully
automatically.

My first reaction when I watched the series was that the tests were not that
useful since they required manual actions to be run. But what I failed to see
was that the tests gave valuable feedback to Kent while he was developing his
program. He wouldn't have been able to make progress that fast and with quality
without the tests.

Perhaps it was even wise to wait with the complete setup. If it turned out that
the program was not useful anyway, or that the solution wasn't good enough in
some respect, it would have been a waste to write extensive setup code. When
you start out, perhaps it is most important to get feedback quickly so that you
can verify what you are about to do is the right thing to do. Eventually you
might want full automation in your tests.

Another case where full automation is maybe not that important is when you
refactor code. Perhaps the tests will only live for a short period of time, and
if they can give you feedback fast enough, full automation might be a waste.

--

* [Screencast series](http://pragprog.com/screencasts/v-kbtdd/test-driven-development)
