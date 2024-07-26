---
title: Output Tracking vs Mocks
date: 2024-07-26
---

In this blog post we're going to explore how to write and test a Git client
using the [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
approach. Specifically we're going to explore how to apply [Output
Tracking](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#output-tracking)
to this example and also contrast it with mocks. All details about the example
are not explained, but the full source code is included at the end. Check out
James' article for more details about the testing without mocks approach.

## Example Git client

The example Git client is a CLI-application that provides a simplified
interface to Git. This represents a [real world scenario](https://gut-cli.dev/)
yet can be made small enough for an example.

The application implements two commands:

```
myscm save  -> git commit

myscm share -> git push
```

## Architecture

The application consists of the following classes:

```
App --+--> SaveCommand ---> Process
      |
      +--> ShareCommand --> Process
      |
      +--> Args
      |
      +--> Terminal
```

* `Process`, `Args`, and `Terminal` are low-level [infrastructure
  wrappers](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#infrastructure-wrappers)
  that are made
  [nullable](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#nullables)
  using [embedded
  stubs](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#embedded-stub).
    * `Process` is for running external processes. (`git` in this example.)
    * `Args` is for reading command line arguments.
    * `Terminal` is for writing text to the terminal.

* `SaveCommand` and `ShareCommand` are application code that perform a
  function in the domain of a Git client. They are made nullable using [fake it
  once you make
  it](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#fake-it).

* `App` is also application code that routes commands to the correct
  sub-command. It is also made nullable using "fake it once you make it".

## How to test `App`?

We want to write sociable, state-based test.

What does that mean in the context of testing `App`?

Sociable means that we should use its real dependencies. That is, we should
inject a real `SaveCommand`, `ShareCommand`, `Args`, and `Terminal`. We should
not inject test doubles like mocks or stubs.

So the test setup will look something like this:

$:output:python:
"""
>>> app = App(
...     save_command=SaveCommand(...),
...     share_command=ShareCommand(...),
...     terminal=Terminal(...),
...     args=Args(...),
... )
"""
$:END

However, if we were to invoke methods on `app` now, it would interact with the
outside world. It would read command line arguments, execute `git` commands,
and write to the terminal.

We don't want to do that. It takes a long time and is brittle. We therefore
inject null versions of dependencies like this:

$:output:python:
"""
>>> app = App(
...     save_command=SaveCommand.create_null(),
...     share_command=ShareCommand.create_null(),
...     terminal=Terminal.create_null(),
...     args=Args.create_null(),
... )
"""
$:END

Creating a null version is exactly like creating a real version except that at
the very edge of the application boundary, the communication with the outside
world is turned off. We put this in a factory-method:

$:output:python:
class App:

    @classmethod
    def create_null(cls):
        return cls(
            save_command=SaveCommand.create_null(),
            share_command=ShareCommand.create_null(),
            terminal=Terminal.create_null(),
            args=Args.create_null(),
        )

    ...
$:END

`App` has only one method, and that is `run`:

$:output:python:
def run(self):
    ...
$:END

So the only test we can write is this:

$:output:python:
"""
>>> app = App.create_null()
>>> app.run()
"""
$:END

There is no way to control what the command line arguments are, and there is no
way to observe what the application is doing.

Here are two scenarios that would be useful to test:

* When the application is called with `["save", "message"]`, then git commit
  is performed.

* When the application is called with `["share"]`, then git push is performed.

In order to write those test, we need a way to control the outside world to
simulate that a given set of command line arguments are present. We also need a
way to observe what commands are run.

We can solve the first part by passing simulated command line arguments to
`create_null`. The test then becomes this:

$:output:python:
"""
>>> app = App.create_null(args=["save", "message"])
>>> app.run()
"""
$:END

`App.create_null` is modified to this:

$:output:python:
class App:

    @classmethod
    def create_null(cls, args):
        return cls(
            save_command=SaveCommand.create_null(),
            share_command=ShareCommand.create_null(),
            terminal=Terminal.create_null(),
            args=Args.create_null(args=args),
        )

    ...
$:END

`Args` supports [configuring
responses](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#configurable-responses)
when creating the null version. In that case it would return the configured
command line arguments instead of the real ones. The communication with the
outside world has been turned off, and we simulate the part of the outside
world that reads command line arguments from the environment.

Now we can write our two scenarios like this:

$:output:python:
"""
>>> app = App.create_null(args=["save", "message"])
>>> app.run()
# How to assert that git commit was called?

>>> app = App.create_null(args=["share"])
>>> app.run()
# How to assert that git push was called?
"""
$:END

And now we come to the main topic of this blog post: output tracking.

`App` performs an action by delegating to `SaveCommand` and `ShareCommand`.
Both of them take the rest of the command line arguments and perform an action
without returning anything. To observe that with output tracking, we introduce
state in the commands so that we can query them and see if they were run. A
slightly more elegant solution, instead of introducing state, is to fire
events. Here is how we implement it in `SaveCommand`:

$:output:python:
class SaveCommand(Trackable):

    def run(self, args):
        self.notify(f"SAVE_COMMAND {args!r}")
        ...
$:END

To track events, we can do this:

$:output:python:
"""
>>> events = Events()
>>> SaveCommand.create_null().track_events(events).run(["message"])
>>> events
SAVE_COMMAND ['message']
"""
$:END

We use the event tracking pattern for both commands and the terminal like this:

$:output:python:
class App:

    @classmethod
    def create_null(cls, events, args):
        return cls(
            save_command=SaveCommand.create_null().track_events(events),
            share_command=ShareCommand.create_null().track_events(events),
            terminal=Terminal.create_null().track_events(events),
            args=Args.create_null(args=args),
        )

    ...
$:END

And now we can write our tests like this:

$:output:python:
"""
>>> events = Events()
>>> App.create_null(events, args=["save", "message"]).run()
>>> events
SAVE_COMMAND ['message']

>>> events = Events()
>>> App.create_null(events, args=["share"]).run()
>>> events
SHARE_COMMAND []

>>> events = Events()
>>> App.create_null(events, args=["unknown", "sub", "command"]).run()
>>> events
TERMINAL_WRITE 'Unknown command.'
"""
$:END

The implementation looks like this:

$:output:python:
def run(self):
    args = self.args.get()
    if args[0:1] == ["save"]:
        self.save_command.run(args[1:])
    elif args[0:1] == ["share"]:
        self.share_command.run([])
    else:
        self.terminal.write("Unknown command.")
$:END

## Reflections

The tests for `App` are similar to end-to-end-test in that the whole stack is
executed. Except right at the application boundary. So if we supply incorrect
arguments to the save command for example, this test will blow up:

$:output:python:
"""
>>> App.create_null(Events(), args=["save"]).run()
Traceback (most recent call last):
  ...
ValueError: Expected one argument as the message, but got [].
"""
$:END

This is overlapping, sociable testing. We are actually testing that `App` calls
`SaveCommand` correctly. However, the behavior of the save command is not
tested here. We only test that application parses command line arguments
correctly and calls the appropriate sub-command.

## The Mock version

Let's contrast how the first test can be written using mocks and stubs instead.
Here it is again:

$:output:python:
"""
>>> events = Events()
>>> App.create_null(events, args=["save", "message"]).run()
>>> events
SAVE_COMMAND ['message']
"""
$:END

And here is the mock/stub version:

$:output:python:
"""
>>> save_command_mock = Mock()
>>> App(
...     save_command=save_command_mock,
...     share_command=None,
...     terminal=None,
...     args=Mock(**{"get.return_value": ["save", "message"]})
... ).run()
>>> save_command_mock.run.call_args_list
[call(['message'])]
"""
$:END

The share command and terminal are not exercised in this test, so we
inject `None`. For `args` we inject a stub that is configured to return
`["save", "message"]` when its `get` method is called. For the `save_command`,
we inject a mock. After we call the `run` method on the application, we assert
that the `run` method was called on the mock with the `['message']` argument.

Let's contrast the two assertions:

$:output:python:
"""
>>> events
SAVE_COMMAND ['message']

>>> save_command_mock.run.call_args_list
[call(['message'])]
"""
$:END

They look very similar. Almost to the point that output tracking feels like
mocking.

But there is one crucial difference:

**The mock version creates isolated tests whereas the output tracking version
creates sociable tests.**

We have already seen what happens in the output tracking version when we call
the save command with incorrect arguments. What happens in the mock based
version? It happily passes:

$:output:python:
"""
>>> save_command_mock = Mock()
>>> App(
...     save_command=save_command_mock,
...     share_command=None,
...     terminal=None,
...     args=Mock(**{"get.return_value": ["save"]})
... ).run()
>>> save_command_mock.run.call_args_list
[call([])]
"""
$:END

To make the mock based test suite "equivalently powerful" we need to augment it
with "contract tests". In this case we need a test saying something like when
the save command is called with no arguments, it does not blow up. And we have
to write such tests for every example in our test suite. When we assert that a
dependency is called in a certain way or returns a certain thing under certain
conditions, we also have to write a "contract test" that checks that the
dependency can actually accept those arguments and return those things under
said conditions. That seems like a whole lot more work to me. (I think the term
"contract test" is mostly used in the context of external services, but I think
the reasoning is the same for two classes where one is a dependency. That's how
J.B. Rainsberger uses the term in [J B Rainsberger Integrated Tests Are A Scam
HD](https://youtu.be/VDfX44fZoMc?si=aqwG_mTe_ZPmu-kk&t=2315) and [Getting
Started with Contract
Tests](https://blog.thecodewhisperer.com/permalink/getting-started-with-contract-tests).)


## Recording function calls vs actions

Another more subtle difference between output tracking and mocks is that output
tracking tracks the action that was performed whereas mocks record function
calls.

Here are the two assertions again:

$:output:python:
"""
>>> events
SAVE_COMMAND ['message']

>>> save_command_mock.run.call_args_list
[call(['message'])]
"""
$:END

The save command emits an event that indicates that the save action was
performed with the given arguments. We are free to rename individual functions
and the test will still pass.

In the mock version we explicitly check that the `run` method was called. If we
want to rename it, we have to update the test as well.

I struggle a bit with this difference. I think in most cases, the function call
and the event should contain the same information.

I [asked](https://hachyderm.io/@rickardlindberg/112174367523991295) James about
this:

> I have a question regarding output tracking.
>
> "Output Trackers should write objects that represent the action that was
> performed, not just the function that was called to perform it."
>
> Shouldn't those in most cases be very similar? I mean, the name of a function
> should match what it does, right? Sure, you can refactor them separately, but
> wouldn't you often want to rename the object written if you rename the
> function?

He replied

> It’s really a prescription against treating the tracker as a Spy that records
> the function name and arguments. You should be able to refactor the API
> without feeling like you have to change the tracker, as long as behavior
> remains the same.

I can see cases where tracking events can be a little more flexible. Take this
logging class for example:

$:output:python:
class Logger:

    def info(self, message):
        self.log("INFO", message)

    def error(self, message):
        self.log("ERROR", message)

    def log(self, level, message):
        self.notify(f"LOG {level} {message}")
        ...
$:END

In application code you call `info` and `error`. But in tests you don't need to
care about which exact method was called. Only that the relevant `LOG ...`
event was emitted.

Perhaps the `SAVE_COMMAND` event should not include raw arguments? Here is the
implementation:

$:output:python:
def run(self, args):
    self.notify(f"SAVE_COMMAND {args!r}")
    if len(args) != 1:
        raise ValueError(f"Expected one argument as the message, but got {args!r}.")
    self.process.run(["git", "commit", "-a", "-m", args[0]])
$:END

Perhaps it makes more sense to record the event with an explicit message like
this?

$:output:python:
def run(self, args):
    if len(args) != 1:
        raise ValueError(f"Expected one argument as the message, but got {args!r}.")
    message = args[0]
    self.notify(f"SAVE_COMMAND message={message}")
    self.process.run(["git", "commit", "-a", "-m", message])
$:END

An assertion would then look like this:

$:output:python:
"""
>>> events
SAVE_COMMAND message=message
"""
$:END

Is this event more relevant from the point of view of the caller? Maybe. In any
case, the event approach is more flexible compared to the mock version.

## More on output tracking

* [How to test a router?](/writing/how-to-test-a-router/index.html)

* [How Are Nullables Different From
  Mocks?](https://www.jamesshore.com/v2/projects/nullables/how-are-nullables-different-from-mocks)

## Appendix: myscm.py

$:code:myscm.py
