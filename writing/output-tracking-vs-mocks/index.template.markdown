---
title: Output Tracking vs Mocks
date: 2024-04-02
tags: draft
---

In this blog post we're going to explore how to write and test a Git client
using the [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
approach. Specifically we're going to focus on [Output
Tracking](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#output-tracking)
and explore how to apply it to this example.

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
App --+--> SaveCommand --+--> Process
      |                  |
      |                  +--> Filesystem
      |
      +--> ShareCommand ----> Process
      |
      +--> Args
      |
      +--> Terminal
```

* `Process`, `Filesystem`, `Args`, and `Terminal` are low-level [infrastructure
  wrappers](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#infrastructure-wrappers)
  that are made
  [nullable](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#nullables)
  using [embedded
  stubs](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#embedded-stub).

    * `Process` is for running external processes. (`git` in this example.)
    * `Filesystem` is for reading file contents from disk.
    * `Args` is for reading command line arguments.
    * `Terminal` is for writing text to the terminal.

* `SaveCommand` and `ShareCommand` are application code that performs a
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

* When the application is called with `["save", "message"]`, then `git commit
  -a -m message` is called.

* When the application is called with `["share"]`, then `git push` is called.

In order to write those test, we need a way to control the outside world to
simulate that a given set of command line arguments are present. We also need a
way to observe what commands would be run (if we were not using the
null-version).

We can solve the first part by passing command line arguments to simulate to
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
# How to assert that "git commit" was called?

>>> app = App.create_null(args=["share"])
>>> app.run()
# How to assert that "git push" was called?
"""
$:END

And now we come to the main topic of this blog post: output tracking.

`App` performs action by delegating to `SaveCommand` and `ShareCommand`. Both
of them take the rest of the command line arguments and performs an action
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

Let's contrast how the first test case can be written using mocks and stubs
instead. Here it is again:

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

The share command and terminal are not exercised in this test, so we just
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
conditions, we also have to write a contract test that checks that the
dependency can actually except those arguments and return those things under
said conditions. That seems like a whole lot more work to me.

## Recording function calls vs actions

Another more subtle difference is..

## Notes

See also [How to test a router?](/writing/how-to-test-a-router/index.html)

See also [Favor real dependencies for unit
testing](https://stackoverflow.blog/2022/01/03/favor-real-dependencies-for-unit-testing/)

* Don't mock internal dependencies vs output tracking

* p.117

    * Event: the action that is performed.

    * Output tracking (invisible writes). Write to external system.

    * Track writes in terms of behaviors your callers care about.

        * Logger writes to stdout. (Write string.)

        * Callers care about data written. (Track data.)

* p.123

* Functional Core / Imperative Shell. Functional core returns decision that
  imperative shell executes.

## Appendix: myscm.py

$:code:myscm.py
