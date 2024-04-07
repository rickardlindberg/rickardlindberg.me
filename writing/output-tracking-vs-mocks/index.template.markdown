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
app = App(
    save_command=SaveCommand(...),
    share_command=ShareCommand(...),
    terminal=Terminal(...),
    args=Args(...),
)
$:END

However, if we were to invoke methods on `app` now, it would interact with the
outside world. It would read command line arguments, execute `git` commands,
and write to the terminal.

We don't want to do that. It takes a long time and is brittle. We therefore
inject null versions of dependencies like this:

$:output:python:
app = App(
    save_command=SaveCommand.create_null(),
    share_command=ShareCommand.create_null(),
    terminal=Terminal.create_null(),
    args=Args.create_null(),
)
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

Now, `App` only has one method, and the is `run`:

$:output:python:
def run(self):
    ...
$:END

So the only test we can write is this:

$:output:python:
app = App.create_null()
app.run()
$:END

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

## Appendix: myscm.py

$:code:myscm.py
