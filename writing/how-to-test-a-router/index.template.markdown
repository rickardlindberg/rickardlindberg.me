---
title: How to test a router?
date: 2023-05-20
---

I've been practicing [Testing Without
Mocks](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks)
for a while now. It describes a way of doing overlapping, sociable testing,
which include infrastructure, without having side effects occur in your
tests.

Recently I've been wondering how to test a "router" using this pattern. By
router I mean an entry level function that looks at a url or command line
arguments or whatever and dispatches to the relevant "controller". Something
like this:

$:output:python:
class MyWebApp:

    def main(self, url):
        if url.startswith("/home"):
            self.home_controller.run(...)
        elif url.startswith("/about"):
            self.about_controller.run(...)
$:END

I [asked](https://hachyderm.io/@rickardlindberg/110379826738876668) James if he
had any examples of this, and he
[had](https://github.com/jamesshore/testing-without-mocks-complex/tree/javascript/src/www).
Let's explore.

## James' example

Overly simplified, to only highlight the parts that I'm interested in, James'
example looks like this:

$:output:python:
class WwwRouter:

    def __init__(self, home_page_controller):
        self.home_page_controller = home_page_controller

    def route(self, url):
        if url == "/":
            return self.home_page_controller.get()
        else:
            return "FAIL"

class HomePageController:

    @staticmethod
    def create():
        return HomePageController(SomeInfrastructure.create())

    @staticmethod
    def create_null():
        return HomePageController(SomeInfrastructure.create_null())

    def __init__(self, some_infrastructure):
        self.some_infrastructure = some_infrastructure

    def get(self):
        ...
        return "Home Page"
$:END

The question I had was, how to test the `WwwRouter`? James does it like this:

$:output:python:
def test_routes_home_page():
    router = WwwRouter(HomePageController.create_null())
    response = router.route("/")
    assert response == HomePageController.create_null().get()

def test_routes_errors():
    router = WwwRouter(HomePageController.create_null())
    response = router.route("/no-such-url")
    assert response == "FAIL"
$:END

Some characteristics of this test setup:

* The router takes all the controllers as dependencies.

* The null version of the controllers are used.

* The test uses [collaborator-based
  isolation](https://www.jamesshore.com/v2/projects/nullables/testing-without-mocks#isolation).
  (The test doesn't care what `HomePageController` returns as long as it is the
  same as the router returns.)

## What if return value is missing?

How about a router or dispatcher where the controllers don't return anything.
How to we test that?

Example:

$:output:python:
def CliDispatcher:

    def __init__(self, add_command, remove_command):
        self.add_command = add_command
        self.remove_command = remove_command

    def dispatch(self, arguments):
        if arguments[:1] == ["add"]:
            self.add_command.run(arguments[1:])
        elif arguments[:1] == ["remove"]:
            self.remove_command.run(arguments[1:])
        else:
            sys.exit("Unknown command.")
$:END

We start out the same:

$:output:python:
def test_dispatches_to_add():
    add_command = AddCommand.create_null()
    remove_command = RemoveCommand.create_null()
    cli = CliDispatcher(add_command, remove_command)
    cli.dispatch(["add", "item name"])
$:END

But `dispatch` does not return anything, so we can't check any return value.
What to do?

We want to test that the function of `AddCommand` was performed, and nothing
else. We could introduce queries on commands to see if they have been run. Then
we can write the asserts like this:

$:output:python:
assert add_command.get_last_arguments() == ["item name"]
assert remove_command.get_last_arguments() == None
$:END

So we want to assert that the add command was run with the given arguments and
that all the other (only one in the example) commands were not run.

I think this is in the spirit of testing without mocks. Here is what it says
about state-based testing:

> Use state-based tests instead of interaction-based tests. A state-based test
> checks the output or state of the code under test, without any awareness of
> its implementation.

An it goes on to say

> For mutable objects, provide a way for changes in state to be observed,
> either with a getter method or an event.

## Events instead

I am not a fan of the `get_last_*` pattern. Our code could call
`command.run(...)` twice, and the test would not catch the error. I prefer an
events approach instead.

Assuming that commands are observable and that they emit events when run (we
can test that separately), the test can be written like this:

$:output:python:
def test_dispatches_to_add():
    events = []
    add_command = AddCommand.create_null()
    add_command.on_event(events.append)
    remove_command = RemoveCommand.create_null()
    remove_command.on_event(events.append)
    cli = CliDispatcher(add_command, remove_command)
    cli.dispatch(["add", "item name"])
    assert events == [
        {"name": "AddCommand", "arguments": ["item name"]},
    ]
$:END

This test ensures that exactly one command was run and that it was run
only once. Exactly what we wanted to test, but now expressed with a single
assert.

## Better test setup

This test setup becomes tedious to do for every command. We can extract it to a
factory method:

$:output:python:
def create_cli():
    events = []
    add_command = AddCommand.create_null()
    add_command.on_event(events.append)
    remove_command = RemoveCommand.create_null()
    remove_command.on_event(events.append)
    cli = CliDispatcher(add_command, remove_command)
    return cli, events

def test_dispatches_to_add():
    cli, events = create_cli()
    cli.dispatch(["add", "item name"])
    assert events == [
        {"name": "AddCommand", "arguments": ["item name"]},
    ]

def test_dispatches_to_remove():
    cli, events = create_cli()
    cli.dispatch(["remove", "item name"])
    assert events == [
        {"name": "RemoveCommand", "arguments": ["item name"]},
    ]
$:END

Or even better:

$:output:python:
def run_in_test_mode(arguments):
    events = []
    add_command = AddCommand.create_null()
    add_command.on_event(events.append)
    remove_command = RemoveCommand.create_null()
    remove_command.on_event(events.append)
    cli = CliDispatcher(add_command, remove_command)
    cli.dispatch(arguments)
    return events

def test_dispatches_to_add():
    assert run_in_test_mode(["add", "item name"]) == [
        {"name": "AddCommand", "arguments": ["item name"]},
    ]

def test_dispatches_to_remove():
    assert run_in_test_mode(["remove", "item name"]) == [
        {"name": "RemoveCommand", "arguments": ["item name"]},
    ]
$:END

## Summary

In hindsight, this seems quite obvious to me. I'm not sure what I had a hard
time understanding. But the example from James helped. Thanks! And writing this
blog post helped me clarify my thinking on the subject.
