#!/usr/bin/env python

from unittest.mock import Mock
import doctest
import subprocess
import sys

class Trackable:

    def __init__(self):
        self.events = []

    def track_events(self, events):
        if events:
            self.events.append(events)
        return self

    def notify(self, event):
        for events in self.events:
            events.append(event)

class Events:

    def __init__(self):
        self.events = []

    def append(self, event):
        self.events.append(event)

    def __repr__(self):
        return "\n".join(self.events)

class App:

    @classmethod
    def create(cls):
        """
        >>> isinstance(App.create(), App)
        True
        """
        return cls(
            save_command=SaveCommand.create(),
            share_command=ShareCommand.create(),
            terminal=Terminal.create(),
            args=Args.create(),
        )

    @classmethod
    def create_null(cls, events=None, args=[]):
        return cls(
            save_command=SaveCommand.create_null().track_events(events),
            share_command=ShareCommand.create_null().track_events(events),
            terminal=Terminal.create_null().track_events(events),
            args=Args.create_null(args=args),
        )

    def __init__(self, save_command, share_command, terminal, args):
        self.save_command = save_command
        self.share_command  = share_command
        self.terminal = terminal
        self.args = args

    def run(self):
        """
        >>> events = Events()
        >>> App.create_null(events, args=["save", "message"]).run()
        >>> events
        SAVE_COMMAND ['message']

        >>> save_command_mock = Mock()
        >>> App(
        ...     save_command=save_command_mock,
        ...     share_command=None,
        ...     terminal=None,
        ...     args=Mock(**{"get.return_value": ["save", "message"]})
        ... ).run()
        >>> save_command_mock.run.call_args_list
        [call(['message'])]

        >>> App.create_null(Events(), args=["save"]).run()
        Traceback (most recent call last):
          ...
        ValueError: Expected one argument as the message, but got [].

        >>> save_command_mock = Mock()
        >>> App(
        ...     save_command=save_command_mock,
        ...     share_command=None,
        ...     terminal=None,
        ...     args=Mock(**{"get.return_value": ["save"]})
        ... ).run()
        >>> save_command_mock.run.call_args_list
        [call([])]

        >>> events = Events()
        >>> App.create_null(events, args=["share"]).run()
        >>> events
        SHARE_COMMAND []

        >>> events = Events()
        >>> App.create_null(events, args=["unknown", "sub", "command"]).run()
        >>> events
        TERMINAL_WRITE 'Unknown command.'
        """
        args = self.args.get()
        if args[0:1] == ["save"]:
            self.save_command.run(args[1:])
        elif args[0:1] == ["share"]:
            self.share_command.run([])
        else:
            self.terminal.write("Unknown command.")

class SaveCommand(Trackable):

    @classmethod
    def create(cls):
        return cls(
            process=Process.create()
        )

    @classmethod
    def create_null(cls, events=None):
        return cls(
            process=Process.create_null().track_events(events)
        )

    def __init__(self, process):
        Trackable.__init__(self)
        self.process = process

    def run(self, args):
        """
        >>> events = Events()
        >>> SaveCommand.create_null().track_events(events).run(["message"])
        >>> events
        SAVE_COMMAND ['message']

        >>> events = Events()
        >>> SaveCommand.create_null(events=events).run(['message'])
        >>> events
        PROCESS_RUN ['git', 'commit', '-a', '-m', 'message']

        >>> SaveCommand.create_null().run(['message', '--force'])
        Traceback (most recent call last):
          ...
        ValueError: Expected one argument as the message, but got ['message', '--force'].
        """
        self.notify(f"SAVE_COMMAND {args!r}")
        if len(args) != 1:
            raise ValueError(f"Expected one argument as the message, but got {args!r}.")
        self.process.run(["git", "commit", "-a", "-m", args[0]])

class ShareCommand(Trackable):

    @classmethod
    def create(cls):
        return cls(
            process=Process.create()
        )

    @classmethod
    def create_null(cls, events=None):
        return cls(
            process=Process.create_null().track_events(events)
        )

    def __init__(self, process):
        Trackable.__init__(self)
        self.process = process

    def run(self, args):
        """
        >>> events = Events()
        >>> ShareCommand.create_null(events=events).run([])
        >>> events
        PROCESS_RUN ['git', 'push']
        """
        self.notify(f"SHARE_COMMAND {args!r}")
        self.process.run(["git", "push"])

class Terminal(Trackable):

    @classmethod
    def create(cls):
        return cls(sys=sys)

    @classmethod
    def create_null(cls):
        class NullStream:
            def write(self, text):
                pass
            def flush(self):
                pass
        class NullSysModule:
            stdout = NullStream()
        return cls(sys=NullSysModule())

    def __init__(self, sys):
        Trackable.__init__(self)
        self.sys = sys

    def write(self, text):
        """
        >>> events = Events()
        >>> Terminal.create().track_events(events).write("hello")
        hello
        >>> events
        TERMINAL_WRITE 'hello'

        >>> Terminal.create_null().write("hello")
        """
        self.notify(f"TERMINAL_WRITE {text!r}")
        print(text, file=self.sys.stdout, flush=True)

class Args(Trackable):

    @classmethod
    def create(cls):
        return cls(sys=sys)

    @classmethod
    def create_null(cls, args):
        class NullSysModule:
            argv = ["null program"]+args
        return cls(sys=NullSysModule())

    def __init__(self, sys):
        Trackable.__init__(self)
        self.sys = sys

    def get(self):
        """
        >>> Args.create().get()
        ['--test']

        >>> Args.create_null(args=["configured", "args"]).get()
        ['configured', 'args']
        """
        return self.sys.argv[1:]

class Process(Trackable):

    @classmethod
    def create(cls):
        return cls(subprocess=subprocess)

    @classmethod
    def create_null(cls):
        class NullSubprocessModule:
            def call(self, command):
                pass
        return cls(subprocess=NullSubprocessModule())

    def __init__(self, subprocess):
        Trackable.__init__(self)
        self.subprocess = subprocess

    def run(self, command):
        """
        >>> events = Events()
        >>> Process.create().track_events(events).run(["echo", "hello"])
        >>> events
        PROCESS_RUN ['echo', 'hello']

        >>> Process.create_null().run(["echo", "hello"])
        """
        self.notify(f"PROCESS_RUN {command!r}")
        self.subprocess.call(command)

if __name__ == "__main__":
    if Args.create().get() == ["--test"]:
        doctest.testmod()
        print("OK")
    else:
        App.create().run()
