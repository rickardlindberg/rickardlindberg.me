import doctest
import sys

class Trackable:

    def __init__(self):
        self.events = []

    def track_events(self, events):
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
        )

    @classmethod
    def create_null(cls, events):
        return cls(
            save_command=SaveCommand.create_null().track_events(events),
            share_command=ShareCommand.create_null().track_events(events),
            terminal=Terminal.create_null().track_events(events),
        )

    def __init__(self, save_command, share_command, terminal):
        self.save_command = save_command
        self.share_command  = share_command
        self.terminal = terminal

    def run(self, args):
        """
        I dispatch to the correct sub-command:

        >>> events = Events()
        >>> App.create_null(events).run(["save", "message"])
        >>> events
        SAVE_COMMAND ['message']

        >>> events = Events()
        >>> App.create_null(events).run(["share"])
        >>> events
        SHARE_COMMAND []

        >>> events = Events()
        >>> App.create_null(events).run(["unknown", "sub", "command"])
        >>> events
        TERMINAL_WRITE 'Unknown command.'
        """
        if args[0:1] == ["save"]:
            self.save_command.run(args[1:])
        elif args[0:1] == ["share"]:
            self.share_command.run([])
        else:
            self.terminal.write("Unknown command.")

class SaveCommand(Trackable):

    @classmethod
    def create(cls):
        return cls()

    @classmethod
    def create_null(cls):
        return cls()

    def run(self, args):
        """
        >>> SaveCommand.create_null().run([])
        """
        self.notify(f"SAVE_COMMAND {args!r}")

class ShareCommand(Trackable):

    @classmethod
    def create(cls):
        return cls()

    @classmethod
    def create_null(cls):
        return cls()

    def run(self, args):
        """
        >>> ShareCommand.create_null().run([])
        """
        self.notify(f"SHARE_COMMAND {args!r}")

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
        >>> terminal = Terminal.create()
        >>> terminal.track_events(events)
        >>> terminal.write("hello")
        hello
        >>> events
        TERMINAL_WRITE 'hello'

        >>> Terminal.create_null().write("hello")
        """
        self.notify(f"TERMINAL_WRITE {text!r}")
        print(text, file=self.sys.stdout, flush=True)

if __name__ == "__main__":
    doctest.testmod()
    print("OK")
