---
title: Tell, don't ask, example
date: 2016-09-18
---

I recently read *The Pragmatic Programmer*, and in one of the exercises you
should write a parser for a DSL that controls a drawing package. Here is the
example from the book:

    P 2  # select pen 2
    D    # pen down
    W 2  # draw west 2cm
    N 1  # then north 1
    E 2  # then east 2
    S 1  # then back south
    U    # pen up

Here is one solution to this problem (in Python):

```python
class Parser(object):

    def parse(self, file_path):
        with open(file_path) as f:
            result = []
            for line in f:
                result.append(self.parse_line(line))
            return result

    def parse_line(self, line):
        patterns = [
            (r"^P (\d)", lambda match: ("select_pen", int(match.group(1)))),
            (r"^D",      lambda match: ("pen_down",   None)),
            (r"^W (\d)", lambda match: ("west",       int(match.group(1)))),
            (r"^N (\d)", lambda match: ("north",      int(match.group(1)))),
            (r"^E (\d)", lambda match: ("east",       int(match.group(1)))),
            (r"^S (\d)", lambda match: ("south",      int(match.group(1)))),
            (r"^U",      lambda match: ("pen_up",     None)),
        ]
        for (pattern, fn) in patterns:
            match = re.search(pattern, line)
            if match:
                return fn(match)
```

The result of running the parser on the example is the following:

```python
[
    ('select_pen', 2),
    ('pen_down', None),
    ('west', 2),
    ('north', 1),
    ('east', 2),
    ('south', 1),
    ('pen_up', None)
]
```

In this solution, each line is converted to a command object (a tuple) and all
commands are returned in a list. This list of commands is a kind of parse tree,
and it is easier to work with than the plain text. It is up to the consumer of
the parse tree what to do with it next.

Here is an example showing how the parse tree can be pretty printed (formatted
to text again after being parsed):

```python
def pretty_print(commands):
    for (command, argument) in commands:
        if command == "select_pen":
            print("P {}".format(argument))
        elif command == "pen_down":
            print("D")
        elif command == "west":
            print("W {}".format(argument))
        elif command == "north":
            print("N {}".format(argument))
        elif command == "east":
            print("E {}".format(argument))
        elif command == "south":
            print("S {}".format(argument))
        elif command == "pen_up":
            print("U")
```

If run on the example, the following output is obtained:

    P 2
    D
    W 2
    N 1
    E 2
    S 1
    U

The original program is obtained except for all the comments that have been
stripped. Notice that the `pretty_print` function has to know about the
structure of the parse tree: it has to know that it is a list of commands and
that each command is a tuple with two entries (some of which might be `None`).
The lack of a final else-statement is also unsatisfactory.

In object oriented terms, the parser is **asked** for a list of commands. How
would a solution look like if the *Tell, don't ask* principle is applied? Here
is one alternative:

```python
class Parser(object):

    def __init__(self, interpreter):
        self._interpreter = interpreter

    def parse(self, file_path):
        with open(file_path) as f:
            for line in f:
                self.parse_line(line)

    def parse_line(self, line):
        patterns = [
            (r"^P (\d)", lambda match: self._interpreter.select_pen(int(match.group(1)))),
            (r"^D",      lambda match: self._interpreter.pen_down()),
            (r"^W (\d)", lambda match: self._interpreter.west(int(match.group(1)))),
            (r"^N (\d)", lambda match: self._interpreter.north(int(match.group(1)))),
            (r"^E (\d)", lambda match: self._interpreter.east(int(match.group(1)))),
            (r"^S (\d)", lambda match: self._interpreter.south(int(match.group(1)))),
            (r"^U",      lambda match: self._interpreter.pen_up()),
        ]
        for (pattern, fn) in patterns:
            match = re.search(pattern, line)
            if match:
                fn(match)
```

This parser returns nothing. Instead it depends on an interpreter that it
forwards calls to: It **tells** the interpreter to handle various commands. An
interpreter can be any object supporting the command-methods. Here is what the
pretty printer from the first example would look like:

```python
class PrettyPrinter(object):

    def select_pen(self, number):
        print("P {}".format(number))

    def pen_down(self):
        print("D")

    def west(self, amount):
        print("W {}".format(amount))

    def north(self, amount):
        print("N {}".format(amount))

    def east(self, amount):
        print("E {}".format(amount))

    def south(self, amount):
        print("S {}".format(amount))

    def pen_up(self):
        print("U")
```

The pretty printer no longer has to do the looping. It doesn't have to bother
with arguments being `None` either. Instead it just has one method to handle
each type of command.

Try to compare the solutions and think about in what situations one is better
than the other. What has to change if a new command is added? What has to
change to do some actual drawing instead of pretty printing? What if both
drawing and pretty printing must be supported?

See also:

* [http://martinfowler.com/bliki/TellDontAsk.html](http://martinfowler.com/bliki/TellDontAsk.html)
* [https://pragprog.com/articles/tell-dont-ask](https://pragprog.com/articles/tell-dont-ask)
