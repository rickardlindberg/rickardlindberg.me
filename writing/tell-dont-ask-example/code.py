import re


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


if __name__ == "__main__":
    Parser(PrettyPrinter()).parse("example")
