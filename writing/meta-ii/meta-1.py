import re
import sys


class MaybeParseError(Exception):
    pass


class Compiler(object):

    def compile(self, program):
        self._input = program
        self._pos = 0
        self._output = ""
        self._level = 0
        try:
            self.program()
        except MaybeParseError as e:
            sys.stderr.write("ERROR: {}".format(e))
            sys.stderr.write("\n==== REST ====\n")
            sys.stderr.write(self._input[self._pos:self._pos+10])
            sys.stderr.write("\n=== OUTPUT ===\n")
            sys.stderr.write(self._output)
            sys.stderr.write("\n==============\n")
        return self._output

    def _re(self, pattern):
        match = re.match(r"\s*({})".format(pattern), self._input[self._pos:])
        if match:
            self._pos += len(match.group(0))
            return match.group(1)
        raise MaybeParseError("re not found")

    def _backup(self):
        return (self._pos, self._output, self._level)

    def _restore(self, backup):
        self._pos, self._output, self._level = backup

    def _write(self, text):
        for ch in text:
            if self._output and self._output[-1] == "\n" and ch != "\n":
                self._output += "    " * self._level
            self._output += ch

    def _indent(self):
        self._level += 1

    def _dedent(self):
        self._level -= 1


class Meta1(Compiler):

    def program(self):
        try:
            backup = self._backup()
            match = self._re("compiler")
            match = self._re("[a-zA-Z0-9]+")
            self._write("class ")
            self._write(match)
            self._write("(Compiler):\n")
            self._indent()
            match = self._re("\\{")
            self.rules()
            match = self._re("\\}")
            self._dedent()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def rules(self):
        try:
            backup = self._backup()
            self.rule()
            self.moreRules()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def moreRules(self):
        try:
            backup = self._backup()
            self.rules()
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            self.empty()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def rule(self):
        try:
            backup = self._backup()
            match = self._re("[a-zA-Z]+")
            self._write("\ndef ")
            self._write(match)
            self._write("(self):\n")
            self._indent()
            match = self._re("=")
            self.choices()
            self._write("raise MaybeParseError('no choice found')\n")
            self._dedent()
            match = self._re(";")
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def choices(self):
        try:
            backup = self._backup()
            self.choice()
            self.moreChoices()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def moreChoices(self):
        try:
            backup = self._backup()
            match = self._re("\\|")
            self.choices()
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            self.empty()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def choice(self):
        try:
            backup = self._backup()
            self._write("try:\n")
            self._indent()
            self._write("backup = self._backup()\n")
            self.sequence()
            self._write("return\n")
            self._dedent()
            self._write("except MaybeParseError:\n")
            self._indent()
            self._write("self._restore(backup)\n")
            self._dedent()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def sequence(self):
        try:
            backup = self._backup()
            self.part()
            self.moreParts()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def moreParts(self):
        try:
            backup = self._backup()
            self.sequence()
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            self.empty()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def part(self):
        try:
            backup = self._backup()
            self.output()
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            match = self._re("\x22.*?\x22")
            self._write("match = self._re(")
            self._write(match)
            self._write(")\n")
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            match = self._re("[a-zA-Z]+")
            self._write("self.")
            self._write(match)
            self._write("()\n")
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def output(self):
        try:
            backup = self._backup()
            match = self._re("{")
            self.outParts()
            match = self._re("}")
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def outParts(self):
        try:
            backup = self._backup()
            self.outPart()
            self.moreOutParts()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def moreOutParts(self):
        try:
            backup = self._backup()
            self.outParts()
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            self.empty()
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def outPart(self):
        try:
            backup = self._backup()
            match = self._re("\x22.*?\x22")
            self._write("self._write(")
            self._write(match)
            self._write(")\n")
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            match = self._re("%")
            self._write("self._write(match)\n")
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            match = self._re(">")
            self._write("self._indent()\n")
            return
        except MaybeParseError:
            self._restore(backup)
        try:
            backup = self._backup()
            match = self._re("<")
            self._write("self._dedent()\n")
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')

    def empty(self):
        try:
            backup = self._backup()
            match = self._re("")
            return
        except MaybeParseError:
            self._restore(backup)
        raise MaybeParseError('no choice found')


if __name__ == "__main__":
    sys.stdout.write(Meta1().compile(sys.stdin.read()))
