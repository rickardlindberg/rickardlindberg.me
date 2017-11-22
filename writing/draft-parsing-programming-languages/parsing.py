# expr = level1 eof
#
# level1
#  = level2 ("+" level2 { "add()\n" })*
#  | level2 ("-" level2 { "sub()\n" })*
#
# level2
#  = level3 ("*" level3 { "mult()\n" })*
#  | level3 ("/" level3 { "div()\n"  })*
#
# level3
#  = number { "push(" % ")\n" }
#  | "(" expr ")"


class MaybeParseError(Exception):
    pass


class BaseGrammar(object):

    def parse(self, text):
        self._input = text
        self._input_pos = 0
        self._output = []
        try:
            self.main()
            return "".join(self._output)
        except MaybeParseError as e:
            print("+++")
            print("".join(self._output))
            print("ERROR: {}".format(e))
            print("+++")

    def _number(self):
        self._choice([
            lambda: self._text("0"),
            lambda: self._text("1"),
            lambda: self._text("2"),
            lambda: self._text("3"),
            lambda: self._text("4"),
            lambda: self._text("5"),
            lambda: self._text("6"),
            lambda: self._text("7"),
            lambda: self._text("8"),
            lambda: self._text("9"),
        ])

    def _text(self, text):
        if self._input[self._input_pos:].startswith(text):
            self._input_pos += len(text)
            self._last_match = text
        else:
            raise MaybeParseError("No match for {}".format(text))

    def _write(self, text):
        self._output.append(text)

    def _write_last(self):
        self._write(self._last_match)

    def _star(self, parser):
        while True:
            try:
                savepoint = self._save()
                parser()
            except MaybeParseError:
                self._restore(savepoint)
                return

    def _eof(self):
        if len(self._input[self._input_pos:]) == 0:
            return
        else:
            raise MaybeParseError("Not at end.")

    def _choice(self, parsers):
        for parser in parsers:
            try:
                savepoint = self._save()
                parser()
                return
            except MaybeParseError:
                self._restore(savepoint)
        raise MaybeParseError("No choice matched")

    def _save(self):
        return self._input_pos, len(self._output)

    def _restore(self, savepoint):
        input_pos, output_len = savepoint
        self._input_pos = input_pos
        self._output = self._output[:output_len]


class Grammar(BaseGrammar):

    def main(self):
        self.expr()

    def expr(self):
        self.level1()
        self._eof()

    def level1(self):
        def level1_1():
            self.level2()
            def level1_1_1():
                self._text("+")
                self.level2()
                self._write("add()\n")
            self._star(level1_1_1)
        def level1_2():
            self.level2()
            def level1_2_1():
                self._text("-")
                self.level2()
                self._write("sub()\n")
            self._star(level1_2_1)
        self._choice([level1_1, level1_2])

    def level2(self):
        def level2_1():
            self.level3()
            def level2_1_1():
                self._text("*")
                self.level3()
                self._write("mult()\n")
            self._star(level2_1_1)
        def level2_2():
            self.level3()
            def level2_2_1():
                self._text("/")
                self.level3()
                self._write("div()\n")
            self._star(level2_2_1)
        self._choice([level2_1, level2_2])

    def level3(self):
        def level3_1():
            self._number()
            self._write("push(")
            self._write_last()
            self._write(")\n")
        def level3_2():
            self._text("(")
            self.expr()
            self._text(")")
        self._choice([level3_1, level3_2])


print(Grammar().parse("3+8*5"))
