class _RLMeta(object):

    def __init__(self, in_stream, out_stream):
        self.in_stream = in_stream
        self.out_stream = out_stream

    def run(self, rule_name):
        result = getattr(self, rule_name)().eval()
        if hasattr(result, "to_rlmeta_output_stream"):
            out_stream.write(result.to_rlmeta_output_stream())
        else:
            out_stream.write(result)
class _Builder(object):

    def to_rlmeta_output_stream(self):
        output = _Output()
        self.write(output)
        return output.value
class _ListBuilder(_Builder):

    def __init__(self, items):
        self.items = [
            item if isinstance(item, _Builder)
                else _AtomBuilder(item)
            for item in items
        ]

    def write(self, output):
        for item in self.items:
            item.write(output)
class _AtomBuilder(_Builder):

    def __init__(self, atom):
        self.atom = atom

    def write(self, output):
        output.write(str(self.atom))
class _IndentBuilder(_Builder):

    def write(self, output):
        output.indent()
class _DedentBuilder(_Builder):

    def write(self, output):
        output.dedent()
class _Output(object):

    def __init__(self):
        self.value = ""
        self.level = 0

    def indent(self):
        self.level += 1

    def dedent(self):
        self.level -= 1

    def write(self, value):
        for ch in value:
            if self.value and ch != "\n" and self.value[-1] == "\n":
                self.value += "    "*self.level
            self.value += ch
