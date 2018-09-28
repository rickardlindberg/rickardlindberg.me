class _RLMeta(object):

    def __init__(self, logger=None):
        self._log = (lambda message: None) if logger is None else logger

    def run(self, rule_name, input_object):
        self._input = _Input.from_object(input_object)
        self._memo = {}
        try:
            result = self._match(rule_name).eval()
            if hasattr(result, "to_rlmeta_output_stream"):
                return result.to_rlmeta_output_stream()
            else:
                return result
        except _MatchError:
            self._dump_memo()
            raise

    def _dump_memo(self):
        items = []
        for (rule_name, _), (_, start, end) in self._memo.items():
            items.append((rule_name, start, end))
        items.sort(key=lambda item: (item[2].as_key(), item[1].as_key()))
        for item in items:
            self._log("matched {: <20} {} -> {}\n".format(*item))

    def _match(self, rule_name):
        key = (rule_name, self._input.as_key())
        if key in self._memo:
            result, _, self._input = self._memo[key]
        else:
            start_input = self._input
            result = getattr(self, "_rule_{}".format(rule_name))()
            self._memo[key] = (result, start_input, self._input)
        return result

    def _or(self, matchers):
        saved_input = self._input
        for matcher in matchers:
            try:
                return matcher()
            except _MatchError:
                self._input = saved_input
        raise _MatchError()

    def _and(self, matchers):
        result = None
        for matcher in matchers:
            result = matcher()
        return result

    def _star(self, matcher):
        result = []
        while True:
            saved_input = self._input
            try:
                result.append(matcher())
            except _MatchError:
                self._input = saved_input
                return _SemanticAction(lambda: [x.eval() for x in result])

    def _negative_lookahead(self, matcher):
        saved_input = self._input
        try:
            matcher()
        except _MatchError:
            return _SemanticAction(lambda: None)
        else:
            raise _MatchError()
        finally:
            self._input = saved_input

    def _match_range(self, a, b):
        next_objext, self._input = self._input.next()
        if next_objext >= a and next_objext <= b:
            return _SemanticAction(lambda: next_objext)
        else:
            raise _MatchError()

    def _match_string(self, string):
        next_object, self._input = self._input.next()
        if next_object == string:
            return _SemanticAction(lambda: string)
        else:
            raise _MatchError()

    def _match_charseq(self, charseq):
        for char in charseq:
            next_object, self._input = self._input.next()
            if next_object != char:
                raise _MatchError()
        return _SemanticAction(lambda: charseq)

    def _any(self):
        next_object, self._input = self._input.next()
        return _SemanticAction(lambda: next_object)

    def _match_list(self, matcher):
        next_object, next_input = self._input.next()
        if isinstance(next_object, list):
            self._input = self._input.nested(next_object)
            matcher()
            if self._input.is_at_end():
                self._input = next_input
                return _SemanticAction(lambda: next_object)
        raise _MatchError()

class _Input(object):

    @classmethod
    def from_object(cls, input_object):
        if isinstance(input_object, basestring):
            return _StringInput(list(input_object))
        else:
            return _ListInput([input_object])

    def __init__(self, objects):
        self._objects = objects

    def next(self):
        if self.is_at_end():
            raise _MatchError()
        next_object = self._objects[0]
        return (
            next_object,
            self._advance(next_object, self._objects[1:]),
        )

    def is_at_end(self):
        return len(self._objects) == 0

class _StringInput(_Input):

    def __init__(self, objects, line=1, column=1):
        _Input.__init__(self, objects)
        self._line = line
        self._column = column

    def as_key(self):
        return (self._line, self._column)

    def _advance(self, next_object, objects):
        if next_object == "\n":
            return _StringInput(objects, self._line+1, 1)
        else:
            return _StringInput(objects, self._line, self._column+1)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)

class _ListInput(_Input):

    def __init__(self, objects, parent=(), pos=0):
        _Input.__init__(self, objects)
        self._parent = parent
        self._pos = pos

    def as_key(self):
        return self._parent + (self._pos,)

    def nested(self, input_object):
        return _ListInput(input_object, self._parent+(self._pos,))

    def _advance(self, next_object, objects):
        return _ListInput(objects, self._parent, self._pos+1)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.as_key()))

class _SemanticAction(object):

    def __init__(self, fn):
        self.fn = fn

    def eval(self):
        return self.fn()

class _MatchError(Exception):
    pass

class _Vars(dict):

    def bind(self, name, value):
        self[name] = value
        return value

    def lookup(self, name):
        return self[name]

class _Builder(object):

    @classmethod
    def create(self, item):
        if isinstance(item, _Builder):
            return item
        elif isinstance(item, list):
            return _ListBuilder([_Builder.create(x) for x in item])
        else:
            return _AtomBuilder(item)

    def to_rlmeta_output_stream(self):
        output = _Output()
        self.write(output)
        return output.value

class _ListBuilder(_Builder):

    def __init__(self, items):
        self.items = items

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
