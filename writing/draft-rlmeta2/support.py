class _RLMeta(object):

    def __init__(self, logger=None):
        self._log = (lambda message: None) if logger is None else logger

    def run(self, rule_name, input_object):
        self._stream = _Stream.from_object(input_object)
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
        key = (rule_name, self._stream.memo_key())
        if key in self._memo:
            result, _, self._stream = self._memo[key]
        else:
            start = self._stream
            result = getattr(self, "_rule_{}".format(rule_name))()
            end = self._stream
            self._memo[key] = (result, start, end)
        return result

    def _or(self, matchers):
        original_stream = self._stream
        for matcher in matchers:
            try:
                return matcher()
            except _MatchError:
                self._stream = original_stream
        raise _MatchError()

    def _and(self, matchers):
        result = None
        for matcher in matchers:
            result = matcher()
        return result

    def _star(self, matcher):
        result = []
        while True:
            original_stream = self._stream
            try:
                result.append(matcher())
            except _MatchError:
                self._stream = original_stream
                return _SemanticAction(lambda: [x.eval() for x in result])

    def _negative_lookahead(self, matcher):
        original_stream = self._stream
        try:
            matcher()
        except _MatchError:
            return _SemanticAction(lambda: None)
        else:
            raise _MatchError()
        finally:
            self._stream = original_stream

    def _match_range(self, start, end):
        next_objext, self._stream = self._stream.next()
        if next_objext >= start and next_objext <= end:
            return _SemanticAction(lambda: next_objext)
        else:
            raise _MatchError()

    def _match_string(self, string):
        next_object, self._stream = self._stream.next()
        if next_object == string:
            return _SemanticAction(lambda: string)
        else:
            raise _MatchError()

    def _match_charseq(self, charseq):
        for char in charseq:
            next_object, self._stream = self._stream.next()
            if next_object != char:
                raise _MatchError()
        return _SemanticAction(lambda: charseq)

    def _any(self):
        next_object, self._stream = self._stream.next()
        return _SemanticAction(lambda: next_object)

    def _match_list(self, matcher):
        next_object, next_input = self._stream.next()
        if isinstance(next_object, list):
            self._stream = self._stream.nested(next_object)
            matcher()
            if self._stream.is_at_end():
                self._stream = next_input
                return _SemanticAction(lambda: next_object)
        raise _MatchError()

class _Stream(object):

    @classmethod
    def from_object(cls, input_object):
        if isinstance(input_object, basestring):
            return _CharStream(list(input_object))
        else:
            return _ObjectStream([input_object])

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

class _CharStream(_Stream):

    def __init__(self, objects, line=1, column=1):
        _Stream.__init__(self, objects)
        self._line = line
        self._column = column

    def memo_key(self):
        return (self._line, self._column)

    def _advance(self, next_object, objects):
        if next_object == "\n":
            return _CharStream(objects, self._line+1, 1)
        else:
            return _CharStream(objects, self._line, self._column+1)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)

class _ObjectStream(_Stream):

    def __init__(self, objects, parent=(), pos=0):
        _Stream.__init__(self, objects)
        self._parent = parent
        self._pos = pos

    def memo_key(self):
        return self._parent + (self._pos,)

    def nested(self, input_object):
        return _ObjectStream(input_object, self._parent+(self._pos,))

    def _advance(self, next_object, objects):
        return _ObjectStream(objects, self._parent, self._pos+1)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.memo_key()))

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
