try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

class _Grammar(object):

    def _or(self, matchers):
        original_stream = self._stream
        for matcher in matchers:
            try:
                return matcher()
            except _MatchError:
                self._stream = original_stream
        original_stream.fail("no choice matched")

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

    def _not(self, matcher):
        original_stream = self._stream
        try:
            matcher()
        except _MatchError:
            return _SemanticAction(lambda: None)
        else:
            original_stream.fail("match found")
        finally:
            self._stream = original_stream

    def _match_rule(self, rule_name):
        key = (rule_name, self._stream.position())
        if key in self._memo:
            result, _, self._stream = self._memo[key]
        else:
            start = self._stream
            result = getattr(self, "_rule_{}".format(rule_name))()
            end = self._stream
            self._memo[key] = (result, start, end)
        return result

    def _match_range(self, start, end):
        original_stream = self._stream
        next_objext, self._stream = self._stream.next()
        if next_objext >= start and next_objext <= end:
            return _SemanticAction(lambda: next_objext)
        else:
            original_stream.fail(
                "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)
            )

    def _match_string(self, string):
        original_stream = self._stream
        next_object, self._stream = self._stream.next()
        if next_object == string:
            return _SemanticAction(lambda: string)
        else:
            original_stream.fail(
                "expected {!r} but found {!r}".format(string, next_object)
            )

    def _match_charseq(self, charseq):
        for char in charseq:
            original_stream = self._stream
            next_object, self._stream = self._stream.next()
            if next_object != char:
                original_stream.fail(
                    "expected {!r} but found {!r}".format(char, next_object)
                )
        return _SemanticAction(lambda: charseq)

    def _match_any(self):
        next_object, self._stream = self._stream.next()
        return _SemanticAction(lambda: next_object)

    def _match_call_rule(self):
        next_object, self._stream = self._stream.next()
        return self._match_rule(str(next_object))

    def _match_list(self, matcher):
        original_stream = self._stream
        next_object, next_stream = self._stream.next()
        if isinstance(next_object, list):
            self._stream = self._stream.nested(next_object)
            matcher()
            if self._stream.is_at_end():
                self._stream = next_stream
                return _SemanticAction(lambda: next_object)
        original_stream.fail("list match failed")

    def run(self, rule_name, input_object):
        self._memo = _Memo()
        self._stream = _Stream.from_object(self._memo, input_object)
        result = self._match_rule(rule_name).eval()
        if isinstance(result, _Builder):
            return result.build_string()
        else:
            return result

class _Vars(dict):

    def bind(self, name, value):
        self[name] = value
        return value

    def lookup(self, name):
        return self[name]

class _SemanticAction(object):

    def __init__(self, fn):
        self.fn = fn

    def eval(self):
        return self.fn()

class _Builder(object):

    def build_string(self):
        output = _Output()
        self.write(output)
        return output.value

    @classmethod
    def create(self, item):
        if isinstance(item, _Builder):
            return item
        elif isinstance(item, list):
            return _ListBuilder([_Builder.create(x) for x in item])
        else:
            return _AtomBuilder(item)

class _Output(object):

    def __init__(self):
        self.buffer = StringIO()
        self.indentation = 0
        self.on_newline = True

    @property
    def value(self):
        return self.buffer.getvalue()

    def write(self, value):
        for ch in value:
            is_linebreak = ch == "\n"
            if self.indentation and self.on_newline and not is_linebreak:
                self.buffer.write("    "*self.indentation)
            self.buffer.write(ch)
            self.on_newline = is_linebreak

class _ListBuilder(_Builder):

    def __init__(self, builders):
        self.builders = builders

    def write(self, output):
        for builder in self.builders:
            builder.write(output)

class _AtomBuilder(_Builder):

    def __init__(self, atom):
        self.atom = atom

    def write(self, output):
        output.write(str(self.atom))

class _IndentBuilder(_Builder):

    def write(self, output):
        output.indentation += 1

class _DedentBuilder(_Builder):

    def write(self, output):
        output.indentation -= 1

class _Memo(dict):

    def __init__(self):
        dict.__init__(self)
        self._latest_stream = _ObjectStream(self, [], 0, position=-1)
        self._latest_message = ""

    def describe(self):
        items = []
        for (rule_name, _), (_, start, end) in self.items():
            if end > start:
                items.append((rule_name, start, end))
        items.sort(key=lambda item: (item[2].position(), item[1].position()))
        message = []
        for item in items:
            message.append("matched {: <20} {} -> {}\n".format(*item))
        message.append("\n")
        message.append("ERROR: {}: {}\n".format(
            self._latest_stream,
            self._latest_message
        ))
        return "".join(message)

    def fail(self, stream, message):
        if stream.position() >= self._latest_stream.position():
            self._latest_stream = stream
            self._latest_message = message
        raise _MatchError(self)

class _MatchError(Exception):

    def __init__(self, memo):
        Exception.__init__(self)
        self._memo = memo

    def describe(self):
        return self._memo.describe()

class _Stream(object):

    @classmethod
    def from_object(cls, memo, input_object):
        if isinstance(input_object, basestring):
            return _CharStream(memo, input_object, 0)
        else:
            return _ObjectStream(memo, [input_object], 0)

    def __init__(self, memo, objects, index):
        self._memo = memo
        self._objects = objects
        self._index = index

    def fail(self, message):
        self._memo.fail(self, message)

    def next(self):
        if self.is_at_end():
            self.fail("not eof")
        return (self._objects[self._index], self._advance())

    def is_at_end(self):
        return self._index >= len(self._objects)

class _CharStream(_Stream):

    def __init__(self, memo, objects, index, line=1, column=1):
        _Stream.__init__(self, memo, objects, index)
        self._line = line
        self._column = column

    def position(self):
        return (self._line, self._column)

    def _advance(self):
        if self._objects[self._index] == "\n":
            line = self._line + 1
            column = 1
        else:
            line = self._line
            column = self._column + 1
        return _CharStream(self._memo, self._objects, self._index+1, line, column)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)

class _ObjectStream(_Stream):

    def __init__(self, memo, objects, index, parent=(), position=0):
        _Stream.__init__(self, memo, objects, index)
        self._parent = parent
        self._position = position

    def position(self):
        return self._parent + (self._position,)

    def nested(self, input_object):
        return _ObjectStream(self._memo, input_object, 0, self._parent+(self._position,))

    def _advance(self):
        return _ObjectStream(self._memo, self._objects, self._index+1, self._parent, self._position+1)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.position()))
