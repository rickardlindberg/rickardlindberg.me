class _Grammar(object):

    def _or(self, matchers):
        original_stream = self._stream
        var_len = len(self._vars)
        for matcher in matchers:
            try:
                return matcher()
            except _MatchError:
                self._stream = original_stream
                self._vars = self._vars[:var_len]
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
            var_len = len(self._vars)
            try:
                result.append(matcher())
            except _MatchError:
                self._stream = original_stream
                self._vars = self._vars[:var_len]
                return _SemanticAction(lambda _vars: [x.eval() for x in result], self._vars[-1])

    def _not(self, matcher):
        original_stream = self._stream
        var_len = len(self._vars)
        try:
            matcher()
        except _MatchError:
            return _SemanticAction(lambda _vars: None, self._vars[-1])
        else:
            original_stream.fail("match found")
        finally:
            self._stream = original_stream
            self._vars = self._vars[:var_len]

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
            return _SemanticAction(lambda _vars: next_objext, self._vars[-1])
        else:
            original_stream.fail(
                "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)
            )

    def _match_string(self, string):
        original_stream = self._stream
        next_object, self._stream = self._stream.next()
        if next_object == string:
            return _SemanticAction(lambda _vars: string, self._vars[-1])
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
        return _SemanticAction(lambda _vars: charseq, self._vars[-1])

    def _match_any(self):
        next_object, self._stream = self._stream.next()
        return _SemanticAction(lambda _vars: next_object, self._vars[-1])

    def _match_list(self, matcher):
        original_stream = self._stream
        next_object, next_stream = self._stream.next()
        if isinstance(next_object, list):
            self._stream = self._stream.nested(next_object)
            matcher()
            if self._stream.is_at_end():
                self._stream = next_stream
                return _SemanticAction(lambda _vars: next_object, self._vars[-1])
        original_stream.fail("list match failed")

    def _new_label(self):
        self._label_counter += 1
        return self._label_counter

    def run(self, rule_name, input_object):
        self._label_counter = 0
        self._vars = [_Vars()]
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

    def __init__(self, fn, vars_):
        self.fn = fn
        self.vars = vars_

    def eval(self):
        return self.fn(self.vars)

class _Label(object):

    def __init__(self, grammar):
        self.grammar = grammar
        self.number = None

    def eval(self):
        if self.number is None:
            self.number = self.grammar._new_label()
        return self.number

class _Builder(object):

    def build_string(self):
        output = _Output()
        self.write(output)
        return output.flatten()

    @classmethod
    def create(self, item, at=None):
        if at:
            return _AtBuilder(at, _Builder.create(item))
        if isinstance(item, _Builder):
            return item
        elif isinstance(item, list):
            return _ListBuilder([_Builder.create(x) for x in item])
        else:
            return _AtomBuilder(item)

class _Output(object):

    def __init__(self, parent=None):
        self.parent = parent
        self.parts = []
        self.forks = {}

    def indent(self):
        self.write(1)

    def dedent(self):
        self.write(-1)

    def write(self, value):
        self.parts.append(value)

    def fork(self, name):
        fork = _Output(self)
        self.write(fork)
        self.forks[name] = fork

    def get(self, fork):
        if fork in self.forks:
            return self.forks[fork]
        elif self.parent is not None:
            self.parent.get(fork)
        else:
            raise Exception("fork {} not found".format(fork))

    def flatten(self):
        parts = []
        self._flatten(parts, 0)
        return "".join(parts)

    def _flatten(self, parts, indentation):
        for part in self.parts:
            if part == -1:
                indentation -= 1
            elif part == 1:
                indentation += 1
            elif isinstance(part, _Output):
                part._flatten(parts, indentation)
            else:
                for ch in part:
                    if not parts or (ch != "\n" and parts[-1] == "\n"):
                        parts.append("    "*indentation)
                    parts.append(ch)

class _AtBuilder(_Builder):

    def __init__(self, fork, builder):
        self.fork = fork
        self.builder = builder

    def write(self, output):
        self.builder.write(output.get(self.fork))

class _ForkBuilder(_Builder):

    def __init__(self, name):
        self.name = name

    def write(self, output):
        output.fork(self.name)

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
        output.indent()

class _DedentBuilder(_Builder):

    def write(self, output):
        output.dedent()

class _Memo(dict):

    def __init__(self):
        dict.__init__(self)
        self._latest_stream = _ObjectStream(self, [], position=-1)
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
            return _CharStream(memo, list(input_object))
        else:
            return _ObjectStream(memo, [input_object])

    def __init__(self, memo, objects):
        self._memo = memo
        self._objects = objects

    def fail(self, message):
        self._memo.fail(self, message)

    def next(self):
        if self.is_at_end():
            self.fail("not eof")
        next_object = self._objects[0]
        return (
            next_object,
            self._advance(next_object, self._objects[1:]),
        )

    def is_at_end(self):
        return len(self._objects) == 0

class _CharStream(_Stream):

    def __init__(self, memo, objects, line=1, column=1):
        _Stream.__init__(self, memo, objects)
        self._line = line
        self._column = column

    def position(self):
        return (self._line, self._column)

    def _advance(self, next_object, objects):
        if next_object == "\n":
            return _CharStream(self._memo, objects, self._line+1, 1)
        else:
            return _CharStream(self._memo, objects, self._line, self._column+1)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)

class _ObjectStream(_Stream):

    def __init__(self, memo, objects, parent=(), position=0):
        _Stream.__init__(self, memo, objects)
        self._parent = parent
        self._position = position

    def position(self):
        return self._parent + (self._position,)

    def nested(self, input_object):
        return _ObjectStream(self._memo, input_object, self._parent+(self._position,))

    def _advance(self, next_object, objects):
        return _ObjectStream(self._memo, objects, self._parent, self._position+1)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.position()))
