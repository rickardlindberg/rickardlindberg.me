class _RLMeta(object):
    def run(self, rule_name, input_object):
        self._input = _Input.from_object(input_object)
        self._memo = {}
        result = self._match(rule_name).eval()
        if hasattr(result, "to_rlmeta_output_stream"):
            return result.to_rlmeta_output_stream()
        else:
            return result
    def _match(self, rule_name):
        key = (rule_name, self._input.pos().key())
        if key not in self._memo:
            start_input = self._input
            result = getattr(self, "_rule_{}".format(rule_name))()
            end_input = self._input
            self._memo[key] = (result, start_input, end_input)
        result, _, self._input = self._memo[key]
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
            if self._input.empty():
                self._input = next_input
                return _SemanticAction(lambda: None)
        raise _MatchError()
class _Input(object):

    @classmethod
    def from_object(cls, input_object):
        if isinstance(input_object, list):
            return cls([input_object], _TreePos())
        else:
            return cls(list(input_object), _StringPos())

    def __init__(self, objects, pos):
        self._objects = objects
        self._pos = pos

    def pos(self):
        return self._pos

    def next(self):
        if self.empty():
            raise _MatchError()
        next_object = self._objects[0]
        return next_object, _Input(
            self._objects[1:],
            pos=self._pos.advance(next_object)
        )

    def empty(self):
        return len(self._objects) == 0

    def nested(self, input_object):
        return _Input(input_object, self._pos.nest())
class _StringPos(object):

    def __init__(self, pos=0, line=1, column=1):
        self._pos = pos
        self._line = line
        self._column = column

    def key(self):
        return self._pos

    def advance(self, next_object):
        if next_object == "\n":
            return _StringPos(self._pos+1, self._line+1, 1)
        else:
            return _StringPos(self._pos+1, self._line, self._column+1)

    def describe(self):
        return "line: {}, column: {}".format(self._line, self._column)
class _TreePos(object):

    def __init__(self, parent=None, pos=0):
        self._parent = [] if parent is None else parent
        self._pos = pos

    def key(self):
        return tuple(self._parent) + (self._pos,)

    def advance(self, next_object):
        return _TreePos(self._parent, self._pos+1)

    def nest(self):
        return _TreePos(self._parent+[self._pos])

    def describe(self):
        return "[{}]".format(", ".join(str(x) for x in self.key()))
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
