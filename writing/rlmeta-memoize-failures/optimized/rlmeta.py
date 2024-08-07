import sys

SUPPORT = 'try:\n    from cStringIO import StringIO\nexcept:\n    from StringIO import StringIO\n\nclass _Grammar(object):\n\n    def _or(self, matchers):\n        original_stream = self._stream\n        for matcher in matchers[:-1]:\n            try:\n                return matcher()\n            except _MatchError:\n                self._stream = original_stream\n        return matchers[-1]()\n\n    def _and(self, matchers):\n        result = None\n        for matcher in matchers:\n            result = matcher()\n        return result\n\n    def _star(self, matcher):\n        result = []\n        while True:\n            original_stream = self._stream\n            try:\n                result.append(matcher())\n            except _MatchError:\n                self._stream = original_stream\n                return _SemanticAction(lambda: [x.eval() for x in result])\n\n    def _not(self, matcher):\n        original_stream = self._stream\n        try:\n            matcher()\n        except _MatchError:\n            return _SemanticAction(lambda: None)\n        else:\n            original_stream.fail(lambda: "match found")\n        finally:\n            self._stream = original_stream\n\n    def _match_rule(self, rule_name):\n        key = (rule_name, self._stream.position())\n        if key in self._memo:\n            if self._memo[key][0] is None:\n                self._stream.fail(self._memo[key][1])\n            else:\n                result, _, self._stream = self._memo[key]\n        else:\n            try:\n                start = self._stream\n                result = getattr(self, "_rule_{}".format(rule_name))()\n                end = self._stream\n                self._memo[key] = (result, start, end)\n            except _MatchError as e:\n                self._memo[key] = (None, e.lazy_message, None)\n                raise\n        return result\n\n    def _match_range(self, start, end):\n        next_objext = self._stream.peek()\n        if next_objext >= start and next_objext <= end:\n            self._stream = self._stream.advance()\n            return _SemanticAction(lambda: next_objext)\n        else:\n            self._stream.fail(\n                lambda: "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)\n            )\n\n    def _match_string(self, string):\n        next_object = self._stream.peek()\n        if next_object == string:\n            self._stream = self._stream.advance()\n            return _SemanticAction(lambda: string)\n        else:\n            self._stream.fail(\n                lambda: "expected {!r} but found {!r}".format(string, next_object)\n            )\n\n    def _match_charseq(self, charseq):\n        for char in charseq:\n            next_object = self._stream.peek()\n            if next_object != char:\n                self._stream.fail(\n                    lambda: "expected {!r} but found {!r}".format(char, next_object)\n                )\n            self._stream = self._stream.advance()\n        return _SemanticAction(lambda: charseq)\n\n    def _match_any(self):\n        next_object = self._stream.peek()\n        self._stream = self._stream.advance()\n        return _SemanticAction(lambda: next_object)\n\n    def _match_call_rule(self):\n        next_object = self._stream.peek()\n        self._stream = self._stream.advance()\n        return self._match_rule(str(next_object))\n\n    def _match_list(self, matcher):\n        original_stream = self._stream\n        next_object = self._stream.peek()\n        if isinstance(next_object, list):\n            self._stream = self._stream.nested(next_object)\n            matcher()\n            if self._stream.is_at_end():\n                self._stream = original_stream.advance()\n                return _SemanticAction(lambda: next_object)\n        original_stream.fail(lambda: "list match failed")\n\n    def run(self, rule_name, input_object):\n        self._memo = _Memo()\n        self._stream = _Stream.from_object(self._memo, input_object)\n        result = self._match_rule(rule_name).eval()\n        if isinstance(result, _Builder):\n            return result.build_string()\n        else:\n            return result\n\nclass _Vars(dict):\n\n    def bind(self, name, value):\n        self[name] = value\n        return value\n\n    def lookup(self, name):\n        return self[name]\n\nclass _SemanticAction(object):\n\n    def __init__(self, fn):\n        self.fn = fn\n\n    def eval(self):\n        return self.fn()\n\nclass _Builder(object):\n\n    def build_string(self):\n        output = _Output()\n        self.write(output)\n        return output.value\n\n    @classmethod\n    def create(self, item):\n        if isinstance(item, _Builder):\n            return item\n        elif isinstance(item, list):\n            return _ListBuilder([_Builder.create(x) for x in item])\n        else:\n            return _AtomBuilder(item)\n\nclass _Output(object):\n\n    def __init__(self):\n        self.buffer = StringIO()\n        self.indentation = 0\n        self.on_newline = True\n\n    @property\n    def value(self):\n        return self.buffer.getvalue()\n\n    def write(self, value):\n        for ch in value:\n            is_linebreak = ch == "\\n"\n            if self.indentation and self.on_newline and not is_linebreak:\n                self.buffer.write("    "*self.indentation)\n            self.buffer.write(ch)\n            self.on_newline = is_linebreak\n\nclass _ListBuilder(_Builder):\n\n    def __init__(self, builders):\n        self.builders = builders\n\n    def write(self, output):\n        for builder in self.builders:\n            builder.write(output)\n\nclass _AtomBuilder(_Builder):\n\n    def __init__(self, atom):\n        self.atom = atom\n\n    def write(self, output):\n        output.write(str(self.atom))\n\nclass _IndentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation += 1\n\nclass _DedentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation -= 1\n\nclass _Memo(dict):\n\n    def __init__(self):\n        dict.__init__(self)\n        self._latest_stream = _ObjectStream(self, [], -1)\n        self._latest_lazy_message = lambda: ""\n\n    def describe(self):\n        items = []\n        for (rule_name, _), (_, start, end) in self.items():\n            if end > start:\n                items.append((rule_name, start, end))\n        items.sort(key=lambda item: (item[2].position(), item[1].position()))\n        message = []\n        for item in items:\n            message.append("matched {: <20} {} -> {}\\n".format(*item))\n        message.append("\\n")\n        message.append("ERROR: {}: {}\\n".format(\n            self._latest_stream,\n            self._latest_lazy_message()\n        ))\n        return "".join(message)\n\n    def fail(self, stream, lazy_message):\n        if stream.position() >= self._latest_stream.position():\n            self._latest_stream = stream\n            self._latest_lazy_message = lazy_message\n        raise _MatchError(self, lazy_message)\n\nclass _MatchError(Exception):\n\n    def __init__(self, memo, lazy_message):\n        Exception.__init__(self)\n        self._memo = memo\n        self.lazy_message = lazy_message\n\n    def describe(self):\n        return self._memo.describe()\n\nclass _Stream(object):\n\n    @classmethod\n    def from_object(cls, memo, input_object):\n        if isinstance(input_object, basestring):\n            return _CharStream(memo, input_object, 0)\n        else:\n            return _ObjectStream(memo, [input_object], 0)\n\n    def __init__(self, memo, objects, index):\n        self._memo = memo\n        self._objects = objects\n        self._index = index\n\n    def fail(self, lazy_message):\n        self._memo.fail(self, lazy_message)\n\n    def peek(self):\n        if self.is_at_end():\n            self.fail(lambda: "not eof")\n        return self._objects[self._index]\n\n    def is_at_end(self):\n        return self._index >= len(self._objects)\n\nclass _CharStream(_Stream):\n\n    def __init__(self, memo, objects, index, line=1, column=1):\n        _Stream.__init__(self, memo, objects, index)\n        self._line = line\n        self._column = column\n\n    def position(self):\n        return self._index\n\n    def advance(self):\n        if self._objects[self._index] == "\\n":\n            line = self._line + 1\n            column = 1\n        else:\n            line = self._line\n            column = self._column + 1\n        return _CharStream(self._memo, self._objects, self._index+1, line, column)\n\n    def __str__(self):\n        return "L{:03d}:C{:03d}".format(self._line, self._column)\n\nclass _ObjectStream(_Stream):\n\n    def __init__(self, memo, objects, index, parent=()):\n        _Stream.__init__(self, memo, objects, index)\n        self._parent_position = parent\n        self._position = self._parent_position + (self._index,)\n\n    def position(self):\n        return self._position\n\n    def nested(self, input_object):\n        return _ObjectStream(self._memo, input_object, 0, self._position)\n\n    def advance(self):\n        return _ObjectStream(self._memo, self._objects, self._index+1, self._parent_position)\n\n    def __str__(self):\n        return "[{}]".format(", ".join(str(x) for x in self.position()))\n'

try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

class _Grammar(object):

    def _or(self, matchers):
        original_stream = self._stream
        for matcher in matchers[:-1]:
            try:
                return matcher()
            except _MatchError:
                self._stream = original_stream
        return matchers[-1]()

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
            original_stream.fail(lambda: "match found")
        finally:
            self._stream = original_stream

    def _match_rule(self, rule_name):
        key = (rule_name, self._stream.position())
        if key in self._memo:
            if self._memo[key][0] is None:
                self._stream.fail(self._memo[key][1])
            else:
                result, _, self._stream = self._memo[key]
        else:
            try:
                start = self._stream
                result = getattr(self, "_rule_{}".format(rule_name))()
                end = self._stream
                self._memo[key] = (result, start, end)
            except _MatchError as e:
                self._memo[key] = (None, e.lazy_message, None)
                raise
        return result

    def _match_range(self, start, end):
        next_objext = self._stream.peek()
        if next_objext >= start and next_objext <= end:
            self._stream = self._stream.advance()
            return _SemanticAction(lambda: next_objext)
        else:
            self._stream.fail(
                lambda: "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)
            )

    def _match_string(self, string):
        next_object = self._stream.peek()
        if next_object == string:
            self._stream = self._stream.advance()
            return _SemanticAction(lambda: string)
        else:
            self._stream.fail(
                lambda: "expected {!r} but found {!r}".format(string, next_object)
            )

    def _match_charseq(self, charseq):
        for char in charseq:
            next_object = self._stream.peek()
            if next_object != char:
                self._stream.fail(
                    lambda: "expected {!r} but found {!r}".format(char, next_object)
                )
            self._stream = self._stream.advance()
        return _SemanticAction(lambda: charseq)

    def _match_any(self):
        next_object = self._stream.peek()
        self._stream = self._stream.advance()
        return _SemanticAction(lambda: next_object)

    def _match_call_rule(self):
        next_object = self._stream.peek()
        self._stream = self._stream.advance()
        return self._match_rule(str(next_object))

    def _match_list(self, matcher):
        original_stream = self._stream
        next_object = self._stream.peek()
        if isinstance(next_object, list):
            self._stream = self._stream.nested(next_object)
            matcher()
            if self._stream.is_at_end():
                self._stream = original_stream.advance()
                return _SemanticAction(lambda: next_object)
        original_stream.fail(lambda: "list match failed")

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
        self._latest_stream = _ObjectStream(self, [], -1)
        self._latest_lazy_message = lambda: ""

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
            self._latest_lazy_message()
        ))
        return "".join(message)

    def fail(self, stream, lazy_message):
        if stream.position() >= self._latest_stream.position():
            self._latest_stream = stream
            self._latest_lazy_message = lazy_message
        raise _MatchError(self, lazy_message)

class _MatchError(Exception):

    def __init__(self, memo, lazy_message):
        Exception.__init__(self)
        self._memo = memo
        self.lazy_message = lazy_message

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

    def fail(self, lazy_message):
        self._memo.fail(self, lazy_message)

    def peek(self):
        if self.is_at_end():
            self.fail(lambda: "not eof")
        return self._objects[self._index]

    def is_at_end(self):
        return self._index >= len(self._objects)

class _CharStream(_Stream):

    def __init__(self, memo, objects, index, line=1, column=1):
        _Stream.__init__(self, memo, objects, index)
        self._line = line
        self._column = column

    def position(self):
        return self._index

    def advance(self):
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

    def __init__(self, memo, objects, index, parent=()):
        _Stream.__init__(self, memo, objects, index)
        self._parent_position = parent
        self._position = self._parent_position + (self._index,)

    def position(self):
        return self._position

    def nested(self, input_object):
        return _ObjectStream(self._memo, input_object, 0, self._position)

    def advance(self):
        return _ObjectStream(self._memo, self._objects, self._index+1, self._parent_position)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.position()))

class Parser(_Grammar):

    def _rule_grammar(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('name'))())),
                (lambda: self._match_rule('space')),
                (lambda: self._match_charseq('{')),
                (lambda: _vars.bind('ys', (lambda: self._star((lambda: self._match_rule('rule'))))())),
                (lambda: self._match_rule('space')),
                (lambda: self._match_charseq('}')),
                (lambda: _SemanticAction(lambda: (['Grammar']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]))),
            ]))()
        )(_Vars()))()

    def _rule_rule(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('name'))())),
                (lambda: self._match_rule('space')),
                (lambda: self._match_charseq('=')),
                (lambda: _vars.bind('y', (lambda: self._match_rule('choice'))())),
                (lambda: _SemanticAction(lambda: (['Rule']+[_vars.lookup('x').eval()]+[_vars.lookup('y').eval()]+[]))),
            ]))()
        )(_Vars()))()

    def _rule_choice(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: self._or([
                    (lambda: (lambda _vars:
                        (lambda: self._and([
                            (lambda: self._match_rule('space')),
                            (lambda: self._match_charseq('|')),
                        ]))()
                    )(_Vars())),
                    (lambda: self._and([
                    ])),
                ])),
                (lambda: _vars.bind('x', (lambda: self._match_rule('sequence'))())),
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: (lambda _vars:
                    (lambda: self._and([
                        (lambda: self._match_rule('space')),
                        (lambda: self._match_charseq('|')),
                        (lambda: self._match_rule('sequence')),
                    ]))()
                )(_Vars()))))())),
                (lambda: _SemanticAction(lambda: (['Or']+[_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[]))),
            ]))()
        )(_Vars()))()

    def _rule_sequence(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('expr'))())),
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('expr'))))())),
                (lambda: _SemanticAction(lambda: (['Scope']+[(['And']+[_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[])]+[]))),
            ]))()
        )(_Vars()))()

    def _rule_expr(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('expr1'))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq(':')),
                    (lambda: _vars.bind('y', (lambda: self._match_rule('name'))())),
                    (lambda: _SemanticAction(lambda: (['Bind']+[_vars.lookup('y').eval()]+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_rule('expr1'))()
            )(_Vars())),
        ]))()

    def _rule_expr1(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('expr2'))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('*')),
                    (lambda: _SemanticAction(lambda: (['Star']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('expr2'))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('?')),
                    (lambda: _SemanticAction(lambda: (['Or']+[_vars.lookup('x').eval()]+[(['And']+[])]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('!')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('expr2'))())),
                    (lambda: _SemanticAction(lambda: (['Not']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('%')),
                    (lambda: _SemanticAction(lambda: (['MatchCallRule']+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_rule('expr2'))()
            )(_Vars())),
        ]))()

    def _rule_expr2(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('->')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('hostExpr'))())),
                    (lambda: _SemanticAction(lambda: (['SemanticAction']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('name'))())),
                    (lambda: self._not((lambda: (lambda _vars:
                        (lambda: self._and([
                            (lambda: self._match_rule('space')),
                            (lambda: self._match_charseq('=')),
                        ]))()
                    )(_Vars())))),
                    (lambda: _SemanticAction(lambda: (['MatchRule']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('char'))())),
                    (lambda: self._match_charseq('-')),
                    (lambda: _vars.bind('y', (lambda: self._match_rule('char'))())),
                    (lambda: _SemanticAction(lambda: (['MatchRange']+[_vars.lookup('x').eval()]+[_vars.lookup('y').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('string'))())),
                    (lambda: _SemanticAction(lambda: (['MatchString']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('charseq'))())),
                    (lambda: _SemanticAction(lambda: (['MatchCharseq']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('.')),
                    (lambda: _SemanticAction(lambda: (['MatchAny']+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('(')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('choice'))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq(')')),
                    (lambda: _SemanticAction(lambda: _vars.lookup('x').eval())),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('[')),
                    (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('expr'))))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq(']')),
                    (lambda: _SemanticAction(lambda: (['MatchList']+[(['And']+_vars.lookup('xs').eval()+[])]+[]))),
                ]))()
            )(_Vars())),
        ]))()

    def _rule_hostExpr(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('string'))())),
                    (lambda: _SemanticAction(lambda: (['String']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('[')),
                    (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('hostExprListItem'))))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq(']')),
                    (lambda: _SemanticAction(lambda: (['List']+_vars.lookup('xs').eval()+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('{')),
                    (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('buildExpr'))))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('}')),
                    (lambda: _SemanticAction(lambda: (['Builder']+_vars.lookup('xs').eval()+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('name'))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('(')),
                    (lambda: _vars.bind('ys', (lambda: self._star((lambda: self._match_rule('hostExpr'))))())),
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq(')')),
                    (lambda: _SemanticAction(lambda: (['FnCall']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('name'))())),
                    (lambda: _SemanticAction(lambda: (['VarLookup']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
        ]))()

    def _rule_hostExprListItem(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('~')),
                    (lambda: _vars.bind('x', (lambda: self._match_rule('hostExpr'))())),
                    (lambda: _SemanticAction(lambda: (['ListItemSplice']+[_vars.lookup('x').eval()]+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_rule('hostExpr'))()
            )(_Vars())),
        ]))()

    def _rule_buildExpr(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('>')),
                    (lambda: _SemanticAction(lambda: (['IndentBuilder']+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_rule('space')),
                    (lambda: self._match_charseq('<')),
                    (lambda: _SemanticAction(lambda: (['DedentBuilder']+[]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_rule('hostExpr'))()
            )(_Vars())),
        ]))()

    def _rule_string(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: self._match_charseq('"')),
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: (lambda _vars:
                    (lambda: self._and([
                        (lambda: self._not((lambda: self._match_charseq('"')))),
                        (lambda: self._match_rule('innerChar')),
                    ]))()
                )(_Vars()))))())),
                (lambda: self._match_charseq('"')),
                (lambda: _SemanticAction(lambda: join(
                    _vars.lookup('xs').eval(),
                ))),
            ]))()
        )(_Vars()))()

    def _rule_charseq(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: self._match_charseq("'")),
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: (lambda _vars:
                    (lambda: self._and([
                        (lambda: self._not((lambda: self._match_charseq("'")))),
                        (lambda: self._match_rule('innerChar')),
                    ]))()
                )(_Vars()))))())),
                (lambda: self._match_charseq("'")),
                (lambda: _SemanticAction(lambda: join(
                    _vars.lookup('xs').eval(),
                ))),
            ]))()
        )(_Vars()))()

    def _rule_char(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: self._match_charseq("'")),
                (lambda: self._not((lambda: self._match_charseq("'")))),
                (lambda: _vars.bind('x', (lambda: self._match_rule('innerChar'))())),
                (lambda: self._match_charseq("'")),
                (lambda: _SemanticAction(lambda: _vars.lookup('x').eval())),
            ]))()
        )(_Vars()))()

    def _rule_innerChar(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_charseq('\\')),
                    (lambda: self._match_rule('escape')),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                self._match_any()
            )(_Vars())),
        ]))()

    def _rule_escape(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_charseq('\\')),
                    (lambda: _SemanticAction(lambda: '\\')),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_charseq("'")),
                    (lambda: _SemanticAction(lambda: "'")),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_charseq('"')),
                    (lambda: _SemanticAction(lambda: '"')),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_charseq('n')),
                    (lambda: _SemanticAction(lambda: '\n')),
                ]))()
            )(_Vars())),
        ]))()

    def _rule_name(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: self._match_rule('space')),
                (lambda: _vars.bind('x', (lambda: self._match_rule('nameStart'))())),
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('nameChar'))))())),
                (lambda: _SemanticAction(lambda: join(
                    ([_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[]),
                ))),
            ]))()
        )(_Vars()))()

    def _rule_nameStart(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._match_range('a', 'z'))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_range('A', 'Z'))()
            )(_Vars())),
        ]))()

    def _rule_nameChar(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._match_range('a', 'z'))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_range('A', 'Z'))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._match_range('0', '9'))()
            )(_Vars())),
        ]))()

    def _rule_space(self):
        return (lambda: (lambda _vars:
            (lambda: self._star((lambda: self._or([
                (lambda: (lambda _vars:
                    (lambda: self._match_charseq(' '))()
                )(_Vars())),
                (lambda: (lambda _vars:
                    (lambda: self._match_charseq('\n'))()
                )(_Vars())),
            ]))))()
        )(_Vars()))()

class CodeGenerator(_Grammar):

    def _rule_Grammar(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _vars.bind('ys', (lambda: self._star((lambda: self._match_rule('ast'))))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    'class ',
                    _vars.lookup('x').eval(),
                    '(_Grammar):\n',
                    _IndentBuilder(),
                    _vars.lookup('ys').eval(),
                    _DedentBuilder(),
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_Rule(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _vars.bind('y', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '\ndef _rule_',
                    _vars.lookup('x').eval(),
                    '(self):\n',
                    _IndentBuilder(),
                    'return ',
                    _vars.lookup('y').eval(),
                    '()\n',
                    _DedentBuilder(),
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_MatchAny(self):
        return (lambda: (lambda _vars:
            (lambda: _SemanticAction(lambda: _Builder.create([
                'self._match_any',
            ])))()
        )(_Vars()))()

    def _rule_MatchCallRule(self):
        return (lambda: (lambda _vars:
            (lambda: _SemanticAction(lambda: _Builder.create([
                'self._match_call_rule',
            ])))()
        )(_Vars()))()

    def _rule_String(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_List(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('astList'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    _vars.lookup('x').eval(),
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_Builder(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('astItems'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '_Builder.create([',
                    _vars.lookup('x').eval(),
                    '])',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_IndentBuilder(self):
        return (lambda: (lambda _vars:
            (lambda: _SemanticAction(lambda: _Builder.create([
                '_IndentBuilder()',
            ])))()
        )(_Vars()))()

    def _rule_DedentBuilder(self):
        return (lambda: (lambda _vars:
            (lambda: _SemanticAction(lambda: _Builder.create([
                '_DedentBuilder()',
            ])))()
        )(_Vars()))()

    def _rule_FnCall(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _vars.bind('y', (lambda: self._match_rule('astItems'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    _vars.lookup('x').eval(),
                    '(',
                    _vars.lookup('y').eval(),
                    ')',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_VarLookup(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '_vars.lookup(',
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                    ').eval()',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_Or(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                    (lambda: self._not(self._match_any)),
                    (lambda: _SemanticAction(lambda: _vars.lookup('x').eval())),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('astItems'))())),
                    (lambda: _SemanticAction(lambda: _Builder.create([
                        '(lambda: self._or([',
                        _vars.lookup('x').eval(),
                        ']))',
                    ]))),
                ]))()
            )(_Vars())),
        ]))()

    def _rule_Scope(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: (lambda _vars:\n',
                    _IndentBuilder(),
                    _vars.lookup('x').eval(),
                    _DedentBuilder(),
                    '()\n)(_Vars()))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_And(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                    (lambda: self._not(self._match_any)),
                    (lambda: _SemanticAction(lambda: _vars.lookup('x').eval())),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('astItems'))())),
                    (lambda: _SemanticAction(lambda: _Builder.create([
                        '(lambda: self._and([',
                        _vars.lookup('x').eval(),
                        ']))',
                    ]))),
                ]))()
            )(_Vars())),
        ]))()

    def _rule_Bind(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _vars.bind('y', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: _vars.bind(',
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                    ', ',
                    _vars.lookup('y').eval(),
                    '()))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_Star(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._star(',
                    _vars.lookup('x').eval(),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_Not(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._not(',
                    _vars.lookup('x').eval(),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_SemanticAction(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: _SemanticAction(lambda: ',
                    _vars.lookup('x').eval(),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_MatchRule(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._match_rule(',
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_MatchRange(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _vars.bind('y', self._match_any())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._match_range(',
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                    ', ',
                    repr(
                        _vars.lookup('y').eval(),
                    ),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_MatchString(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._match_string(',
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_MatchCharseq(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._match_charseq(',
                    repr(
                        _vars.lookup('x').eval(),
                    ),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_MatchList(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(lambda: self._match_list(',
                    _vars.lookup('x').eval(),
                    '))',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_ast(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: self._match_list((lambda: _vars.bind('x', self._match_call_rule())))),
                (lambda: _SemanticAction(lambda: _vars.lookup('x').eval())),
            ]))()
        )(_Vars()))()

    def _rule_astItems(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('astItem'))))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '\n',
                    _IndentBuilder(),
                    _vars.lookup('xs').eval(),
                    _DedentBuilder(),
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_astItem(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    _vars.lookup('x').eval(),
                    ',\n',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_astList(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('astListItem'))))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    '(',
                    _vars.lookup('xs').eval(),
                    '[])',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_astListItem(self):
        return (lambda: self._or([
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: self._match_list((lambda: self._and([
                        (lambda: self._match_string('ListItemSplice')),
                        (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                    ])))),
                    (lambda: _SemanticAction(lambda: _Builder.create([
                        _vars.lookup('x').eval(),
                        '+',
                    ]))),
                ]))()
            )(_Vars())),
            (lambda: (lambda _vars:
                (lambda: self._and([
                    (lambda: _vars.bind('x', (lambda: self._match_rule('ast'))())),
                    (lambda: _SemanticAction(lambda: _Builder.create([
                        '[',
                        _vars.lookup('x').eval(),
                        ']+',
                    ]))),
                ]))()
            )(_Vars())),
        ]))()

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(compile_grammar(sys.stdin.read()))
        except _MatchError as e:
            sys.stderr.write(e.describe())
            sys.exit(1)
