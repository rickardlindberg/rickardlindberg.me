import sys

SUPPORT = 'class _RLMeta(object):\n\n    def __init__(self, logger=None):\n        self._log = (lambda message: None) if logger is None else logger\n\n    def run(self, rule_name, input_object):\n        self._input = _Input.from_object(input_object)\n        self._memo = {}\n        try:\n            result = self._match(rule_name).eval()\n            if hasattr(result, "to_rlmeta_output_stream"):\n                return result.to_rlmeta_output_stream()\n            else:\n                return result\n        except _MatchError:\n            self._dump_memo()\n            raise\n\n    def _dump_memo(self):\n        items = []\n        for (rule_name, _), (_, start, end) in self._memo.items():\n            items.append((rule_name, start, end))\n        items.sort(key=lambda item: (item[2].as_key(), item[1].as_key()))\n        for item in items:\n            self._log("matched {: <20} {} -> {}\\n".format(*item))\n\n    def _match(self, rule_name):\n        key = (rule_name, self._input.as_key())\n        if key in self._memo:\n            result, _, self._input = self._memo[key]\n        else:\n            start_input = self._input\n            result = getattr(self, "_rule_{}".format(rule_name))()\n            self._memo[key] = (result, start_input, self._input)\n        return result\n\n    def _or(self, matchers):\n        saved_input = self._input\n        for matcher in matchers:\n            try:\n                return matcher()\n            except _MatchError:\n                self._input = saved_input\n        raise _MatchError()\n\n    def _and(self, matchers):\n        result = None\n        for matcher in matchers:\n            result = matcher()\n        return result\n\n    def _star(self, matcher):\n        result = []\n        while True:\n            saved_input = self._input\n            try:\n                result.append(matcher())\n            except _MatchError:\n                self._input = saved_input\n                return _SemanticAction(lambda: [x.eval() for x in result])\n\n    def _negative_lookahead(self, matcher):\n        saved_input = self._input\n        try:\n            matcher()\n        except _MatchError:\n            return _SemanticAction(lambda: None)\n        else:\n            raise _MatchError()\n        finally:\n            self._input = saved_input\n\n    def _match_range(self, a, b):\n        next_objext, self._input = self._input.next()\n        if next_objext >= a and next_objext <= b:\n            return _SemanticAction(lambda: next_objext)\n        else:\n            raise _MatchError()\n\n    def _match_string(self, string):\n        next_object, self._input = self._input.next()\n        if next_object == string:\n            return _SemanticAction(lambda: string)\n        else:\n            raise _MatchError()\n\n    def _match_charseq(self, charseq):\n        for char in charseq:\n            next_object, self._input = self._input.next()\n            if next_object != char:\n                raise _MatchError()\n        return _SemanticAction(lambda: charseq)\n\n    def _any(self):\n        next_object, self._input = self._input.next()\n        return _SemanticAction(lambda: next_object)\n\n    def _match_list(self, matcher):\n        next_object, next_input = self._input.next()\n        if isinstance(next_object, list):\n            self._input = self._input.nested(next_object)\n            matcher()\n            if self._input.is_at_end():\n                self._input = next_input\n                return _SemanticAction(lambda: next_object)\n        raise _MatchError()\n\nclass _Input(object):\n\n    @classmethod\n    def from_object(cls, input_object):\n        if isinstance(input_object, basestring):\n            return _StringInput(list(input_object))\n        else:\n            return _ListInput([input_object])\n\n    def __init__(self, objects):\n        self._objects = objects\n\n    def next(self):\n        if self.is_at_end():\n            raise _MatchError()\n        next_object = self._objects[0]\n        return (\n            next_object,\n            self._advance(next_object, self._objects[1:]),\n        )\n\n    def is_at_end(self):\n        return len(self._objects) == 0\n\nclass _StringInput(_Input):\n\n    def __init__(self, objects, line=1, column=1):\n        _Input.__init__(self, objects)\n        self._line = line\n        self._column = column\n\n    def as_key(self):\n        return (self._line, self._column)\n\n    def _advance(self, next_object, objects):\n        if next_object == "\\n":\n            return _StringInput(objects, self._line+1, 1)\n        else:\n            return _StringInput(objects, self._line, self._column+1)\n\n    def __str__(self):\n        return "L{:03d}:C{:03d}".format(self._line, self._column)\n\nclass _ListInput(_Input):\n\n    def __init__(self, objects, parent=(), pos=0):\n        _Input.__init__(self, objects)\n        self._parent = parent\n        self._pos = pos\n\n    def as_key(self):\n        return self._parent + (self._pos,)\n\n    def nested(self, input_object):\n        return _ListInput(input_object, self._parent+(self._pos,))\n\n    def _advance(self, next_object, objects):\n        return _ListInput(objects, self._parent, self._pos+1)\n\n    def __str__(self):\n        return "[{}]".format(", ".join(str(x) for x in self.as_key()))\n\nclass _SemanticAction(object):\n\n    def __init__(self, fn):\n        self.fn = fn\n\n    def eval(self):\n        return self.fn()\n\nclass _MatchError(Exception):\n    pass\n\nclass _Vars(dict):\n\n    def bind(self, name, value):\n        self[name] = value\n        return value\n\n    def lookup(self, name):\n        return self[name]\n\nclass _Builder(object):\n\n    @classmethod\n    def create(self, item):\n        if isinstance(item, _Builder):\n            return item\n        elif isinstance(item, list):\n            return _ListBuilder([_Builder.create(x) for x in item])\n        else:\n            return _AtomBuilder(item)\n\n    def to_rlmeta_output_stream(self):\n        output = _Output()\n        self.write(output)\n        return output.value\n\nclass _ListBuilder(_Builder):\n\n    def __init__(self, items):\n        self.items = items\n\n    def write(self, output):\n        for item in self.items:\n            item.write(output)\n\nclass _AtomBuilder(_Builder):\n\n    def __init__(self, atom):\n        self.atom = atom\n\n    def write(self, output):\n        output.write(str(self.atom))\n\nclass _IndentBuilder(_Builder):\n\n    def write(self, output):\n        output.indent()\n\nclass _DedentBuilder(_Builder):\n\n    def write(self, output):\n        output.dedent()\n\nclass _Output(object):\n\n    def __init__(self):\n        self.value = ""\n        self.level = 0\n\n    def indent(self):\n        self.level += 1\n\n    def dedent(self):\n        self.level -= 1\n\n    def write(self, value):\n        for ch in value:\n            if self.value and ch != "\\n" and self.value[-1] == "\\n":\n                self.value += "    "*self.level\n            self.value += ch\n'

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

class Parser(_RLMeta):

    def _rule_grammar(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('name')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('{')
                                ),
                                (lambda:
                                    _vars.bind('ys', (lambda:
                                        self._star((lambda:
                                            self._match('rule')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('}')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Grammar']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_rule(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('name')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('=')
                                ),
                                (lambda:
                                    _vars.bind('y', (lambda:
                                        self._match('choices')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Rule']+[_vars.lookup('x').eval()]+[_vars.lookup('y').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_choices(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._or([
                                        (lambda:
                                            self._or([
                                                (lambda:
                                                    (lambda _vars:
                                                        (lambda:
                                                            self._and([
                                                                (lambda:
                                                                    self._match('space')
                                                                ),
                                                                (lambda:
                                                                    self._match_charseq('|')
                                                                ),
                                                            ])
                                                        )()
                                                    )(_Vars())
                                                ),
                                            ])
                                        ),
                                        (lambda:
                                            None
                                        ),
                                    ])
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('sequence')
                                    )())
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._or([
                                                (lambda:
                                                    (lambda _vars:
                                                        (lambda:
                                                            self._and([
                                                                (lambda:
                                                                    self._match('space')
                                                                ),
                                                                (lambda:
                                                                    self._match_charseq('|')
                                                                ),
                                                                (lambda:
                                                                    self._match('sequence')
                                                                ),
                                                            ])
                                                        )()
                                                    )(_Vars())
                                                ),
                                            ])
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Or']+[_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_sequence(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('expr')
                                    )())
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('expr')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Scope']+[(['And']+[_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[])]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_expr(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('expr1')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq(':')
                                ),
                                (lambda:
                                    _vars.bind('y', (lambda:
                                        self._match('name')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Bind']+[_vars.lookup('y').eval()]+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('expr1')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_expr1(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('expr2')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('*')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Star']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('expr2')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('?')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Or']+[_vars.lookup('x').eval()]+[(['MatchNothing']+[])]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('!')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('expr2')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Not']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('expr2')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_expr2(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('->')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('hostExpr')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['SemanticAction']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('name')
                                    )())
                                ),
                                (lambda:
                                    self._negative_lookahead((lambda:
                                        self._or([
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._match('space')
                                                            ),
                                                            (lambda:
                                                                self._match_charseq('=')
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['Apply']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('char')
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq('-')
                                ),
                                (lambda:
                                    _vars.bind('y', (lambda:
                                        self._match('char')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['MatchCharRange']+[_vars.lookup('x').eval()]+[_vars.lookup('y').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('string')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['MatchString']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('charseq')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['MatchCharseq']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('.')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['MatchAny']+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('(')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('choices')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq(')')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _vars.lookup('x').eval())
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('[')
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('expr')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq(']')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['MatchList']+[(['And']+_vars.lookup('xs').eval()+[])]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_hostExpr(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('string')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['String']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('charseq')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['String']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('[')
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('hostExprListItem')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq(']')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['List']+_vars.lookup('xs').eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('{')
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('buildExpr')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('}')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['ListBuilder']+_vars.lookup('xs').eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('name')
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('(')
                                ),
                                (lambda:
                                    _vars.bind('ys', (lambda:
                                        self._star((lambda:
                                            self._match('hostExpr')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq(')')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['FnCall']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('name')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['VarLookup']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_hostExprListItem(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('~')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('hostExpr')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['ListItemSplice']+[_vars.lookup('x').eval()]+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('hostExpr')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_buildExpr(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('>')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['IndentBuilder']+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    self._match_charseq('<')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: (['DedentBuilder']+[]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('hostExpr')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_string(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq('"')
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._or([
                                                (lambda:
                                                    (lambda _vars:
                                                        (lambda:
                                                            self._and([
                                                                (lambda:
                                                                    self._negative_lookahead((lambda:
                                                                        self._match_charseq('"')
                                                                    ))
                                                                ),
                                                                (lambda:
                                                                    self._match('innerChar')
                                                                ),
                                                            ])
                                                        )()
                                                    )(_Vars())
                                                ),
                                            ])
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq('"')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: join(
                                        _vars.lookup('xs').eval(),
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_charseq(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._or([
                                                (lambda:
                                                    (lambda _vars:
                                                        (lambda:
                                                            self._and([
                                                                (lambda:
                                                                    self._negative_lookahead((lambda:
                                                                        self._match_charseq("'")
                                                                    ))
                                                                ),
                                                                (lambda:
                                                                    self._match('innerChar')
                                                                ),
                                                            ])
                                                        )()
                                                    )(_Vars())
                                                ),
                                            ])
                                        ))
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: join(
                                        _vars.lookup('xs').eval(),
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_char(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    self._negative_lookahead((lambda:
                                        self._match_charseq("'")
                                    ))
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('innerChar')
                                    )())
                                ),
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: join(
                                        _vars.lookup('x').eval(),
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_innerChar(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq('\\')
                                ),
                                (lambda:
                                    self._match('escape')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                self._any,
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_escape(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq('\\')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: '\\')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq("'")
                                ),
                                (lambda:
                                    _SemanticAction(lambda: "'")
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq('"')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: '"')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq('n')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: '\n')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_charseq('t')
                                ),
                                (lambda:
                                    _SemanticAction(lambda: '\t')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_name(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match('space')
                                ),
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('nameStart')
                                    )())
                                ),
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('nameChar')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: join(
                                        ([_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[]),
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_nameStart(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_range('a', 'z')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_range('A', 'Z')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_nameChar(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_range('a', 'z')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_range('A', 'Z')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_range('0', '9')
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_space(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._star((lambda:
                                        self._or([
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._match_charseq(' ')
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._match_charseq('\t')
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                            (lambda:
                                                (lambda _vars:
                                                    (lambda:
                                                        self._and([
                                                            (lambda:
                                                                self._match_charseq('\n')
                                                            ),
                                                        ])
                                                    )()
                                                )(_Vars())
                                            ),
                                        ])
                                    ))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

class CodeGenerator(_RLMeta):

    def _rule_ast(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Grammar')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                            (lambda:
                                                _vars.bind('xs', (lambda:
                                                    self._star((lambda:
                                                        self._match('ast')
                                                    ))
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'class ',
                                        _vars.lookup('x').eval(),
                                        '(_RLMeta):\n',
                                        _IndentBuilder(),
                                        _vars.lookup('xs').eval(),
                                        _DedentBuilder(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Rule')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                            (lambda:
                                                _vars.bind('y', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '\ndef _rule_',
                                        _vars.lookup('x').eval(),
                                        '(self):\n',
                                        _IndentBuilder(),
                                        'return ',
                                        _vars.lookup('y').eval(),
                                        '()\n',
                                        _DedentBuilder(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('MatchAny')
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._any',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('String')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('List')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('astList')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup('x').eval(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('ListBuilder')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('astItems')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '_Builder.create([',
                                        _vars.lookup('x').eval(),
                                        '])',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('IndentBuilder')
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '_IndentBuilder()',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('DedentBuilder')
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '_DedentBuilder()',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('FnCall')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                            (lambda:
                                                _vars.bind('y', (lambda:
                                                    self._match('astItems')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup('x').eval(),
                                        '(',
                                        _vars.lookup('y').eval(),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('VarLookup')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '_vars.lookup(',
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                        ').eval()',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('astFnBody')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '(lambda:\n',
                                        _IndentBuilder(),
                                        _vars.lookup('x').eval(),
                                        _DedentBuilder(),
                                        '\n)',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astFnBody(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Or')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('astItems')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._or([',
                                        _vars.lookup('x').eval(),
                                        '])',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Scope')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '(lambda _vars:\n',
                                        _IndentBuilder(),
                                        _vars.lookup('x').eval(),
                                        _DedentBuilder(),
                                        '()\n)(_Vars())',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('And')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('astItems')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._and([',
                                        _vars.lookup('x').eval(),
                                        '])',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Bind')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                            (lambda:
                                                _vars.bind('y', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '_vars.bind(',
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                        ', ',
                                        _vars.lookup('y').eval(),
                                        '())',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Star')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._star(',
                                        _vars.lookup('x').eval(),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('MatchNothing')
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'None',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Not')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._negative_lookahead(',
                                        _vars.lookup('x').eval(),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('SemanticAction')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '_SemanticAction(lambda: ',
                                        _vars.lookup('x').eval(),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('Apply')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._match(',
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('MatchCharRange')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                            (lambda:
                                                _vars.bind('y', self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._match_range(',
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                        ', ',
                                        repr(
                                            _vars.lookup('y').eval(),
                                        ),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('MatchString')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._match_string(',
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('MatchCharseq')
                                            ),
                                            (lambda:
                                                _vars.bind('x', self._any())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._match_charseq(',
                                        repr(
                                            _vars.lookup('x').eval(),
                                        ),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('MatchList')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        'self._match_list(',
                                        _vars.lookup('x').eval(),
                                        ')',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astItems(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('astItem')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '\n',
                                        _IndentBuilder(),
                                        _vars.lookup('xs').eval(),
                                        _DedentBuilder(),
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astItem(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('ast')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup('x').eval(),
                                        ',\n',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astList(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('xs', (lambda:
                                        self._star((lambda:
                                            self._match('astListItem')
                                        ))
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '(',
                                        _vars.lookup('xs').eval(),
                                        '[])',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

    def _rule_astListItem(self):
        return (lambda:
            self._or([
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    self._match_list((lambda:
                                        self._and([
                                            (lambda:
                                                self._match_string('ListItemSplice')
                                            ),
                                            (lambda:
                                                _vars.bind('x', (lambda:
                                                    self._match('ast')
                                                )())
                                            ),
                                        ])
                                    ))
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        _vars.lookup('x').eval(),
                                        '+',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
                (lambda:
                    (lambda _vars:
                        (lambda:
                            self._and([
                                (lambda:
                                    _vars.bind('x', (lambda:
                                        self._match('ast')
                                    )())
                                ),
                                (lambda:
                                    _SemanticAction(lambda: _Builder.create([
                                        '[',
                                        _vars.lookup('x').eval(),
                                        ']+',
                                    ]))
                                ),
                            ])
                        )()
                    )(_Vars())
                ),
            ])
        )()

join = "".join

def compile_grammar(grammar, logger=None):
    parser = Parser(logger)
    code_generator = CodeGenerator(logger)
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        sys.stdout.write(compile_grammar(
            sys.stdin.read(),
            logger=sys.stderr.write
        ))
