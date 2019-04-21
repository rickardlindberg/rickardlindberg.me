import sys

SUPPORT = 'class _Grammar(object):\n\n    def _or(self, matchers):\n        original_stream = self._stream\n        var_len = len(self._vars)\n        for matcher in matchers:\n            try:\n                return matcher()\n            except _MatchError:\n                self._stream = original_stream\n                self._vars = self._vars[:var_len]\n        original_stream.fail("no choice matched")\n\n    def _and(self, matchers):\n        result = None\n        for matcher in matchers:\n            result = matcher()\n        return result\n\n    def _star(self, matcher):\n        result = []\n        while True:\n            original_stream = self._stream\n            var_len = len(self._vars)\n            try:\n                result.append(matcher())\n            except _MatchError:\n                self._stream = original_stream\n                self._vars = self._vars[:var_len]\n                return _SemanticAction(lambda _vars: [x.eval() for x in result], self._vars[-1])\n\n    def _not(self, matcher):\n        original_stream = self._stream\n        var_len = len(self._vars)\n        try:\n            matcher()\n        except _MatchError:\n            return _SemanticAction(lambda _vars: None, self._vars[-1])\n        else:\n            original_stream.fail("match found")\n        finally:\n            self._stream = original_stream\n            self._vars = self._vars[:var_len]\n\n    def _match_rule(self, rule_name):\n        key = (rule_name, self._stream.position())\n        if key in self._memo:\n            result, _, self._stream = self._memo[key]\n        else:\n            start = self._stream\n            result = getattr(self, "_rule_{}".format(rule_name))()\n            end = self._stream\n            self._memo[key] = (result, start, end)\n        return result\n\n    def _match_range(self, start, end):\n        original_stream = self._stream\n        next_objext, self._stream = self._stream.next()\n        if next_objext >= start and next_objext <= end:\n            return _SemanticAction(lambda _vars: next_objext, self._vars[-1])\n        else:\n            original_stream.fail(\n                "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)\n            )\n\n    def _match_string(self, string):\n        original_stream = self._stream\n        next_object, self._stream = self._stream.next()\n        if next_object == string:\n            return _SemanticAction(lambda _vars: string, self._vars[-1])\n        else:\n            original_stream.fail(\n                "expected {!r} but found {!r}".format(string, next_object)\n            )\n\n    def _match_charseq(self, charseq):\n        for char in charseq:\n            original_stream = self._stream\n            next_object, self._stream = self._stream.next()\n            if next_object != char:\n                original_stream.fail(\n                    "expected {!r} but found {!r}".format(char, next_object)\n                )\n        return _SemanticAction(lambda _vars: charseq, self._vars[-1])\n\n    def _match_any(self):\n        next_object, self._stream = self._stream.next()\n        return _SemanticAction(lambda _vars: next_object, self._vars[-1])\n\n    def _match_list(self, matcher):\n        original_stream = self._stream\n        next_object, next_stream = self._stream.next()\n        if isinstance(next_object, list):\n            self._stream = self._stream.nested(next_object)\n            matcher()\n            if self._stream.is_at_end():\n                self._stream = next_stream\n                return _SemanticAction(lambda _vars: next_object, self._vars[-1])\n        original_stream.fail("list match failed")\n\n    def _new_label(self):\n        self._label_counter += 1\n        return self._label_counter\n\n    def run(self, rule_name, input_object):\n        self._label_counter = 0\n        self._vars = [_Vars()]\n        self._memo = _Memo()\n        self._stream = _Stream.from_object(self._memo, input_object)\n        result = self._match_rule(rule_name).eval()\n        if isinstance(result, _Builder):\n            return result.build_string()\n        else:\n            return result\n\nclass _Vars(dict):\n\n    def bind(self, name, value):\n        self[name] = value\n        return value\n\n    def lookup(self, name):\n        return self[name]\n\nclass _SemanticAction(object):\n\n    def __init__(self, fn, vars_):\n        self.fn = fn\n        self.vars = vars_\n\n    def eval(self):\n        return self.fn(self.vars)\n\nclass _Label(object):\n\n    def __init__(self, grammar):\n        self.grammar = grammar\n        self.number = None\n\n    def eval(self):\n        if self.number is None:\n            self.number = self.grammar._new_label()\n        return self.number\n\nclass _Builder(object):\n\n    def build_string(self):\n        output = _Output()\n        self.write(output)\n        return output.flatten()\n\n    @classmethod\n    def create(self, item, at=None):\n        if at:\n            return _AtBuilder(at, _Builder.create(item))\n        if isinstance(item, _Builder):\n            return item\n        elif isinstance(item, list):\n            return _ListBuilder([_Builder.create(x) for x in item])\n        else:\n            return _AtomBuilder(item)\n\nclass _Output(object):\n\n    def __init__(self, parent=None):\n        self.parent = parent\n        self.parts = []\n        self.forks = {}\n\n    def indent(self):\n        self.write(1)\n\n    def dedent(self):\n        self.write(-1)\n\n    def write(self, value):\n        self.parts.append(value)\n\n    def fork(self, name):\n        fork = _Output(self)\n        self.write(fork)\n        self.forks[name] = fork\n\n    def get(self, fork):\n        if fork in self.forks:\n            return self.forks[fork]\n        elif self.parent is not None:\n            self.parent.get(fork)\n        else:\n            raise Exception("fork {} not found".format(fork))\n\n    def flatten(self):\n        parts = []\n        self._flatten(parts, 0)\n        return "".join(parts)\n\n    def _flatten(self, parts, indentation):\n        for part in self.parts:\n            if part == -1:\n                indentation -= 1\n            elif part == 1:\n                indentation += 1\n            elif isinstance(part, _Output):\n                part._flatten(parts, indentation)\n            else:\n                for ch in part:\n                    if not parts or (ch != "\\n" and parts[-1] == "\\n"):\n                        parts.append("    "*indentation)\n                    parts.append(ch)\n\nclass _AtBuilder(_Builder):\n\n    def __init__(self, fork, builder):\n        self.fork = fork\n        self.builder = builder\n\n    def write(self, output):\n        self.builder.write(output.get(self.fork))\n\nclass _ForkBuilder(_Builder):\n\n    def __init__(self, name):\n        self.name = name\n\n    def write(self, output):\n        output.fork(self.name)\n\nclass _ListBuilder(_Builder):\n\n    def __init__(self, builders):\n        self.builders = builders\n\n    def write(self, output):\n        for builder in self.builders:\n            builder.write(output)\n\nclass _AtomBuilder(_Builder):\n\n    def __init__(self, atom):\n        self.atom = atom\n\n    def write(self, output):\n        output.write(str(self.atom))\n\nclass _IndentBuilder(_Builder):\n\n    def write(self, output):\n        output.indent()\n\nclass _DedentBuilder(_Builder):\n\n    def write(self, output):\n        output.dedent()\n\nclass _Memo(dict):\n\n    def __init__(self):\n        dict.__init__(self)\n        self._latest_stream = _ObjectStream(self, [], position=-1)\n        self._latest_message = ""\n\n    def describe(self):\n        items = []\n        for (rule_name, _), (_, start, end) in self.items():\n            if end > start:\n                items.append((rule_name, start, end))\n        items.sort(key=lambda item: (item[2].position(), item[1].position()))\n        message = []\n        for item in items:\n            message.append("matched {: <20} {} -> {}\\n".format(*item))\n        message.append("\\n")\n        message.append("ERROR: {}: {}\\n".format(\n            self._latest_stream,\n            self._latest_message\n        ))\n        return "".join(message)\n\n    def fail(self, stream, message):\n        if stream.position() >= self._latest_stream.position():\n            self._latest_stream = stream\n            self._latest_message = message\n        raise _MatchError(self)\n\nclass _MatchError(Exception):\n\n    def __init__(self, memo):\n        Exception.__init__(self)\n        self._memo = memo\n\n    def describe(self):\n        return self._memo.describe()\n\nclass _Stream(object):\n\n    @classmethod\n    def from_object(cls, memo, input_object):\n        if isinstance(input_object, basestring):\n            return _CharStream(memo, list(input_object))\n        else:\n            return _ObjectStream(memo, [input_object])\n\n    def __init__(self, memo, objects):\n        self._memo = memo\n        self._objects = objects\n\n    def fail(self, message):\n        self._memo.fail(self, message)\n\n    def next(self):\n        if self.is_at_end():\n            self.fail("not eof")\n        next_object = self._objects[0]\n        return (\n            next_object,\n            self._advance(next_object, self._objects[1:]),\n        )\n\n    def is_at_end(self):\n        return len(self._objects) == 0\n\nclass _CharStream(_Stream):\n\n    def __init__(self, memo, objects, line=1, column=1):\n        _Stream.__init__(self, memo, objects)\n        self._line = line\n        self._column = column\n\n    def position(self):\n        return (self._line, self._column)\n\n    def _advance(self, next_object, objects):\n        if next_object == "\\n":\n            return _CharStream(self._memo, objects, self._line+1, 1)\n        else:\n            return _CharStream(self._memo, objects, self._line, self._column+1)\n\n    def __str__(self):\n        return "L{:03d}:C{:03d}".format(self._line, self._column)\n\nclass _ObjectStream(_Stream):\n\n    def __init__(self, memo, objects, parent=(), position=0):\n        _Stream.__init__(self, memo, objects)\n        self._parent = parent\n        self._position = position\n\n    def position(self):\n        return self._parent + (self._position,)\n\n    def nested(self, input_object):\n        return _ObjectStream(self._memo, input_object, self._parent+(self._position,))\n\n    def _advance(self, next_object, objects):\n        return _ObjectStream(self._memo, objects, self._parent, self._position+1)\n\n    def __str__(self):\n        return "[{}]".format(", ".join(str(x) for x in self.position()))\n'

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

class Parser(_Grammar):

    def _rule5(self):
        return self._match_rule('name')
    def _rule4(self):
        return self._vars[-1].bind('x', self._rule5())
    def _rule6(self):
        return self._match_rule('space')
    def _rule7(self):
        return self._match_charseq('{')
    def _rule10(self):
        return self._match_rule('rule')
    def _rule9(self):
        return self._star(self._rule10)
    def _rule8(self):
        return self._vars[-1].bind('ys', self._rule9())
    def _rule11(self):
        return self._match_rule('space')
    def _rule12(self):
        return self._match_charseq('}')
    def _rule13(self):
        return _SemanticAction(lambda _vars: (['Grammar']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]), self._vars[-1])
    def _rule3(self):
        return self._and([
            self._rule4,
            self._rule6,
            self._rule7,
            self._rule8,
            self._rule11,
            self._rule12,
            self._rule13,
        ])
    def _rule2(self):
        self._vars.append(_Vars())
        result = self._rule3()
        self._vars.pop(-1)
        return result
    def _rule1(self):
        return self._or([
            self._rule2,
        ])
    def _rule18(self):
        return self._match_rule('name')
    def _rule17(self):
        return self._vars[-1].bind('x', self._rule18())
    def _rule19(self):
        return self._match_rule('space')
    def _rule20(self):
        return self._match_charseq('=')
    def _rule22(self):
        return self._match_rule('choice')
    def _rule21(self):
        return self._vars[-1].bind('y', self._rule22())
    def _rule23(self):
        return _SemanticAction(lambda _vars: (['Rule']+[_vars.lookup('x').eval()]+[_vars.lookup('y').eval()]+[]), self._vars[-1])
    def _rule16(self):
        return self._and([
            self._rule17,
            self._rule19,
            self._rule20,
            self._rule21,
            self._rule23,
        ])
    def _rule15(self):
        self._vars.append(_Vars())
        result = self._rule16()
        self._vars.pop(-1)
        return result
    def _rule14(self):
        return self._or([
            self._rule15,
        ])
    def _rule31(self):
        return self._match_rule('space')
    def _rule32(self):
        return self._match_charseq('|')
    def _rule30(self):
        return self._and([
            self._rule31,
            self._rule32,
        ])
    def _rule29(self):
        self._vars.append(_Vars())
        result = self._rule30()
        self._vars.pop(-1)
        return result
    def _rule28(self):
        return self._or([
            self._rule29,
        ])
    def _rule33(self):
        return self._and([
        ])
    def _rule27(self):
        return self._or([
            self._rule28,
            self._rule33,
        ])
    def _rule35(self):
        return self._match_rule('sequence')
    def _rule34(self):
        return self._vars[-1].bind('x', self._rule35())
    def _rule41(self):
        return self._match_rule('space')
    def _rule42(self):
        return self._match_charseq('|')
    def _rule43(self):
        return self._match_rule('sequence')
    def _rule40(self):
        return self._and([
            self._rule41,
            self._rule42,
            self._rule43,
        ])
    def _rule39(self):
        self._vars.append(_Vars())
        result = self._rule40()
        self._vars.pop(-1)
        return result
    def _rule38(self):
        return self._or([
            self._rule39,
        ])
    def _rule37(self):
        return self._star(self._rule38)
    def _rule36(self):
        return self._vars[-1].bind('xs', self._rule37())
    def _rule44(self):
        return _SemanticAction(lambda _vars: (['Or']+[_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[]), self._vars[-1])
    def _rule26(self):
        return self._and([
            self._rule27,
            self._rule34,
            self._rule36,
            self._rule44,
        ])
    def _rule25(self):
        self._vars.append(_Vars())
        result = self._rule26()
        self._vars.pop(-1)
        return result
    def _rule24(self):
        return self._or([
            self._rule25,
        ])
    def _rule49(self):
        return self._match_rule('expr')
    def _rule48(self):
        return self._vars[-1].bind('x', self._rule49())
    def _rule52(self):
        return self._match_rule('expr')
    def _rule51(self):
        return self._star(self._rule52)
    def _rule50(self):
        return self._vars[-1].bind('xs', self._rule51())
    def _rule53(self):
        return _SemanticAction(lambda _vars: (['Scope']+[(['And']+[_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[])]+[]), self._vars[-1])
    def _rule47(self):
        return self._and([
            self._rule48,
            self._rule50,
            self._rule53,
        ])
    def _rule46(self):
        self._vars.append(_Vars())
        result = self._rule47()
        self._vars.pop(-1)
        return result
    def _rule45(self):
        return self._or([
            self._rule46,
        ])
    def _rule58(self):
        return self._match_rule('expr1')
    def _rule57(self):
        return self._vars[-1].bind('x', self._rule58())
    def _rule59(self):
        return self._match_rule('space')
    def _rule60(self):
        return self._match_charseq(':')
    def _rule62(self):
        return self._match_rule('name')
    def _rule61(self):
        return self._vars[-1].bind('y', self._rule62())
    def _rule63(self):
        return _SemanticAction(lambda _vars: (['Bind']+[_vars.lookup('y').eval()]+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule56(self):
        return self._and([
            self._rule57,
            self._rule59,
            self._rule60,
            self._rule61,
            self._rule63,
        ])
    def _rule55(self):
        self._vars.append(_Vars())
        result = self._rule56()
        self._vars.pop(-1)
        return result
    def _rule66(self):
        return self._match_rule('expr1')
    def _rule65(self):
        return self._and([
            self._rule66,
        ])
    def _rule64(self):
        self._vars.append(_Vars())
        result = self._rule65()
        self._vars.pop(-1)
        return result
    def _rule54(self):
        return self._or([
            self._rule55,
            self._rule64,
        ])
    def _rule71(self):
        return self._match_rule('expr2')
    def _rule70(self):
        return self._vars[-1].bind('x', self._rule71())
    def _rule72(self):
        return self._match_rule('space')
    def _rule73(self):
        return self._match_charseq('*')
    def _rule74(self):
        return _SemanticAction(lambda _vars: (['Star']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule69(self):
        return self._and([
            self._rule70,
            self._rule72,
            self._rule73,
            self._rule74,
        ])
    def _rule68(self):
        self._vars.append(_Vars())
        result = self._rule69()
        self._vars.pop(-1)
        return result
    def _rule78(self):
        return self._match_rule('expr2')
    def _rule77(self):
        return self._vars[-1].bind('x', self._rule78())
    def _rule79(self):
        return self._match_rule('space')
    def _rule80(self):
        return self._match_charseq('?')
    def _rule81(self):
        return _SemanticAction(lambda _vars: (['Or']+[_vars.lookup('x').eval()]+[(['And']+[])]+[]), self._vars[-1])
    def _rule76(self):
        return self._and([
            self._rule77,
            self._rule79,
            self._rule80,
            self._rule81,
        ])
    def _rule75(self):
        self._vars.append(_Vars())
        result = self._rule76()
        self._vars.pop(-1)
        return result
    def _rule84(self):
        return self._match_rule('space')
    def _rule85(self):
        return self._match_charseq('!')
    def _rule87(self):
        return self._match_rule('expr2')
    def _rule86(self):
        return self._vars[-1].bind('x', self._rule87())
    def _rule88(self):
        return _SemanticAction(lambda _vars: (['Not']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule83(self):
        return self._and([
            self._rule84,
            self._rule85,
            self._rule86,
            self._rule88,
        ])
    def _rule82(self):
        self._vars.append(_Vars())
        result = self._rule83()
        self._vars.pop(-1)
        return result
    def _rule91(self):
        return self._match_rule('space')
    def _rule92(self):
        return self._match_charseq('#')
    def _rule93(self):
        return _SemanticAction(lambda _vars: (['Label']+[]), self._vars[-1])
    def _rule90(self):
        return self._and([
            self._rule91,
            self._rule92,
            self._rule93,
        ])
    def _rule89(self):
        self._vars.append(_Vars())
        result = self._rule90()
        self._vars.pop(-1)
        return result
    def _rule96(self):
        return self._match_rule('expr2')
    def _rule95(self):
        return self._and([
            self._rule96,
        ])
    def _rule94(self):
        self._vars.append(_Vars())
        result = self._rule95()
        self._vars.pop(-1)
        return result
    def _rule67(self):
        return self._or([
            self._rule68,
            self._rule75,
            self._rule82,
            self._rule89,
            self._rule94,
        ])
    def _rule100(self):
        return self._match_rule('space')
    def _rule101(self):
        return self._match_charseq('->')
    def _rule103(self):
        return self._match_rule('hostExpr')
    def _rule102(self):
        return self._vars[-1].bind('x', self._rule103())
    def _rule104(self):
        return _SemanticAction(lambda _vars: (['SemanticAction']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule99(self):
        return self._and([
            self._rule100,
            self._rule101,
            self._rule102,
            self._rule104,
        ])
    def _rule98(self):
        self._vars.append(_Vars())
        result = self._rule99()
        self._vars.pop(-1)
        return result
    def _rule108(self):
        return self._match_rule('name')
    def _rule107(self):
        return self._vars[-1].bind('x', self._rule108())
    def _rule113(self):
        return self._match_rule('space')
    def _rule114(self):
        return self._match_charseq('=')
    def _rule112(self):
        return self._and([
            self._rule113,
            self._rule114,
        ])
    def _rule111(self):
        self._vars.append(_Vars())
        result = self._rule112()
        self._vars.pop(-1)
        return result
    def _rule110(self):
        return self._or([
            self._rule111,
        ])
    def _rule109(self):
        return self._not(self._rule110)
    def _rule115(self):
        return _SemanticAction(lambda _vars: (['MatchRule']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule106(self):
        return self._and([
            self._rule107,
            self._rule109,
            self._rule115,
        ])
    def _rule105(self):
        self._vars.append(_Vars())
        result = self._rule106()
        self._vars.pop(-1)
        return result
    def _rule118(self):
        return self._match_rule('space')
    def _rule120(self):
        return self._match_rule('char')
    def _rule119(self):
        return self._vars[-1].bind('x', self._rule120())
    def _rule121(self):
        return self._match_charseq('-')
    def _rule123(self):
        return self._match_rule('char')
    def _rule122(self):
        return self._vars[-1].bind('y', self._rule123())
    def _rule124(self):
        return _SemanticAction(lambda _vars: (['MatchRange']+[_vars.lookup('x').eval()]+[_vars.lookup('y').eval()]+[]), self._vars[-1])
    def _rule117(self):
        return self._and([
            self._rule118,
            self._rule119,
            self._rule121,
            self._rule122,
            self._rule124,
        ])
    def _rule116(self):
        self._vars.append(_Vars())
        result = self._rule117()
        self._vars.pop(-1)
        return result
    def _rule127(self):
        return self._match_rule('space')
    def _rule129(self):
        return self._match_rule('string')
    def _rule128(self):
        return self._vars[-1].bind('x', self._rule129())
    def _rule130(self):
        return _SemanticAction(lambda _vars: (['MatchString']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule126(self):
        return self._and([
            self._rule127,
            self._rule128,
            self._rule130,
        ])
    def _rule125(self):
        self._vars.append(_Vars())
        result = self._rule126()
        self._vars.pop(-1)
        return result
    def _rule133(self):
        return self._match_rule('space')
    def _rule135(self):
        return self._match_rule('charseq')
    def _rule134(self):
        return self._vars[-1].bind('x', self._rule135())
    def _rule136(self):
        return _SemanticAction(lambda _vars: (['MatchCharseq']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule132(self):
        return self._and([
            self._rule133,
            self._rule134,
            self._rule136,
        ])
    def _rule131(self):
        self._vars.append(_Vars())
        result = self._rule132()
        self._vars.pop(-1)
        return result
    def _rule139(self):
        return self._match_rule('space')
    def _rule140(self):
        return self._match_charseq('.')
    def _rule141(self):
        return _SemanticAction(lambda _vars: (['MatchAny']+[]), self._vars[-1])
    def _rule138(self):
        return self._and([
            self._rule139,
            self._rule140,
            self._rule141,
        ])
    def _rule137(self):
        self._vars.append(_Vars())
        result = self._rule138()
        self._vars.pop(-1)
        return result
    def _rule144(self):
        return self._match_rule('space')
    def _rule145(self):
        return self._match_charseq('(')
    def _rule147(self):
        return self._match_rule('choice')
    def _rule146(self):
        return self._vars[-1].bind('x', self._rule147())
    def _rule148(self):
        return self._match_rule('space')
    def _rule149(self):
        return self._match_charseq(')')
    def _rule150(self):
        return _SemanticAction(lambda _vars: _vars.lookup('x').eval(), self._vars[-1])
    def _rule143(self):
        return self._and([
            self._rule144,
            self._rule145,
            self._rule146,
            self._rule148,
            self._rule149,
            self._rule150,
        ])
    def _rule142(self):
        self._vars.append(_Vars())
        result = self._rule143()
        self._vars.pop(-1)
        return result
    def _rule153(self):
        return self._match_rule('space')
    def _rule154(self):
        return self._match_charseq('[')
    def _rule157(self):
        return self._match_rule('expr')
    def _rule156(self):
        return self._star(self._rule157)
    def _rule155(self):
        return self._vars[-1].bind('xs', self._rule156())
    def _rule158(self):
        return self._match_rule('space')
    def _rule159(self):
        return self._match_charseq(']')
    def _rule160(self):
        return _SemanticAction(lambda _vars: (['MatchList']+[(['And']+_vars.lookup('xs').eval()+[])]+[]), self._vars[-1])
    def _rule152(self):
        return self._and([
            self._rule153,
            self._rule154,
            self._rule155,
            self._rule158,
            self._rule159,
            self._rule160,
        ])
    def _rule151(self):
        self._vars.append(_Vars())
        result = self._rule152()
        self._vars.pop(-1)
        return result
    def _rule97(self):
        return self._or([
            self._rule98,
            self._rule105,
            self._rule116,
            self._rule125,
            self._rule131,
            self._rule137,
            self._rule142,
            self._rule151,
        ])
    def _rule164(self):
        return self._match_rule('space')
    def _rule166(self):
        return self._match_rule('string')
    def _rule165(self):
        return self._vars[-1].bind('x', self._rule166())
    def _rule167(self):
        return _SemanticAction(lambda _vars: (['String']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule163(self):
        return self._and([
            self._rule164,
            self._rule165,
            self._rule167,
        ])
    def _rule162(self):
        self._vars.append(_Vars())
        result = self._rule163()
        self._vars.pop(-1)
        return result
    def _rule170(self):
        return self._match_rule('space')
    def _rule171(self):
        return self._match_charseq('[')
    def _rule174(self):
        return self._match_rule('hostExprListItem')
    def _rule173(self):
        return self._star(self._rule174)
    def _rule172(self):
        return self._vars[-1].bind('xs', self._rule173())
    def _rule175(self):
        return self._match_rule('space')
    def _rule176(self):
        return self._match_charseq(']')
    def _rule177(self):
        return _SemanticAction(lambda _vars: (['List']+_vars.lookup('xs').eval()+[]), self._vars[-1])
    def _rule169(self):
        return self._and([
            self._rule170,
            self._rule171,
            self._rule172,
            self._rule175,
            self._rule176,
            self._rule177,
        ])
    def _rule168(self):
        self._vars.append(_Vars())
        result = self._rule169()
        self._vars.pop(-1)
        return result
    def _rule181(self):
        return self._match_rule('at')
    def _rule180(self):
        return self._vars[-1].bind('x', self._rule181())
    def _rule182(self):
        return self._match_rule('space')
    def _rule183(self):
        return self._match_charseq('{')
    def _rule186(self):
        return self._match_rule('buildExpr')
    def _rule185(self):
        return self._star(self._rule186)
    def _rule184(self):
        return self._vars[-1].bind('ys', self._rule185())
    def _rule187(self):
        return self._match_rule('space')
    def _rule188(self):
        return self._match_charseq('}')
    def _rule189(self):
        return _SemanticAction(lambda _vars: (['Builder']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]), self._vars[-1])
    def _rule179(self):
        return self._and([
            self._rule180,
            self._rule182,
            self._rule183,
            self._rule184,
            self._rule187,
            self._rule188,
            self._rule189,
        ])
    def _rule178(self):
        self._vars.append(_Vars())
        result = self._rule179()
        self._vars.pop(-1)
        return result
    def _rule193(self):
        return self._match_rule('name')
    def _rule192(self):
        return self._vars[-1].bind('x', self._rule193())
    def _rule194(self):
        return self._match_rule('space')
    def _rule195(self):
        return self._match_charseq('(')
    def _rule198(self):
        return self._match_rule('hostExpr')
    def _rule197(self):
        return self._star(self._rule198)
    def _rule196(self):
        return self._vars[-1].bind('ys', self._rule197())
    def _rule199(self):
        return self._match_rule('space')
    def _rule200(self):
        return self._match_charseq(')')
    def _rule201(self):
        return _SemanticAction(lambda _vars: (['FnCall']+[_vars.lookup('x').eval()]+_vars.lookup('ys').eval()+[]), self._vars[-1])
    def _rule191(self):
        return self._and([
            self._rule192,
            self._rule194,
            self._rule195,
            self._rule196,
            self._rule199,
            self._rule200,
            self._rule201,
        ])
    def _rule190(self):
        self._vars.append(_Vars())
        result = self._rule191()
        self._vars.pop(-1)
        return result
    def _rule205(self):
        return self._match_rule('name')
    def _rule204(self):
        return self._vars[-1].bind('x', self._rule205())
    def _rule206(self):
        return _SemanticAction(lambda _vars: (['VarLookup']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule203(self):
        return self._and([
            self._rule204,
            self._rule206,
        ])
    def _rule202(self):
        self._vars.append(_Vars())
        result = self._rule203()
        self._vars.pop(-1)
        return result
    def _rule161(self):
        return self._or([
            self._rule162,
            self._rule168,
            self._rule178,
            self._rule190,
            self._rule202,
        ])
    def _rule210(self):
        return self._match_rule('space')
    def _rule211(self):
        return self._match_charseq('~')
    def _rule213(self):
        return self._match_rule('hostExpr')
    def _rule212(self):
        return self._vars[-1].bind('x', self._rule213())
    def _rule214(self):
        return _SemanticAction(lambda _vars: (['ListItemSplice']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule209(self):
        return self._and([
            self._rule210,
            self._rule211,
            self._rule212,
            self._rule214,
        ])
    def _rule208(self):
        self._vars.append(_Vars())
        result = self._rule209()
        self._vars.pop(-1)
        return result
    def _rule217(self):
        return self._match_rule('hostExpr')
    def _rule216(self):
        return self._and([
            self._rule217,
        ])
    def _rule215(self):
        self._vars.append(_Vars())
        result = self._rule216()
        self._vars.pop(-1)
        return result
    def _rule207(self):
        return self._or([
            self._rule208,
            self._rule215,
        ])
    def _rule221(self):
        return self._match_rule('space')
    def _rule222(self):
        return self._match_charseq('>')
    def _rule223(self):
        return _SemanticAction(lambda _vars: (['IndentBuilder']+[]), self._vars[-1])
    def _rule220(self):
        return self._and([
            self._rule221,
            self._rule222,
            self._rule223,
        ])
    def _rule219(self):
        self._vars.append(_Vars())
        result = self._rule220()
        self._vars.pop(-1)
        return result
    def _rule226(self):
        return self._match_rule('space')
    def _rule227(self):
        return self._match_charseq('<')
    def _rule228(self):
        return _SemanticAction(lambda _vars: (['DedentBuilder']+[]), self._vars[-1])
    def _rule225(self):
        return self._and([
            self._rule226,
            self._rule227,
            self._rule228,
        ])
    def _rule224(self):
        self._vars.append(_Vars())
        result = self._rule225()
        self._vars.pop(-1)
        return result
    def _rule231(self):
        return self._match_rule('space')
    def _rule232(self):
        return self._match_charseq('#')
    def _rule234(self):
        return self._match_rule('name')
    def _rule233(self):
        return self._vars[-1].bind('x', self._rule234())
    def _rule235(self):
        return _SemanticAction(lambda _vars: (['Fork']+[_vars.lookup('x').eval()]+[]), self._vars[-1])
    def _rule230(self):
        return self._and([
            self._rule231,
            self._rule232,
            self._rule233,
            self._rule235,
        ])
    def _rule229(self):
        self._vars.append(_Vars())
        result = self._rule230()
        self._vars.pop(-1)
        return result
    def _rule238(self):
        return self._match_rule('hostExpr')
    def _rule237(self):
        return self._and([
            self._rule238,
        ])
    def _rule236(self):
        self._vars.append(_Vars())
        result = self._rule237()
        self._vars.pop(-1)
        return result
    def _rule218(self):
        return self._or([
            self._rule219,
            self._rule224,
            self._rule229,
            self._rule236,
        ])
    def _rule242(self):
        return self._match_rule('space')
    def _rule243(self):
        return self._match_charseq('@')
    def _rule244(self):
        return self._match_rule('name')
    def _rule241(self):
        return self._and([
            self._rule242,
            self._rule243,
            self._rule244,
        ])
    def _rule240(self):
        self._vars.append(_Vars())
        result = self._rule241()
        self._vars.pop(-1)
        return result
    def _rule247(self):
        return _SemanticAction(lambda _vars: '', self._vars[-1])
    def _rule246(self):
        return self._and([
            self._rule247,
        ])
    def _rule245(self):
        self._vars.append(_Vars())
        result = self._rule246()
        self._vars.pop(-1)
        return result
    def _rule239(self):
        return self._or([
            self._rule240,
            self._rule245,
        ])
    def _rule251(self):
        return self._match_charseq('"')
    def _rule258(self):
        return self._match_charseq('"')
    def _rule257(self):
        return self._not(self._rule258)
    def _rule259(self):
        return self._match_rule('innerChar')
    def _rule256(self):
        return self._and([
            self._rule257,
            self._rule259,
        ])
    def _rule255(self):
        self._vars.append(_Vars())
        result = self._rule256()
        self._vars.pop(-1)
        return result
    def _rule254(self):
        return self._or([
            self._rule255,
        ])
    def _rule253(self):
        return self._star(self._rule254)
    def _rule252(self):
        return self._vars[-1].bind('xs', self._rule253())
    def _rule260(self):
        return self._match_charseq('"')
    def _rule261(self):
        return _SemanticAction(lambda _vars: join(
            _vars.lookup('xs').eval(),
        ), self._vars[-1])
    def _rule250(self):
        return self._and([
            self._rule251,
            self._rule252,
            self._rule260,
            self._rule261,
        ])
    def _rule249(self):
        self._vars.append(_Vars())
        result = self._rule250()
        self._vars.pop(-1)
        return result
    def _rule248(self):
        return self._or([
            self._rule249,
        ])
    def _rule265(self):
        return self._match_charseq("'")
    def _rule272(self):
        return self._match_charseq("'")
    def _rule271(self):
        return self._not(self._rule272)
    def _rule273(self):
        return self._match_rule('innerChar')
    def _rule270(self):
        return self._and([
            self._rule271,
            self._rule273,
        ])
    def _rule269(self):
        self._vars.append(_Vars())
        result = self._rule270()
        self._vars.pop(-1)
        return result
    def _rule268(self):
        return self._or([
            self._rule269,
        ])
    def _rule267(self):
        return self._star(self._rule268)
    def _rule266(self):
        return self._vars[-1].bind('xs', self._rule267())
    def _rule274(self):
        return self._match_charseq("'")
    def _rule275(self):
        return _SemanticAction(lambda _vars: join(
            _vars.lookup('xs').eval(),
        ), self._vars[-1])
    def _rule264(self):
        return self._and([
            self._rule265,
            self._rule266,
            self._rule274,
            self._rule275,
        ])
    def _rule263(self):
        self._vars.append(_Vars())
        result = self._rule264()
        self._vars.pop(-1)
        return result
    def _rule262(self):
        return self._or([
            self._rule263,
        ])
    def _rule279(self):
        return self._match_charseq("'")
    def _rule281(self):
        return self._match_charseq("'")
    def _rule280(self):
        return self._not(self._rule281)
    def _rule283(self):
        return self._match_rule('innerChar')
    def _rule282(self):
        return self._vars[-1].bind('x', self._rule283())
    def _rule284(self):
        return self._match_charseq("'")
    def _rule285(self):
        return _SemanticAction(lambda _vars: _vars.lookup('x').eval(), self._vars[-1])
    def _rule278(self):
        return self._and([
            self._rule279,
            self._rule280,
            self._rule282,
            self._rule284,
            self._rule285,
        ])
    def _rule277(self):
        self._vars.append(_Vars())
        result = self._rule278()
        self._vars.pop(-1)
        return result
    def _rule276(self):
        return self._or([
            self._rule277,
        ])
    def _rule289(self):
        return self._match_charseq('\\')
    def _rule290(self):
        return self._match_rule('escape')
    def _rule288(self):
        return self._and([
            self._rule289,
            self._rule290,
        ])
    def _rule287(self):
        self._vars.append(_Vars())
        result = self._rule288()
        self._vars.pop(-1)
        return result
    def _rule292(self):
        return self._and([
            self._match_any,
        ])
    def _rule291(self):
        self._vars.append(_Vars())
        result = self._rule292()
        self._vars.pop(-1)
        return result
    def _rule286(self):
        return self._or([
            self._rule287,
            self._rule291,
        ])
    def _rule296(self):
        return self._match_charseq('\\')
    def _rule297(self):
        return _SemanticAction(lambda _vars: '\\', self._vars[-1])
    def _rule295(self):
        return self._and([
            self._rule296,
            self._rule297,
        ])
    def _rule294(self):
        self._vars.append(_Vars())
        result = self._rule295()
        self._vars.pop(-1)
        return result
    def _rule300(self):
        return self._match_charseq("'")
    def _rule301(self):
        return _SemanticAction(lambda _vars: "'", self._vars[-1])
    def _rule299(self):
        return self._and([
            self._rule300,
            self._rule301,
        ])
    def _rule298(self):
        self._vars.append(_Vars())
        result = self._rule299()
        self._vars.pop(-1)
        return result
    def _rule304(self):
        return self._match_charseq('"')
    def _rule305(self):
        return _SemanticAction(lambda _vars: '"', self._vars[-1])
    def _rule303(self):
        return self._and([
            self._rule304,
            self._rule305,
        ])
    def _rule302(self):
        self._vars.append(_Vars())
        result = self._rule303()
        self._vars.pop(-1)
        return result
    def _rule308(self):
        return self._match_charseq('n')
    def _rule309(self):
        return _SemanticAction(lambda _vars: '\n', self._vars[-1])
    def _rule307(self):
        return self._and([
            self._rule308,
            self._rule309,
        ])
    def _rule306(self):
        self._vars.append(_Vars())
        result = self._rule307()
        self._vars.pop(-1)
        return result
    def _rule293(self):
        return self._or([
            self._rule294,
            self._rule298,
            self._rule302,
            self._rule306,
        ])
    def _rule313(self):
        return self._match_rule('space')
    def _rule315(self):
        return self._match_rule('nameStart')
    def _rule314(self):
        return self._vars[-1].bind('x', self._rule315())
    def _rule318(self):
        return self._match_rule('nameChar')
    def _rule317(self):
        return self._star(self._rule318)
    def _rule316(self):
        return self._vars[-1].bind('xs', self._rule317())
    def _rule319(self):
        return _SemanticAction(lambda _vars: join(
            ([_vars.lookup('x').eval()]+_vars.lookup('xs').eval()+[]),
        ), self._vars[-1])
    def _rule312(self):
        return self._and([
            self._rule313,
            self._rule314,
            self._rule316,
            self._rule319,
        ])
    def _rule311(self):
        self._vars.append(_Vars())
        result = self._rule312()
        self._vars.pop(-1)
        return result
    def _rule310(self):
        return self._or([
            self._rule311,
        ])
    def _rule323(self):
        return self._match_range('a', 'z')
    def _rule322(self):
        return self._and([
            self._rule323,
        ])
    def _rule321(self):
        self._vars.append(_Vars())
        result = self._rule322()
        self._vars.pop(-1)
        return result
    def _rule326(self):
        return self._match_range('A', 'Z')
    def _rule325(self):
        return self._and([
            self._rule326,
        ])
    def _rule324(self):
        self._vars.append(_Vars())
        result = self._rule325()
        self._vars.pop(-1)
        return result
    def _rule320(self):
        return self._or([
            self._rule321,
            self._rule324,
        ])
    def _rule330(self):
        return self._match_range('a', 'z')
    def _rule329(self):
        return self._and([
            self._rule330,
        ])
    def _rule328(self):
        self._vars.append(_Vars())
        result = self._rule329()
        self._vars.pop(-1)
        return result
    def _rule333(self):
        return self._match_range('A', 'Z')
    def _rule332(self):
        return self._and([
            self._rule333,
        ])
    def _rule331(self):
        self._vars.append(_Vars())
        result = self._rule332()
        self._vars.pop(-1)
        return result
    def _rule336(self):
        return self._match_range('0', '9')
    def _rule335(self):
        return self._and([
            self._rule336,
        ])
    def _rule334(self):
        self._vars.append(_Vars())
        result = self._rule335()
        self._vars.pop(-1)
        return result
    def _rule327(self):
        return self._or([
            self._rule328,
            self._rule331,
            self._rule334,
        ])
    def _rule344(self):
        return self._match_charseq(' ')
    def _rule343(self):
        return self._and([
            self._rule344,
        ])
    def _rule342(self):
        self._vars.append(_Vars())
        result = self._rule343()
        self._vars.pop(-1)
        return result
    def _rule347(self):
        return self._match_charseq('\n')
    def _rule346(self):
        return self._and([
            self._rule347,
        ])
    def _rule345(self):
        self._vars.append(_Vars())
        result = self._rule346()
        self._vars.pop(-1)
        return result
    def _rule341(self):
        return self._or([
            self._rule342,
            self._rule345,
        ])
    def _rule340(self):
        return self._star(self._rule341)
    def _rule339(self):
        return self._and([
            self._rule340,
        ])
    def _rule338(self):
        self._vars.append(_Vars())
        result = self._rule339()
        self._vars.pop(-1)
        return result
    def _rule337(self):
        return self._or([
            self._rule338,
        ])

    def _rule_grammar(self):
        return self._rule1()

    def _rule_rule(self):
        return self._rule14()

    def _rule_choice(self):
        return self._rule24()

    def _rule_sequence(self):
        return self._rule45()

    def _rule_expr(self):
        return self._rule54()

    def _rule_expr1(self):
        return self._rule67()

    def _rule_expr2(self):
        return self._rule97()

    def _rule_hostExpr(self):
        return self._rule161()

    def _rule_hostExprListItem(self):
        return self._rule207()

    def _rule_buildExpr(self):
        return self._rule218()

    def _rule_at(self):
        return self._rule239()

    def _rule_string(self):
        return self._rule248()

    def _rule_charseq(self):
        return self._rule262()

    def _rule_char(self):
        return self._rule276()

    def _rule_innerChar(self):
        return self._rule286()

    def _rule_escape(self):
        return self._rule293()

    def _rule_name(self):
        return self._rule310()

    def _rule_nameStart(self):
        return self._rule320()

    def _rule_nameChar(self):
        return self._rule327()

    def _rule_space(self):
        return self._rule337()

class CodeGenerator(_Grammar):

    def _rule6(self):
        return self._match_string('Grammar')
    def _rule7(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule10(self):
        return self._match_rule('ast')
    def _rule9(self):
        return self._star(self._rule10)
    def _rule8(self):
        return self._vars[-1].bind('ys', self._rule9())
    def _rule5(self):
        return self._and([
            self._rule6,
            self._rule7,
            self._rule8,
        ])
    def _rule4(self):
        return self._match_list(self._rule5)
    def _rule11(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'class ',
            _vars.lookup('x').eval(),
            '(_Grammar):\n',
            _IndentBuilder(),
            _ForkBuilder('rules'),
            '\n',
            _vars.lookup('ys').eval(),
            _DedentBuilder(),
        ], ''), self._vars[-1])
    def _rule3(self):
        return self._and([
            self._rule4,
            self._rule11,
        ])
    def _rule2(self):
        self._vars.append(_Vars())
        result = self._rule3()
        self._vars.pop(-1)
        return result
    def _rule16(self):
        return self._match_string('Rule')
    def _rule17(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule19(self):
        return self._match_rule('ast')
    def _rule18(self):
        return self._vars[-1].bind('y', self._rule19())
    def _rule15(self):
        return self._and([
            self._rule16,
            self._rule17,
            self._rule18,
        ])
    def _rule14(self):
        return self._match_list(self._rule15)
    def _rule20(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '\ndef _rule_',
            _vars.lookup('x').eval(),
            '(self):\n',
            _IndentBuilder(),
            'return ',
            _vars.lookup('y').eval(),
            '()\n',
            _DedentBuilder(),
        ], ''), self._vars[-1])
    def _rule13(self):
        return self._and([
            self._rule14,
            self._rule20,
        ])
    def _rule12(self):
        self._vars.append(_Vars())
        result = self._rule13()
        self._vars.pop(-1)
        return result
    def _rule25(self):
        return self._match_string('MatchAny')
    def _rule24(self):
        return self._and([
            self._rule25,
        ])
    def _rule23(self):
        return self._match_list(self._rule24)
    def _rule26(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'self._match_any',
        ], ''), self._vars[-1])
    def _rule22(self):
        return self._and([
            self._rule23,
            self._rule26,
        ])
    def _rule21(self):
        self._vars.append(_Vars())
        result = self._rule22()
        self._vars.pop(-1)
        return result
    def _rule31(self):
        return self._match_string('String')
    def _rule32(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule30(self):
        return self._and([
            self._rule31,
            self._rule32,
        ])
    def _rule29(self):
        return self._match_list(self._rule30)
    def _rule33(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            repr(
                _vars.lookup('x').eval(),
            ),
        ], ''), self._vars[-1])
    def _rule28(self):
        return self._and([
            self._rule29,
            self._rule33,
        ])
    def _rule27(self):
        self._vars.append(_Vars())
        result = self._rule28()
        self._vars.pop(-1)
        return result
    def _rule38(self):
        return self._match_string('List')
    def _rule40(self):
        return self._match_rule('astList')
    def _rule39(self):
        return self._vars[-1].bind('x', self._rule40())
    def _rule37(self):
        return self._and([
            self._rule38,
            self._rule39,
        ])
    def _rule36(self):
        return self._match_list(self._rule37)
    def _rule41(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            _vars.lookup('x').eval(),
        ], ''), self._vars[-1])
    def _rule35(self):
        return self._and([
            self._rule36,
            self._rule41,
        ])
    def _rule34(self):
        self._vars.append(_Vars())
        result = self._rule35()
        self._vars.pop(-1)
        return result
    def _rule46(self):
        return self._match_string('Builder')
    def _rule47(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule49(self):
        return self._match_rule('astItems')
    def _rule48(self):
        return self._vars[-1].bind('y', self._rule49())
    def _rule45(self):
        return self._and([
            self._rule46,
            self._rule47,
            self._rule48,
        ])
    def _rule44(self):
        return self._match_list(self._rule45)
    def _rule50(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '_Builder.create([',
            _vars.lookup('y').eval(),
            '], ',
            repr(
                _vars.lookup('x').eval(),
            ),
            ')',
        ], ''), self._vars[-1])
    def _rule43(self):
        return self._and([
            self._rule44,
            self._rule50,
        ])
    def _rule42(self):
        self._vars.append(_Vars())
        result = self._rule43()
        self._vars.pop(-1)
        return result
    def _rule55(self):
        return self._match_string('Fork')
    def _rule56(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule54(self):
        return self._and([
            self._rule55,
            self._rule56,
        ])
    def _rule53(self):
        return self._match_list(self._rule54)
    def _rule57(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '_ForkBuilder(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ')',
        ], ''), self._vars[-1])
    def _rule52(self):
        return self._and([
            self._rule53,
            self._rule57,
        ])
    def _rule51(self):
        self._vars.append(_Vars())
        result = self._rule52()
        self._vars.pop(-1)
        return result
    def _rule62(self):
        return self._match_string('IndentBuilder')
    def _rule61(self):
        return self._and([
            self._rule62,
        ])
    def _rule60(self):
        return self._match_list(self._rule61)
    def _rule63(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '_IndentBuilder()',
        ], ''), self._vars[-1])
    def _rule59(self):
        return self._and([
            self._rule60,
            self._rule63,
        ])
    def _rule58(self):
        self._vars.append(_Vars())
        result = self._rule59()
        self._vars.pop(-1)
        return result
    def _rule68(self):
        return self._match_string('DedentBuilder')
    def _rule67(self):
        return self._and([
            self._rule68,
        ])
    def _rule66(self):
        return self._match_list(self._rule67)
    def _rule69(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '_DedentBuilder()',
        ], ''), self._vars[-1])
    def _rule65(self):
        return self._and([
            self._rule66,
            self._rule69,
        ])
    def _rule64(self):
        self._vars.append(_Vars())
        result = self._rule65()
        self._vars.pop(-1)
        return result
    def _rule74(self):
        return self._match_string('FnCall')
    def _rule75(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule77(self):
        return self._match_rule('astItems')
    def _rule76(self):
        return self._vars[-1].bind('y', self._rule77())
    def _rule73(self):
        return self._and([
            self._rule74,
            self._rule75,
            self._rule76,
        ])
    def _rule72(self):
        return self._match_list(self._rule73)
    def _rule78(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            _vars.lookup('x').eval(),
            '(',
            _vars.lookup('y').eval(),
            ')',
        ], ''), self._vars[-1])
    def _rule71(self):
        return self._and([
            self._rule72,
            self._rule78,
        ])
    def _rule70(self):
        self._vars.append(_Vars())
        result = self._rule71()
        self._vars.pop(-1)
        return result
    def _rule83(self):
        return self._match_string('VarLookup')
    def _rule84(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule82(self):
        return self._and([
            self._rule83,
            self._rule84,
        ])
    def _rule81(self):
        return self._match_list(self._rule82)
    def _rule85(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '_vars.lookup(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ').eval()',
        ], ''), self._vars[-1])
    def _rule80(self):
        return self._and([
            self._rule81,
            self._rule85,
        ])
    def _rule79(self):
        self._vars.append(_Vars())
        result = self._rule80()
        self._vars.pop(-1)
        return result
    def _rule89(self):
        return self._match_rule('astFnBody')
    def _rule88(self):
        return self._vars[-1].bind('x', self._rule89())
    def _rule91(self):
        return _Label(self)
    def _rule90(self):
        return self._vars[-1].bind('y', self._rule91())
    def _rule92(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            _Builder.create([
                _ForkBuilder('rules'),
                '\ndef _rule',
                _vars.lookup('y').eval(),
                '(self):\n',
                _IndentBuilder(),
                _vars.lookup('x').eval(),
                _DedentBuilder(),
            ], 'rules'),
            'self._rule',
            _vars.lookup('y').eval(),
        ], ''), self._vars[-1])
    def _rule87(self):
        return self._and([
            self._rule88,
            self._rule90,
            self._rule92,
        ])
    def _rule86(self):
        self._vars.append(_Vars())
        result = self._rule87()
        self._vars.pop(-1)
        return result
    def _rule1(self):
        return self._or([
            self._rule2,
            self._rule12,
            self._rule21,
            self._rule27,
            self._rule34,
            self._rule42,
            self._rule51,
            self._rule58,
            self._rule64,
            self._rule70,
            self._rule79,
            self._rule86,
        ])
    def _rule98(self):
        return self._match_string('Or')
    def _rule100(self):
        return self._match_rule('astItems')
    def _rule99(self):
        return self._vars[-1].bind('x', self._rule100())
    def _rule97(self):
        return self._and([
            self._rule98,
            self._rule99,
        ])
    def _rule96(self):
        return self._match_list(self._rule97)
    def _rule101(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._or([',
            _vars.lookup('x').eval(),
            '])',
        ], ''), self._vars[-1])
    def _rule95(self):
        return self._and([
            self._rule96,
            self._rule101,
        ])
    def _rule94(self):
        self._vars.append(_Vars())
        result = self._rule95()
        self._vars.pop(-1)
        return result
    def _rule106(self):
        return self._match_string('Scope')
    def _rule108(self):
        return self._match_rule('ast')
    def _rule107(self):
        return self._vars[-1].bind('x', self._rule108())
    def _rule105(self):
        return self._and([
            self._rule106,
            self._rule107,
        ])
    def _rule104(self):
        return self._match_list(self._rule105)
    def _rule109(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'self._vars.append(_Vars())\n',
            'result = ',
            _vars.lookup('x').eval(),
            '()\n',
            'self._vars.pop(-1)\n',
            'return result',
        ], ''), self._vars[-1])
    def _rule103(self):
        return self._and([
            self._rule104,
            self._rule109,
        ])
    def _rule102(self):
        self._vars.append(_Vars())
        result = self._rule103()
        self._vars.pop(-1)
        return result
    def _rule114(self):
        return self._match_string('And')
    def _rule116(self):
        return self._match_rule('astItems')
    def _rule115(self):
        return self._vars[-1].bind('x', self._rule116())
    def _rule113(self):
        return self._and([
            self._rule114,
            self._rule115,
        ])
    def _rule112(self):
        return self._match_list(self._rule113)
    def _rule117(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._and([',
            _vars.lookup('x').eval(),
            '])',
        ], ''), self._vars[-1])
    def _rule111(self):
        return self._and([
            self._rule112,
            self._rule117,
        ])
    def _rule110(self):
        self._vars.append(_Vars())
        result = self._rule111()
        self._vars.pop(-1)
        return result
    def _rule122(self):
        return self._match_string('Bind')
    def _rule123(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule125(self):
        return self._match_rule('ast')
    def _rule124(self):
        return self._vars[-1].bind('y', self._rule125())
    def _rule121(self):
        return self._and([
            self._rule122,
            self._rule123,
            self._rule124,
        ])
    def _rule120(self):
        return self._match_list(self._rule121)
    def _rule126(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._vars[-1].bind(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ', ',
            _vars.lookup('y').eval(),
            '())',
        ], ''), self._vars[-1])
    def _rule119(self):
        return self._and([
            self._rule120,
            self._rule126,
        ])
    def _rule118(self):
        self._vars.append(_Vars())
        result = self._rule119()
        self._vars.pop(-1)
        return result
    def _rule131(self):
        return self._match_string('Star')
    def _rule133(self):
        return self._match_rule('ast')
    def _rule132(self):
        return self._vars[-1].bind('x', self._rule133())
    def _rule130(self):
        return self._and([
            self._rule131,
            self._rule132,
        ])
    def _rule129(self):
        return self._match_list(self._rule130)
    def _rule134(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._star(',
            _vars.lookup('x').eval(),
            ')',
        ], ''), self._vars[-1])
    def _rule128(self):
        return self._and([
            self._rule129,
            self._rule134,
        ])
    def _rule127(self):
        self._vars.append(_Vars())
        result = self._rule128()
        self._vars.pop(-1)
        return result
    def _rule139(self):
        return self._match_string('Not')
    def _rule141(self):
        return self._match_rule('ast')
    def _rule140(self):
        return self._vars[-1].bind('x', self._rule141())
    def _rule138(self):
        return self._and([
            self._rule139,
            self._rule140,
        ])
    def _rule137(self):
        return self._match_list(self._rule138)
    def _rule142(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._not(',
            _vars.lookup('x').eval(),
            ')',
        ], ''), self._vars[-1])
    def _rule136(self):
        return self._and([
            self._rule137,
            self._rule142,
        ])
    def _rule135(self):
        self._vars.append(_Vars())
        result = self._rule136()
        self._vars.pop(-1)
        return result
    def _rule147(self):
        return self._match_string('SemanticAction')
    def _rule149(self):
        return self._match_rule('ast')
    def _rule148(self):
        return self._vars[-1].bind('x', self._rule149())
    def _rule146(self):
        return self._and([
            self._rule147,
            self._rule148,
        ])
    def _rule145(self):
        return self._match_list(self._rule146)
    def _rule150(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return _SemanticAction(lambda _vars: ',
            _vars.lookup('x').eval(),
            ', self._vars[-1])',
        ], ''), self._vars[-1])
    def _rule144(self):
        return self._and([
            self._rule145,
            self._rule150,
        ])
    def _rule143(self):
        self._vars.append(_Vars())
        result = self._rule144()
        self._vars.pop(-1)
        return result
    def _rule155(self):
        return self._match_string('Label')
    def _rule154(self):
        return self._and([
            self._rule155,
        ])
    def _rule153(self):
        return self._match_list(self._rule154)
    def _rule156(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return _Label(self)',
        ], ''), self._vars[-1])
    def _rule152(self):
        return self._and([
            self._rule153,
            self._rule156,
        ])
    def _rule151(self):
        self._vars.append(_Vars())
        result = self._rule152()
        self._vars.pop(-1)
        return result
    def _rule161(self):
        return self._match_string('MatchRule')
    def _rule162(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule160(self):
        return self._and([
            self._rule161,
            self._rule162,
        ])
    def _rule159(self):
        return self._match_list(self._rule160)
    def _rule163(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._match_rule(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ')',
        ], ''), self._vars[-1])
    def _rule158(self):
        return self._and([
            self._rule159,
            self._rule163,
        ])
    def _rule157(self):
        self._vars.append(_Vars())
        result = self._rule158()
        self._vars.pop(-1)
        return result
    def _rule168(self):
        return self._match_string('MatchRange')
    def _rule169(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule170(self):
        return self._vars[-1].bind('y', self._match_any())
    def _rule167(self):
        return self._and([
            self._rule168,
            self._rule169,
            self._rule170,
        ])
    def _rule166(self):
        return self._match_list(self._rule167)
    def _rule171(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._match_range(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ', ',
            repr(
                _vars.lookup('y').eval(),
            ),
            ')',
        ], ''), self._vars[-1])
    def _rule165(self):
        return self._and([
            self._rule166,
            self._rule171,
        ])
    def _rule164(self):
        self._vars.append(_Vars())
        result = self._rule165()
        self._vars.pop(-1)
        return result
    def _rule176(self):
        return self._match_string('MatchString')
    def _rule177(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule175(self):
        return self._and([
            self._rule176,
            self._rule177,
        ])
    def _rule174(self):
        return self._match_list(self._rule175)
    def _rule178(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._match_string(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ')',
        ], ''), self._vars[-1])
    def _rule173(self):
        return self._and([
            self._rule174,
            self._rule178,
        ])
    def _rule172(self):
        self._vars.append(_Vars())
        result = self._rule173()
        self._vars.pop(-1)
        return result
    def _rule183(self):
        return self._match_string('MatchCharseq')
    def _rule184(self):
        return self._vars[-1].bind('x', self._match_any())
    def _rule182(self):
        return self._and([
            self._rule183,
            self._rule184,
        ])
    def _rule181(self):
        return self._match_list(self._rule182)
    def _rule185(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._match_charseq(',
            repr(
                _vars.lookup('x').eval(),
            ),
            ')',
        ], ''), self._vars[-1])
    def _rule180(self):
        return self._and([
            self._rule181,
            self._rule185,
        ])
    def _rule179(self):
        self._vars.append(_Vars())
        result = self._rule180()
        self._vars.pop(-1)
        return result
    def _rule190(self):
        return self._match_string('MatchList')
    def _rule192(self):
        return self._match_rule('ast')
    def _rule191(self):
        return self._vars[-1].bind('x', self._rule192())
    def _rule189(self):
        return self._and([
            self._rule190,
            self._rule191,
        ])
    def _rule188(self):
        return self._match_list(self._rule189)
    def _rule193(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            'return self._match_list(',
            _vars.lookup('x').eval(),
            ')',
        ], ''), self._vars[-1])
    def _rule187(self):
        return self._and([
            self._rule188,
            self._rule193,
        ])
    def _rule186(self):
        self._vars.append(_Vars())
        result = self._rule187()
        self._vars.pop(-1)
        return result
    def _rule93(self):
        return self._or([
            self._rule94,
            self._rule102,
            self._rule110,
            self._rule118,
            self._rule127,
            self._rule135,
            self._rule143,
            self._rule151,
            self._rule157,
            self._rule164,
            self._rule172,
            self._rule179,
            self._rule186,
        ])
    def _rule199(self):
        return self._match_rule('astItem')
    def _rule198(self):
        return self._star(self._rule199)
    def _rule197(self):
        return self._vars[-1].bind('xs', self._rule198())
    def _rule200(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '\n',
            _IndentBuilder(),
            _vars.lookup('xs').eval(),
            _DedentBuilder(),
        ], ''), self._vars[-1])
    def _rule196(self):
        return self._and([
            self._rule197,
            self._rule200,
        ])
    def _rule195(self):
        self._vars.append(_Vars())
        result = self._rule196()
        self._vars.pop(-1)
        return result
    def _rule194(self):
        return self._or([
            self._rule195,
        ])
    def _rule205(self):
        return self._match_rule('ast')
    def _rule204(self):
        return self._vars[-1].bind('x', self._rule205())
    def _rule206(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            _vars.lookup('x').eval(),
            ',\n',
        ], ''), self._vars[-1])
    def _rule203(self):
        return self._and([
            self._rule204,
            self._rule206,
        ])
    def _rule202(self):
        self._vars.append(_Vars())
        result = self._rule203()
        self._vars.pop(-1)
        return result
    def _rule201(self):
        return self._or([
            self._rule202,
        ])
    def _rule212(self):
        return self._match_rule('astListItem')
    def _rule211(self):
        return self._star(self._rule212)
    def _rule210(self):
        return self._vars[-1].bind('xs', self._rule211())
    def _rule213(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '(',
            _vars.lookup('xs').eval(),
            '[])',
        ], ''), self._vars[-1])
    def _rule209(self):
        return self._and([
            self._rule210,
            self._rule213,
        ])
    def _rule208(self):
        self._vars.append(_Vars())
        result = self._rule209()
        self._vars.pop(-1)
        return result
    def _rule207(self):
        return self._or([
            self._rule208,
        ])
    def _rule219(self):
        return self._match_string('ListItemSplice')
    def _rule221(self):
        return self._match_rule('ast')
    def _rule220(self):
        return self._vars[-1].bind('x', self._rule221())
    def _rule218(self):
        return self._and([
            self._rule219,
            self._rule220,
        ])
    def _rule217(self):
        return self._match_list(self._rule218)
    def _rule222(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            _vars.lookup('x').eval(),
            '+',
        ], ''), self._vars[-1])
    def _rule216(self):
        return self._and([
            self._rule217,
            self._rule222,
        ])
    def _rule215(self):
        self._vars.append(_Vars())
        result = self._rule216()
        self._vars.pop(-1)
        return result
    def _rule226(self):
        return self._match_rule('ast')
    def _rule225(self):
        return self._vars[-1].bind('x', self._rule226())
    def _rule227(self):
        return _SemanticAction(lambda _vars: _Builder.create([
            '[',
            _vars.lookup('x').eval(),
            ']+',
        ], ''), self._vars[-1])
    def _rule224(self):
        return self._and([
            self._rule225,
            self._rule227,
        ])
    def _rule223(self):
        self._vars.append(_Vars())
        result = self._rule224()
        self._vars.pop(-1)
        return result
    def _rule214(self):
        return self._or([
            self._rule215,
            self._rule223,
        ])

    def _rule_ast(self):
        return self._rule1()

    def _rule_astFnBody(self):
        return self._rule93()

    def _rule_astItems(self):
        return self._rule194()

    def _rule_astItem(self):
        return self._rule201()

    def _rule_astList(self):
        return self._rule207()

    def _rule_astListItem(self):
        return self._rule214()

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
