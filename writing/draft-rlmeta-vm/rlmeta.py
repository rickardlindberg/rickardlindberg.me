import sys

SUPPORT = 'try:\n    from cStringIO import StringIO\nexcept:\n    from StringIO import StringIO\n\nclass _Program(object):\n\n    def __init__(self):\n        self._instructions = []\n        self._labels = {}\n        self._load()\n\n    def _label(self, name):\n        self._labels[name] = len(self._instructions)\n\n    def _instruction(self, name, arg1=None, arg2=None):\n        self._instructions.append((name, arg1, arg2))\n\n    def run(self, rule_name, input_object):\n        self._label_counter = 0\n        self._vars = []\n        self._stack = []\n        self._action = _NewSemanticAction(lambda env: None, None)\n        self._pc = self._labels[rule_name]\n        if isinstance(input_object, basestring):\n            self._input = input_object\n        else:\n            self._input = [input_object]\n        self._pos = 0\n        self._input_stack = []\n        while True:\n            name, arg1, arg2 = self._instructions[self._pc]\n            next_pc = self._pc + 1\n            fail = ""\n            if name == \'\':\n                pass\n            elif name == \'CALL\':\n                self._stack.append(self._pc+1)\n                next_pc = self._labels[arg1]\n            elif name == \'RETURN\':\n                if len(self._stack) == 0:\n                    result = self._action.eval()\n                    if isinstance(result, _Builder):\n                        return result.build_string()\n                    else:\n                        return result\n                next_pc = self._stack.pop()\n            elif name == \'BACKTRACK\':\n                self._stack.append(("backtrack", self._labels[arg1], self._pos, len(self._input_stack), len(self._vars)))\n            elif name == \'LABEL\':\n                self._action = _NewSemanticAction(lambda env: env, self._label_counter)\n                self._label_counter += 1\n            elif name == \'COMMIT\':\n                while not isinstance(self._stack[-1], tuple):\n                    self._stack.pop()\n                self._stack.pop()\n                next_pc = self._labels[arg1]\n            elif name == \'MATCH_CHARSEQ\':\n                for char in arg1:\n                    if self._pos >= len(self._input) or self._input[self._pos] != char:\n                        fail = "match charseq"\n                        break\n                    self._pos += 1\n                else:\n                    self._action = _NewSemanticAction(lambda env: env, arg1)\n            elif name == \'MATCH_RANGE\':\n                if self._pos >= len(self._input) or not (arg1 <= self._input[self._pos] <= arg2):\n                    fail = "match range"\n                else:\n                    self._action = _NewSemanticAction(lambda env: env, self._input[self._pos])\n                    self._pos += 1\n            elif name == \'MATCH_ANY\':\n                if self._pos >= len(self._input):\n                    fail = "match any"\n                else:\n                    self._action = _NewSemanticAction(lambda env: env, self._input[self._pos])\n                    self._pos += 1\n            elif name == \'MATCH_STRING\':\n                if self._pos >= len(self._input) or self._input[self._pos] != arg1:\n                    fail = "match string {}".format(arg1)\n                else:\n                    self._action = _NewSemanticAction(lambda env: env, arg1)\n                    self._pos += 1\n            elif name == \'BIND\':\n                self._vars[-1][arg1] = self._action\n            elif name == \'ACTION\':\n                self._action = _NewSemanticAction(arg1, self._vars[-1])\n            elif name == \'PUSH_SCOPE\':\n                self._vars.append({})\n            elif name == \'POP_SCOPE\':\n                self._vars.pop()\n            elif name == \'PUSH_INPUT\':\n                if self._pos >= len(self._input) or not isinstance(self._input[self._pos], list):\n                    fail = "push input"\n                else:\n                    self._input_stack.append((self._input, self._pos+1))\n                    self._input = self._input[self._pos]\n                    self._pos = 0\n            elif name == \'MATCH_CALL_RULE\':\n                if self._pos >= len(self._input):\n                    fail = "match call rule"\n                else:\n                    self._stack.append(self._pc+1)\n                    next_pc = self._labels[self._input[self._pos]]\n                    self._pos += 1\n            elif name == \'POP_INPUT\':\n                if self._pos != len(self._input):\n                    fail = "pop input"\n                else:\n                    self._input, self._pos = self._input_stack.pop()\n            elif name == \'LIST_START\':\n                self._vars.append([])\n            elif name == \'LIST_APPEND\':\n                self._vars[-1].append(self._action)\n            elif name == \'LIST_END\':\n                self._action = _NewSemanticAction(lambda env: [x.eval() for x in env], self._vars.pop())\n            elif name == \'FAIL\':\n                pass\n            else:\n                raise Exception("unknown command {}".format(name))\n            if name == \'FAIL\' or fail:\n                while self._stack and not isinstance(self._stack[-1], tuple):\n                    self._stack.pop()\n                if not self._stack:\n                    raise Exception("totally failed")\n                x = (_, next_pc, self._pos, input_len, vars_len) = self._stack.pop()\n                self._vars = self._vars[:vars_len]\n                if len(self._input_stack) > input_len:\n                    self._input = self._input_stack[input_len][0]\n                    self._input_stack = self._input_stack[:input_len]\n            self._pc = next_pc\n\nclass _Grammar(object):\n\n    def _or(self, matchers):\n        original_stream = self._stream\n        for matcher in matchers[:-1]:\n            try:\n                return matcher()\n            except _MatchError:\n                self._stream = original_stream\n        return matchers[-1]()\n\n    def _and(self, matchers):\n        result = None\n        for matcher in matchers:\n            result = matcher()\n        return result\n\n    def _star(self, matcher):\n        result = []\n        while True:\n            original_stream = self._stream\n            try:\n                result.append(matcher())\n            except _MatchError:\n                self._stream = original_stream\n                return _SemanticAction(lambda: [x.eval() for x in result])\n\n    def _not(self, matcher):\n        original_stream = self._stream\n        try:\n            matcher()\n        except _MatchError:\n            return _SemanticAction(lambda: None)\n        else:\n            original_stream.fail(lambda: "match found")\n        finally:\n            self._stream = original_stream\n\n    def _match_rule(self, rule_name):\n        key = (rule_name, self._stream.position())\n        if key in self._memo:\n            result, _, self._stream = self._memo[key]\n        else:\n            start = self._stream\n            result = getattr(self, "_rule_{}".format(rule_name))()\n            end = self._stream\n            self._memo[key] = (result, start, end)\n        return result\n\n    def _match_range(self, start, end):\n        next_objext = self._stream.peek()\n        if next_objext >= start and next_objext <= end:\n            self._stream = self._stream.advance()\n            return _SemanticAction(lambda: next_objext)\n        else:\n            self._stream.fail(\n                lambda: "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)\n            )\n\n    def _match_string(self, string):\n        next_object = self._stream.peek()\n        if next_object == string:\n            self._stream = self._stream.advance()\n            return _SemanticAction(lambda: string)\n        else:\n            self._stream.fail(\n                lambda: "expected {!r} but found {!r}".format(string, next_object)\n            )\n\n    def _match_charseq(self, charseq):\n        for char in charseq:\n            next_object = self._stream.peek()\n            if next_object != char:\n                self._stream.fail(\n                    lambda: "expected {!r} but found {!r}".format(char, next_object)\n                )\n            self._stream = self._stream.advance()\n        return _SemanticAction(lambda: charseq)\n\n    def _match_any(self):\n        next_object = self._stream.peek()\n        self._stream = self._stream.advance()\n        return _SemanticAction(lambda: next_object)\n\n    def _match_call_rule(self):\n        next_object = self._stream.peek()\n        self._stream = self._stream.advance()\n        return self._match_rule(str(next_object))\n\n    def _match_list(self, matcher):\n        original_stream = self._stream\n        next_object = self._stream.peek()\n        if isinstance(next_object, list):\n            self._stream = self._stream.nested(next_object)\n            matcher()\n            if self._stream.is_at_end():\n                self._stream = original_stream.advance()\n                return _SemanticAction(lambda: next_object)\n        original_stream.fail(lambda: "list match failed")\n\n    def _create_label(self):\n        current = self._label_counter\n        self._label_counter += 1\n        return _SemanticAction(lambda: current)\n\n    def run(self, rule_name, input_object):\n        self._memo = _Memo()\n        self._stream = _Stream.from_object(self._memo, input_object)\n        self._label_counter = 0\n        result = self._match_rule(rule_name).eval()\n        if isinstance(result, _Builder):\n            return result.build_string()\n        else:\n            return result\n\nclass _Vars(dict):\n\n    def bind(self, name, value):\n        self[name] = value\n        return value\n\n    def lookup(self, name):\n        return self[name]\n\nclass _SemanticAction(object):\n\n    def __init__(self, fn):\n        self.fn = fn\n\n    def eval(self):\n        return self.fn()\n\nclass _NewSemanticAction(object):\n\n    def __init__(self, fn, env):\n        self.fn = fn\n        self.env = env\n\n    def eval(self):\n        return self.fn(self.env)\n\nclass _Builder(object):\n\n    def build_string(self):\n        output = _Output()\n        self.write(output)\n        return output.value\n\n    @classmethod\n    def create(self, item):\n        if isinstance(item, _Builder):\n            return item\n        elif isinstance(item, list):\n            return _ListBuilder([_Builder.create(x) for x in item])\n        else:\n            return _AtomBuilder(item)\n\nclass _Output(object):\n\n    def __init__(self):\n        self.buffer = StringIO()\n        self.indentation = 0\n        self.on_newline = True\n\n    @property\n    def value(self):\n        return self.buffer.getvalue()\n\n    def write(self, value):\n        for ch in value:\n            is_linebreak = ch == "\\n"\n            if self.indentation and self.on_newline and not is_linebreak:\n                self.buffer.write("    "*self.indentation)\n            self.buffer.write(ch)\n            self.on_newline = is_linebreak\n\nclass _ListBuilder(_Builder):\n\n    def __init__(self, builders):\n        self.builders = builders\n\n    def write(self, output):\n        for builder in self.builders:\n            builder.write(output)\n\nclass _AtomBuilder(_Builder):\n\n    def __init__(self, atom):\n        self.atom = atom\n\n    def write(self, output):\n        output.write(str(self.atom))\n\nclass _IndentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation += 1\n\nclass _DedentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation -= 1\n\nclass _Memo(dict):\n\n    def __init__(self):\n        dict.__init__(self)\n        self._latest_stream = _ObjectStream(self, [], -1)\n        self._latest_lazy_message = lambda: ""\n\n    def describe(self):\n        items = []\n        for (rule_name, _), (_, start, end) in self.items():\n            if end > start:\n                items.append((rule_name, start, end))\n        items.sort(key=lambda item: (item[2].position(), item[1].position()))\n        message = []\n        for item in items:\n            message.append("matched {: <20} {} -> {}\\n".format(*item))\n        message.append("\\n")\n        message.append("ERROR: {}: {}\\n".format(\n            self._latest_stream,\n            self._latest_lazy_message()\n        ))\n        return "".join(message)\n\n    def fail(self, stream, lazy_message):\n        if stream.position() >= self._latest_stream.position():\n            self._latest_stream = stream\n            self._latest_lazy_message = lazy_message\n        raise _MatchError(self)\n\nclass _MatchError(Exception):\n\n    def __init__(self, memo):\n        Exception.__init__(self)\n        self._memo = memo\n\n    def describe(self):\n        return self._memo.describe()\n\nclass _Stream(object):\n\n    @classmethod\n    def from_object(cls, memo, input_object):\n        if isinstance(input_object, basestring):\n            return _CharStream(memo, input_object, 0)\n        else:\n            return _ObjectStream(memo, [input_object], 0)\n\n    def __init__(self, memo, objects, index):\n        self._memo = memo\n        self._objects = objects\n        self._index = index\n\n    def fail(self, lazy_message):\n        self._memo.fail(self, lazy_message)\n\n    def peek(self):\n        if self.is_at_end():\n            self.fail("not eof")\n        return self._objects[self._index]\n\n    def is_at_end(self):\n        return self._index >= len(self._objects)\n\nclass _CharStream(_Stream):\n\n    def __init__(self, memo, objects, index, line=1, column=1):\n        _Stream.__init__(self, memo, objects, index)\n        self._line = line\n        self._column = column\n\n    def position(self):\n        return self._index\n\n    def advance(self):\n        if self._objects[self._index] == "\\n":\n            line = self._line + 1\n            column = 1\n        else:\n            line = self._line\n            column = self._column + 1\n        return _CharStream(self._memo, self._objects, self._index+1, line, column)\n\n    def __str__(self):\n        return "L{:03d}:C{:03d}".format(self._line, self._column)\n\nclass _ObjectStream(_Stream):\n\n    def __init__(self, memo, objects, index, parent=()):\n        _Stream.__init__(self, memo, objects, index)\n        self._parent_position = parent\n        self._position = self._parent_position + (self._index,)\n\n    def position(self):\n        return self._position\n\n    def nested(self, input_object):\n        return _ObjectStream(self._memo, input_object, 0, self._position)\n\n    def advance(self):\n        return _ObjectStream(self._memo, self._objects, self._index+1, self._parent_position)\n\n    def __str__(self):\n        return "[{}]".format(", ".join(str(x) for x in self.position()))\n'

try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

class _Program(object):

    def __init__(self):
        self._instructions = []
        self._labels = {}
        self._load()

    def _label(self, name):
        self._labels[name] = len(self._instructions)

    def _instruction(self, name, arg1=None, arg2=None):
        self._instructions.append((name, arg1, arg2))

    def run(self, rule_name, input_object):
        self._label_counter = 0
        self._vars = []
        self._stack = []
        self._action = _NewSemanticAction(lambda env: None, None)
        self._pc = self._labels[rule_name]
        if isinstance(input_object, basestring):
            self._input = input_object
        else:
            self._input = [input_object]
        self._pos = 0
        self._input_stack = []
        while True:
            name, arg1, arg2 = self._instructions[self._pc]
            next_pc = self._pc + 1
            fail = ""
            if name == '':
                pass
            elif name == 'CALL':
                self._stack.append(self._pc+1)
                next_pc = self._labels[arg1]
            elif name == 'RETURN':
                if len(self._stack) == 0:
                    result = self._action.eval()
                    if isinstance(result, _Builder):
                        return result.build_string()
                    else:
                        return result
                next_pc = self._stack.pop()
            elif name == 'BACKTRACK':
                self._stack.append(("backtrack", self._labels[arg1], self._pos, len(self._input_stack), len(self._vars)))
            elif name == 'LABEL':
                self._action = _NewSemanticAction(lambda env: env, self._label_counter)
                self._label_counter += 1
            elif name == 'COMMIT':
                while not isinstance(self._stack[-1], tuple):
                    self._stack.pop()
                self._stack.pop()
                next_pc = self._labels[arg1]
            elif name == 'MATCH_CHARSEQ':
                for char in arg1:
                    if self._pos >= len(self._input) or self._input[self._pos] != char:
                        fail = "match charseq"
                        break
                    self._pos += 1
                else:
                    self._action = _NewSemanticAction(lambda env: env, arg1)
            elif name == 'MATCH_RANGE':
                if self._pos >= len(self._input) or not (arg1 <= self._input[self._pos] <= arg2):
                    fail = "match range"
                else:
                    self._action = _NewSemanticAction(lambda env: env, self._input[self._pos])
                    self._pos += 1
            elif name == 'MATCH_ANY':
                if self._pos >= len(self._input):
                    fail = "match any"
                else:
                    self._action = _NewSemanticAction(lambda env: env, self._input[self._pos])
                    self._pos += 1
            elif name == 'MATCH_STRING':
                if self._pos >= len(self._input) or self._input[self._pos] != arg1:
                    fail = "match string {}".format(arg1)
                else:
                    self._action = _NewSemanticAction(lambda env: env, arg1)
                    self._pos += 1
            elif name == 'BIND':
                self._vars[-1][arg1] = self._action
            elif name == 'ACTION':
                self._action = _NewSemanticAction(arg1, self._vars[-1])
            elif name == 'PUSH_SCOPE':
                self._vars.append({})
            elif name == 'POP_SCOPE':
                self._vars.pop()
            elif name == 'PUSH_INPUT':
                if self._pos >= len(self._input) or not isinstance(self._input[self._pos], list):
                    fail = "push input"
                else:
                    self._input_stack.append((self._input, self._pos+1))
                    self._input = self._input[self._pos]
                    self._pos = 0
            elif name == 'MATCH_CALL_RULE':
                if self._pos >= len(self._input):
                    fail = "match call rule"
                else:
                    self._stack.append(self._pc+1)
                    next_pc = self._labels[self._input[self._pos]]
                    self._pos += 1
            elif name == 'POP_INPUT':
                if self._pos != len(self._input):
                    fail = "pop input"
                else:
                    self._input, self._pos = self._input_stack.pop()
            elif name == 'LIST_START':
                self._vars.append([])
            elif name == 'LIST_APPEND':
                self._vars[-1].append(self._action)
            elif name == 'LIST_END':
                self._action = _NewSemanticAction(lambda env: [x.eval() for x in env], self._vars.pop())
            elif name == 'FAIL':
                pass
            else:
                raise Exception("unknown command {}".format(name))
            if name == 'FAIL' or fail:
                while self._stack and not isinstance(self._stack[-1], tuple):
                    self._stack.pop()
                if not self._stack:
                    raise Exception("totally failed")
                x = (_, next_pc, self._pos, input_len, vars_len) = self._stack.pop()
                self._vars = self._vars[:vars_len]
                if len(self._input_stack) > input_len:
                    self._input = self._input_stack[input_len][0]
                    self._input_stack = self._input_stack[:input_len]
            self._pc = next_pc

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
            result, _, self._stream = self._memo[key]
        else:
            start = self._stream
            result = getattr(self, "_rule_{}".format(rule_name))()
            end = self._stream
            self._memo[key] = (result, start, end)
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

    def _create_label(self):
        current = self._label_counter
        self._label_counter += 1
        return _SemanticAction(lambda: current)

    def run(self, rule_name, input_object):
        self._memo = _Memo()
        self._stream = _Stream.from_object(self._memo, input_object)
        self._label_counter = 0
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

class _NewSemanticAction(object):

    def __init__(self, fn, env):
        self.fn = fn
        self.env = env

    def eval(self):
        return self.fn(self.env)

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

    def fail(self, lazy_message):
        self._memo.fail(self, lazy_message)

    def peek(self):
        if self.is_at_end():
            self.fail("not eof")
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

class Parser(_Program):
    def _load(self):
        self._label('grammar')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'name')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '{')
        self._instruction('LIST_START')
        self._label(2)
        self._instruction('BACKTRACK', 3)
        self._instruction('CALL', 'rule')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 2)
        self._label(3)
        self._instruction('LIST_END')
        self._instruction('BIND', 'ys')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '}')
        self._instruction('ACTION', lambda env: ['Grammar']+[env['x'].eval()]+env['ys'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('rule')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'name')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '=')
        self._instruction('CALL', 'choice')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: ['Rule']+[env['x'].eval()]+[env['y'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('choice')
        self._instruction('PUSH_SCOPE')
        self._instruction('BACKTRACK', 6)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '|')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 7)
        self._label(6)
        self._label(7)
        self._instruction('CALL', 'sequence')
        self._instruction('BIND', 'x')
        self._instruction('LIST_START')
        self._label(10)
        self._instruction('BACKTRACK', 11)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '|')
        self._instruction('CALL', 'sequence')
        self._instruction('POP_SCOPE')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 10)
        self._label(11)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('ACTION', lambda env: ['Or']+[env['x'].eval()]+env['xs'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('sequence')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'expr')
        self._instruction('BIND', 'x')
        self._instruction('LIST_START')
        self._label(14)
        self._instruction('BACKTRACK', 15)
        self._instruction('CALL', 'expr')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 14)
        self._label(15)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('ACTION', lambda env: ['Scope']+[['And']+[env['x'].eval()]+env['xs'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('expr')
        self._instruction('BACKTRACK', 16)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'expr1')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', ':')
        self._instruction('CALL', 'name')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: ['Bind']+[env['y'].eval()]+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 17)
        self._label(16)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'expr1')
        self._instruction('POP_SCOPE')
        self._label(17)
        self._instruction('RETURN')
        self._label('expr1')
        self._instruction('BACKTRACK', 26)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'expr2')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '*')
        self._instruction('ACTION', lambda env: ['Star']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 27)
        self._label(26)
        self._instruction('BACKTRACK', 24)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'expr2')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '?')
        self._instruction('ACTION', lambda env: ['Or']+[env['x'].eval()]+[['And']])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 25)
        self._label(24)
        self._instruction('BACKTRACK', 22)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '!')
        self._instruction('CALL', 'expr2')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['Not']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 23)
        self._label(22)
        self._instruction('BACKTRACK', 20)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '%')
        self._instruction('ACTION', lambda env: ['MatchCallRule'])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 21)
        self._label(20)
        self._instruction('BACKTRACK', 18)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '#')
        self._instruction('ACTION', lambda env: ['Label'])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 19)
        self._label(18)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'expr2')
        self._instruction('POP_SCOPE')
        self._label(19)
        self._label(21)
        self._label(23)
        self._label(25)
        self._label(27)
        self._instruction('RETURN')
        self._label('expr2')
        self._instruction('BACKTRACK', 52)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '->')
        self._instruction('CALL', 'hostExpr')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['SemanticAction']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 53)
        self._label(52)
        self._instruction('BACKTRACK', 50)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'name')
        self._instruction('BIND', 'x')
        self._instruction('BACKTRACK', 35)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '=')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 34)
        self._label(34)
        self._instruction('FAIL')
        self._label(35)
        self._instruction('ACTION', lambda env: ['MatchRule']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 51)
        self._label(50)
        self._instruction('BACKTRACK', 48)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('CALL', 'char')
        self._instruction('BIND', 'x')
        self._instruction('MATCH_CHARSEQ', '-')
        self._instruction('CALL', 'char')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: ['MatchRange']+[env['x'].eval()]+[env['y'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 49)
        self._label(48)
        self._instruction('BACKTRACK', 46)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('CALL', 'string')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['MatchString']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 47)
        self._label(46)
        self._instruction('BACKTRACK', 44)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('CALL', 'charseq')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['MatchCharseq']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 45)
        self._label(44)
        self._instruction('BACKTRACK', 42)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '.')
        self._instruction('ACTION', lambda env: ['MatchAny'])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 43)
        self._label(42)
        self._instruction('BACKTRACK', 40)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '(')
        self._instruction('CALL', 'choice')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', ')')
        self._instruction('ACTION', lambda env: env['x'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 41)
        self._label(40)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '[')
        self._instruction('LIST_START')
        self._label(38)
        self._instruction('BACKTRACK', 39)
        self._instruction('CALL', 'expr')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 38)
        self._label(39)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', ']')
        self._instruction('ACTION', lambda env: ['MatchList']+[['And']+env['xs'].eval()])
        self._instruction('POP_SCOPE')
        self._label(41)
        self._label(43)
        self._label(45)
        self._label(47)
        self._label(49)
        self._label(51)
        self._label(53)
        self._instruction('RETURN')
        self._label('hostExpr')
        self._instruction('BACKTRACK', 84)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('CALL', 'string')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['String']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 85)
        self._label(84)
        self._instruction('BACKTRACK', 82)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '[')
        self._instruction('LIST_START')
        self._label(60)
        self._instruction('BACKTRACK', 61)
        self._instruction('CALL', 'hostExprListItem')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 60)
        self._label(61)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', ']')
        self._instruction('ACTION', lambda env: ['List']+env['xs'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 83)
        self._label(82)
        self._instruction('BACKTRACK', 80)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '{')
        self._instruction('LIST_START')
        self._label(68)
        self._instruction('BACKTRACK', 69)
        self._instruction('CALL', 'buildExpr')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 68)
        self._label(69)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '}')
        self._instruction('ACTION', lambda env: ['Builder']+env['xs'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 81)
        self._label(80)
        self._instruction('BACKTRACK', 78)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'name')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '(')
        self._instruction('LIST_START')
        self._label(76)
        self._instruction('BACKTRACK', 77)
        self._instruction('CALL', 'hostExpr')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 76)
        self._label(77)
        self._instruction('LIST_END')
        self._instruction('BIND', 'ys')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', ')')
        self._instruction('ACTION', lambda env: ['FnCall']+[env['x'].eval()]+env['ys'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 79)
        self._label(78)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'name')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['VarLookup']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._label(79)
        self._label(81)
        self._label(83)
        self._label(85)
        self._instruction('RETURN')
        self._label('hostExprListItem')
        self._instruction('BACKTRACK', 86)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '~')
        self._instruction('CALL', 'hostExpr')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: ['ListItemSplice']+[env['x'].eval()])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 87)
        self._label(86)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'hostExpr')
        self._instruction('POP_SCOPE')
        self._label(87)
        self._instruction('RETURN')
        self._label('buildExpr')
        self._instruction('BACKTRACK', 90)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '>')
        self._instruction('ACTION', lambda env: ['IndentBuilder'])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 91)
        self._label(90)
        self._instruction('BACKTRACK', 88)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('MATCH_CHARSEQ', '<')
        self._instruction('ACTION', lambda env: ['DedentBuilder'])
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 89)
        self._label(88)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'hostExpr')
        self._instruction('POP_SCOPE')
        self._label(89)
        self._label(91)
        self._instruction('RETURN')
        self._label('string')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', '"')
        self._instruction('LIST_START')
        self._label(102)
        self._instruction('BACKTRACK', 103)
        self._instruction('PUSH_SCOPE')
        self._instruction('BACKTRACK', 101)
        self._instruction('MATCH_CHARSEQ', '"')
        self._instruction('COMMIT', 100)
        self._label(100)
        self._instruction('FAIL')
        self._label(101)
        self._instruction('CALL', 'innerChar')
        self._instruction('POP_SCOPE')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 102)
        self._label(103)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('MATCH_CHARSEQ', '"')
        self._instruction('ACTION', lambda env: join(env['xs'].eval()))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('charseq')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('LIST_START')
        self._label(114)
        self._instruction('BACKTRACK', 115)
        self._instruction('PUSH_SCOPE')
        self._instruction('BACKTRACK', 113)
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('COMMIT', 112)
        self._label(112)
        self._instruction('FAIL')
        self._label(113)
        self._instruction('CALL', 'innerChar')
        self._instruction('POP_SCOPE')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 114)
        self._label(115)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('ACTION', lambda env: join(env['xs'].eval()))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('char')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('BACKTRACK', 119)
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('COMMIT', 118)
        self._label(118)
        self._instruction('FAIL')
        self._label(119)
        self._instruction('CALL', 'innerChar')
        self._instruction('BIND', 'x')
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('ACTION', lambda env: env['x'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('innerChar')
        self._instruction('BACKTRACK', 120)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', '\\')
        self._instruction('CALL', 'escape')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 121)
        self._label(120)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('POP_SCOPE')
        self._label(121)
        self._instruction('RETURN')
        self._label('escape')
        self._instruction('BACKTRACK', 126)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', '\\')
        self._instruction('ACTION', lambda env: '\\')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 127)
        self._label(126)
        self._instruction('BACKTRACK', 124)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', "'")
        self._instruction('ACTION', lambda env: "'")
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 125)
        self._label(124)
        self._instruction('BACKTRACK', 122)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', '"')
        self._instruction('ACTION', lambda env: '"')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 123)
        self._label(122)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', 'n')
        self._instruction('ACTION', lambda env: '\n')
        self._instruction('POP_SCOPE')
        self._label(123)
        self._label(125)
        self._label(127)
        self._instruction('RETURN')
        self._label('name')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'space')
        self._instruction('CALL', 'nameStart')
        self._instruction('BIND', 'x')
        self._instruction('LIST_START')
        self._label(130)
        self._instruction('BACKTRACK', 131)
        self._instruction('CALL', 'nameChar')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 130)
        self._label(131)
        self._instruction('LIST_END')
        self._instruction('BIND', 'xs')
        self._instruction('ACTION', lambda env: join([env['x'].eval()]+env['xs'].eval()))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('nameStart')
        self._instruction('BACKTRACK', 132)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_RANGE', 'a', 'z')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 133)
        self._label(132)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_RANGE', 'A', 'Z')
        self._instruction('POP_SCOPE')
        self._label(133)
        self._instruction('RETURN')
        self._label('nameChar')
        self._instruction('BACKTRACK', 136)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_RANGE', 'a', 'z')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 137)
        self._label(136)
        self._instruction('BACKTRACK', 134)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_RANGE', 'A', 'Z')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 135)
        self._label(134)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_RANGE', '0', '9')
        self._instruction('POP_SCOPE')
        self._label(135)
        self._label(137)
        self._instruction('RETURN')
        self._label('space')
        self._instruction('PUSH_SCOPE')
        self._instruction('LIST_START')
        self._label(140)
        self._instruction('BACKTRACK', 141)
        self._instruction('BACKTRACK', 138)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', ' ')
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 139)
        self._label(138)
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_CHARSEQ', '\n')
        self._instruction('POP_SCOPE')
        self._label(139)
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 140)
        self._label(141)
        self._instruction('LIST_END')
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')

class CodeGenerator(_Program):
    def _load(self):
        self._label('Grammar')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('LIST_START')
        self._label(2)
        self._instruction('BACKTRACK', 3)
        self._instruction('CALL', 'ast')
        self._instruction('LIST_APPEND')
        self._instruction('COMMIT', 2)
        self._label(3)
        self._instruction('LIST_END')
        self._instruction('BIND', 'ys')
        self._instruction('ACTION', lambda env: _Builder.create(['class ', env['x'].eval(), '(_Program):\n', _IndentBuilder(), 'def _load(self):\n', _IndentBuilder(), env['ys'].eval(), _DedentBuilder(), _DedentBuilder()]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('Rule')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create(['self._label(', repr(env['x'].eval()), ')\n', env['y'].eval(), "self._instruction('RETURN')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchAny')
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('MATCH_ANY')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchCallRule')
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('MATCH_CALL_RULE')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('String')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create([repr(env['x'].eval())]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('List')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'astList')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('Builder')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'astItems')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(['_Builder.create([', env['x'].eval(), '])']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('IndentBuilder')
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create(['_IndentBuilder()']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('DedentBuilder')
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create(['_DedentBuilder()']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('FnCall')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'astItems')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval(), '(', env['y'].eval(), ')']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('VarLookup')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(['env[', repr(env['x'].eval()), '].eval()']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('Or')
        self._instruction('BACKTRACK', 14)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('BACKTRACK', 11)
        self._instruction('MATCH_ANY')
        self._instruction('COMMIT', 10)
        self._label(10)
        self._instruction('FAIL')
        self._label(11)
        self._instruction('ACTION', lambda env: env['x'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 15)
        self._label(14)
        self._instruction('BACKTRACK', 12)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'Or')
        self._instruction('BIND', 'y')
        self._instruction('LABEL')
        self._instruction('BIND', 'a')
        self._instruction('LABEL')
        self._instruction('BIND', 'b')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('BACKTRACK', ", env['a'].eval(), ')\n', env['x'].eval(), "self._instruction('COMMIT', ", env['b'].eval(), ')\n', 'self._label(', env['a'].eval(), ')\n', env['y'].eval(), 'self._label(', env['b'].eval(), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 13)
        self._label(12)
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create([]))
        self._instruction('POP_SCOPE')
        self._label(13)
        self._label(15)
        self._instruction('RETURN')
        self._label('Scope')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('PUSH_SCOPE')\n", env['x'].eval(), "self._instruction('POP_SCOPE')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('And')
        self._instruction('BACKTRACK', 26)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('BACKTRACK', 23)
        self._instruction('MATCH_ANY')
        self._instruction('COMMIT', 22)
        self._label(22)
        self._instruction('FAIL')
        self._label(23)
        self._instruction('ACTION', lambda env: env['x'].eval())
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 27)
        self._label(26)
        self._instruction('BACKTRACK', 24)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'And')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval(), env['y'].eval()]))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 25)
        self._label(24)
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create([]))
        self._instruction('POP_SCOPE')
        self._label(25)
        self._label(27)
        self._instruction('RETURN')
        self._label('Bind')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create([env['y'].eval(), "self._instruction('BIND', ", repr(env['x'].eval()), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('Star')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('LABEL')
        self._instruction('BIND', 'a')
        self._instruction('LABEL')
        self._instruction('BIND', 'b')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('LIST_START')\n", 'self._label(', env['a'].eval(), ')\n', "self._instruction('BACKTRACK', ", env['b'].eval(), ')\n', env['x'].eval(), "self._instruction('LIST_APPEND')\n", "self._instruction('COMMIT', ", env['a'].eval(), ')\n', 'self._label(', env['b'].eval(), ')\n', "self._instruction('LIST_END')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('Not')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('LABEL')
        self._instruction('BIND', 'a')
        self._instruction('LABEL')
        self._instruction('BIND', 'b')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('BACKTRACK', ", env['b'].eval(), ')\n', env['x'].eval(), "self._instruction('COMMIT', ", env['a'].eval(), ')\n', 'self._label(', env['a'].eval(), ')\n', "self._instruction('FAIL')\n", 'self._label(', env['b'].eval(), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('SemanticAction')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('ACTION', lambda env: ", env['x'].eval(), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('Label')
        self._instruction('PUSH_SCOPE')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('LABEL')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchRule')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('CALL', ", repr(env['x'].eval()), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchRange')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('MATCH_RANGE', ", repr(env['x'].eval()), ', ', repr(env['y'].eval()), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchString')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('MATCH_STRING', ", repr(env['x'].eval()), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchCharseq')
        self._instruction('PUSH_SCOPE')
        self._instruction('MATCH_ANY')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('MATCH_CHARSEQ', ", repr(env['x'].eval()), ')\n']))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('MatchList')
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(["self._instruction('PUSH_INPUT')\n", env['x'].eval(), "self._instruction('POP_INPUT')\n"]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('ast')
        self._instruction('PUSH_SCOPE')
        self._instruction('PUSH_INPUT')
        self._instruction('MATCH_CALL_RULE')
        self._instruction('BIND', 'x')
        self._instruction('POP_INPUT')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        self._instruction('POP_SCOPE')
        self._instruction('RETURN')
        self._label('astItems')
        self._instruction('BACKTRACK', 46)
        self._instruction('PUSH_SCOPE')
        self._instruction('BACKTRACK', 35)
        self._instruction('MATCH_ANY')
        self._instruction('COMMIT', 34)
        self._label(34)
        self._instruction('FAIL')
        self._label(35)
        self._instruction('ACTION', lambda env: _Builder.create(['']))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 47)
        self._label(46)
        self._instruction('BACKTRACK', 44)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('BACKTRACK', 43)
        self._instruction('MATCH_ANY')
        self._instruction('COMMIT', 42)
        self._label(42)
        self._instruction('FAIL')
        self._label(43)
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 45)
        self._label(44)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'astItems')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval(), ', ', env['y'].eval()]))
        self._instruction('POP_SCOPE')
        self._label(45)
        self._label(47)
        self._instruction('RETURN')
        self._label('astList')
        self._instruction('BACKTRACK', 66)
        self._instruction('PUSH_SCOPE')
        self._instruction('BACKTRACK', 55)
        self._instruction('MATCH_ANY')
        self._instruction('COMMIT', 54)
        self._label(54)
        self._instruction('FAIL')
        self._label(55)
        self._instruction('ACTION', lambda env: _Builder.create(['[]']))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 67)
        self._label(66)
        self._instruction('BACKTRACK', 64)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'astListItem')
        self._instruction('BIND', 'x')
        self._instruction('BACKTRACK', 63)
        self._instruction('MATCH_ANY')
        self._instruction('COMMIT', 62)
        self._label(62)
        self._instruction('FAIL')
        self._label(63)
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 65)
        self._label(64)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'astListItem')
        self._instruction('BIND', 'x')
        self._instruction('CALL', 'astList')
        self._instruction('BIND', 'y')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval(), '+', env['y'].eval()]))
        self._instruction('POP_SCOPE')
        self._label(65)
        self._label(67)
        self._instruction('RETURN')
        self._label('astListItem')
        self._instruction('BACKTRACK', 68)
        self._instruction('PUSH_SCOPE')
        self._instruction('PUSH_INPUT')
        self._instruction('MATCH_STRING', 'ListItemSplice')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('POP_INPUT')
        self._instruction('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        self._instruction('POP_SCOPE')
        self._instruction('COMMIT', 69)
        self._label(68)
        self._instruction('PUSH_SCOPE')
        self._instruction('CALL', 'ast')
        self._instruction('BIND', 'x')
        self._instruction('ACTION', lambda env: _Builder.create(['[', env['x'].eval(), ']']))
        self._instruction('POP_SCOPE')
        self._label(69)
        self._instruction('RETURN')

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
