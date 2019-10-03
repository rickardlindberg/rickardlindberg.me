import sys

SUPPORT = 'try:\n    from cStringIO import StringIO\nexcept:\n    from StringIO import StringIO\n\ndef rlmeta_vm(instructions, labels, start_rule, stream):\n    label_counter = 0\n    last_action = _ConstantSemanticAction(None)\n    pc = labels[start_rule]\n    call_backtrack_stack = []\n    stream, pos, stream_pos_stack = (stream, 0, [])\n    scope, scope_stack = (None, [])\n    fail_message = None\n    latest_fail_message, latest_fail_pos = (None, tuple())\n    memo = {}\n    while True:\n        name, arg1, arg2 = instructions[pc]\n        if name == "PUSH_SCOPE":\n            scope_stack.append(scope)\n            scope = {}\n            pc += 1\n            continue\n        elif name == "BACKTRACK":\n            call_backtrack_stack.append((labels[arg1], pos, len(stream_pos_stack), len(scope_stack)))\n            pc += 1\n            continue\n        elif name == "CALL":\n            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))\n            if key in memo:\n                if memo[key][0] is None:\n                    fail_message = memo[key][1]\n                else:\n                    last_action, stream_pos_stack = memo[key]\n                    stream_pos_stack = stream_pos_stack[:]\n                    stream, pos = stream_pos_stack.pop()\n                    pc += 1\n                    continue\n            else:\n                call_backtrack_stack.append((pc+1, key))\n                pc = labels[arg1]\n                continue\n        elif name == "MATCH_CHARSEQ":\n            for char in arg1:\n                if pos >= len(stream) or stream[pos] != char:\n                    fail_message = ("expected {!r}", char)\n                    break\n                pos += 1\n            else:\n                last_action = _ConstantSemanticAction(arg1)\n                pc += 1\n                continue\n        elif name == "COMMIT":\n            call_backtrack_stack.pop()\n            pc = labels[arg1]\n            continue\n        elif name == "POP_SCOPE":\n            scope = scope_stack.pop()\n            pc += 1\n            continue\n        elif name == "RETURN":\n            if len(call_backtrack_stack) == 0:\n                return last_action.eval()\n            pc, key = call_backtrack_stack.pop()\n            memo[key] = (last_action, stream_pos_stack+[(stream, pos)])\n            continue\n        elif name == "LIST_APPEND":\n            scope.append(last_action)\n            pc += 1\n            continue\n        elif name == "BIND":\n            scope[arg1] = last_action\n            pc += 1\n            continue\n        elif name == "ACTION":\n            last_action = _UserSemanticAction(arg1, scope)\n            pc += 1\n            continue\n        elif name == "MATCH_RANGE":\n            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):\n                fail_message = ("expected range {!r}-{!r}", arg1, arg2)\n            else:\n                last_action = _ConstantSemanticAction(stream[pos])\n                pos += 1\n                pc += 1\n                continue\n        elif name == "LIST_START":\n            scope_stack.append(scope)\n            scope = []\n            pc += 1\n            continue\n        elif name == "LIST_END":\n            last_action = _UserSemanticAction(lambda xs: [x.eval() for x in xs], scope)\n            scope = scope_stack.pop()\n            pc += 1\n            continue\n        elif name == "MATCH_ANY":\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n            else:\n                last_action = _ConstantSemanticAction(stream[pos])\n                pos += 1\n                pc += 1\n                continue\n        elif name == "PUSH_STREAM":\n            if pos >= len(stream) or not isinstance(stream[pos], list):\n                fail_message = ("expected list",)\n            else:\n                stream_pos_stack.append((stream, pos))\n                stream = stream[pos]\n                pos = 0\n                pc += 1\n                continue\n        elif name == "POP_STREAM":\n            if pos < len(stream):\n                fail_message = ("expected end of list",)\n            else:\n                stream, pos = stream_pos_stack.pop()\n                pos += 1\n                pc += 1\n                continue\n        elif name == "MATCH_CALL_RULE":\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n            else:\n                fn_name = str(stream[pos])\n                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))\n                if key in memo:\n                    if memo[key][0] is None:\n                        fail_message = memo[key][1]\n                    else:\n                        last_action, stream_pos_stack = memo[key]\n                        stream_pos_stack = stream_pos_stack[:]\n                        stream, pos = stream_pos_stack.pop()\n                        pc += 1\n                        continue\n                else:\n                    call_backtrack_stack.append((pc+1, key))\n                    pc = labels[fn_name]\n                    pos += 1\n                    continue\n        elif name == "FAIL":\n            fail_message = (arg1,)\n        elif name == "LABEL":\n            last_action = _ConstantSemanticAction(label_counter)\n            label_counter += 1\n            pc += 1\n            continue\n        elif name == "MATCH_STRING":\n            if pos >= len(stream) or stream[pos] != arg1:\n                fail_message = ("expected {!r}", arg1)\n            else:\n                last_action = _ConstantSemanticAction(arg1)\n                pos += 1\n                pc += 1\n                continue\n        else:\n            raise Exception("unknown instruction {}".format(name))\n        fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n        if fail_pos >= latest_fail_pos:\n            latest_fail_message = fail_message\n            latest_fail_pos = fail_pos\n        call_backtrack_entry = tuple()\n        while call_backtrack_stack:\n            call_backtrack_entry = call_backtrack_stack.pop()\n            if len(call_backtrack_entry) == 4:\n                break\n            else:\n                _, key = call_backtrack_entry\n                memo[key] = (None, fail_message)\n        if len(call_backtrack_entry) != 4:\n            fail_pos = list(latest_fail_pos)\n            fail_stream = stream_pos_stack[0][0] if stream_pos_stack else stream\n            while len(fail_pos) > 1:\n                fail_stream = fail_stream[fail_pos.pop(0)]\n            raise _MatchError(latest_fail_message, fail_pos[0], fail_stream)\n        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n        if len(stream_pos_stack) > stream_stack_len:\n            stream = stream_pos_stack[stream_stack_len][0]\n        stream_pos_stack = stream_pos_stack[:stream_stack_len]\n        if len(scope_stack) > scope_stack_len:\n            scope = scope_stack[scope_stack_len]\n        scope_stack = scope_stack[:scope_stack_len]\n\nclass _Grammar(object):\n\n    def run(self, rule_name, input_object):\n        if isinstance(input_object, basestring):\n            stream = input_object\n        else:\n            stream = [input_object]\n        result = rlmeta_vm(self._instructions, self._labels, rule_name, stream)\n        if isinstance(result, _Builder):\n            return result.build_string()\n        else:\n            return result\n\nclass _Builder(object):\n\n    def build_string(self):\n        output = _Output()\n        self.write(output)\n        return output.value\n\n    @classmethod\n    def create(self, item):\n        if isinstance(item, _Builder):\n            return item\n        elif isinstance(item, list):\n            return _ListBuilder([_Builder.create(x) for x in item])\n        else:\n            return _AtomBuilder(item)\n\nclass _Output(object):\n\n    def __init__(self):\n        self.buffer = StringIO()\n        self.indentation = 0\n        self.on_newline = True\n\n    @property\n    def value(self):\n        return self.buffer.getvalue()\n\n    def write(self, value):\n        for ch in value:\n            is_linebreak = ch == "\\n"\n            if self.indentation and self.on_newline and not is_linebreak:\n                self.buffer.write("    "*self.indentation)\n            self.buffer.write(ch)\n            self.on_newline = is_linebreak\n\nclass _ListBuilder(_Builder):\n\n    def __init__(self, builders):\n        self.builders = builders\n\n    def write(self, output):\n        for builder in self.builders:\n            builder.write(output)\n\nclass _AtomBuilder(_Builder):\n\n    def __init__(self, atom):\n        self.atom = atom\n\n    def write(self, output):\n        output.write(str(self.atom))\n\nclass _IndentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation += 1\n\nclass _DedentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation -= 1\n\nclass _ConstantSemanticAction(object):\n\n    def __init__(self, value):\n        self.value = value\n\n    def eval(self):\n        return self.value\n\nclass _UserSemanticAction(object):\n\n    def __init__(self, fn, scope):\n        self.fn = fn\n        self.scope = scope\n\n    def eval(self):\n        return self.fn(self.scope)\n\nclass _MatchError(Exception):\n\n    def __init__(self, message, pos, stream):\n        Exception.__init__(self)\n        self.message = message\n        self.pos = pos\n        self.stream = stream\n\n    def describe(self):\n        message = ""\n        if isinstance(self.stream, basestring):\n            before = self.stream[:self.pos].splitlines()\n            after = self.stream[self.pos:].splitlines()\n            for context_before in before[-4:-1]:\n                message += self._context(context_before)\n            message += self._context(before[-1], after[0])\n            message += self._arrow(len(before[-1]))\n            for context_after in after[1:4]:\n                message += self._context(context_after)\n        else:\n            message += self._context("[")\n            for context_before in self.stream[:self.pos]:\n                message += self._context("  ", repr(context_before), ",")\n            message += self._context("  ", repr(self.stream[self.pos]), ",")\n            message += self._arrow(2)\n            for context_after in self.stream[self.pos+1:]:\n                message += self._context("  ", repr(context_after), ",")\n            message += self._context("]")\n        message += "Error: "\n        message += self.message[0].format(*self.message[1:])\n        message += "\\n"\n        return message\n\n    def _context(self, *args):\n        return "> {}\\n".format("".join(args))\n\n    def _arrow(self, lenght):\n        return "--{}^\\n".format("-"*lenght)\n\ndef _makeList(items):\n    def _addItem(depth, item):\n        if depth == 0:\n            result.append(item)\n        else:\n            for subitem in item:\n                _addItem(depth-1, subitem)\n    result = []\n    for depth, item in items:\n        _addItem(depth, item)\n    return result\n'

try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

def rlmeta_vm(instructions, labels, start_rule, stream):
    label_counter = 0
    last_action = _ConstantSemanticAction(None)
    pc = labels[start_rule]
    call_backtrack_stack = []
    stream, pos, stream_pos_stack = (stream, 0, [])
    scope, scope_stack = (None, [])
    fail_message = None
    latest_fail_message, latest_fail_pos = (None, tuple())
    memo = {}
    while True:
        name, arg1, arg2 = instructions[pc]
        if name == "PUSH_SCOPE":
            scope_stack.append(scope)
            scope = {}
            pc += 1
            continue
        elif name == "BACKTRACK":
            call_backtrack_stack.append((labels[arg1], pos, len(stream_pos_stack), len(scope_stack)))
            pc += 1
            continue
        elif name == "CALL":
            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))
            if key in memo:
                if memo[key][0] is None:
                    fail_message = memo[key][1]
                else:
                    last_action, stream_pos_stack = memo[key]
                    stream_pos_stack = stream_pos_stack[:]
                    stream, pos = stream_pos_stack.pop()
                    pc += 1
                    continue
            else:
                call_backtrack_stack.append((pc+1, key))
                pc = labels[arg1]
                continue
        elif name == "MATCH_CHARSEQ":
            for char in arg1:
                if pos >= len(stream) or stream[pos] != char:
                    fail_message = ("expected {!r}", char)
                    break
                pos += 1
            else:
                last_action = _ConstantSemanticAction(arg1)
                pc += 1
                continue
        elif name == "COMMIT":
            call_backtrack_stack.pop()
            pc = labels[arg1]
            continue
        elif name == "POP_SCOPE":
            scope = scope_stack.pop()
            pc += 1
            continue
        elif name == "RETURN":
            if len(call_backtrack_stack) == 0:
                return last_action.eval()
            pc, key = call_backtrack_stack.pop()
            memo[key] = (last_action, stream_pos_stack+[(stream, pos)])
            continue
        elif name == "LIST_APPEND":
            scope.append(last_action)
            pc += 1
            continue
        elif name == "BIND":
            scope[arg1] = last_action
            pc += 1
            continue
        elif name == "ACTION":
            last_action = _UserSemanticAction(arg1, scope)
            pc += 1
            continue
        elif name == "MATCH_RANGE":
            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
                fail_message = ("expected range {!r}-{!r}", arg1, arg2)
            else:
                last_action = _ConstantSemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "LIST_START":
            scope_stack.append(scope)
            scope = []
            pc += 1
            continue
        elif name == "LIST_END":
            last_action = _UserSemanticAction(lambda xs: [x.eval() for x in xs], scope)
            scope = scope_stack.pop()
            pc += 1
            continue
        elif name == "MATCH_ANY":
            if pos >= len(stream):
                fail_message = ("expected any",)
            else:
                last_action = _ConstantSemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "PUSH_STREAM":
            if pos >= len(stream) or not isinstance(stream[pos], list):
                fail_message = ("expected list",)
            else:
                stream_pos_stack.append((stream, pos))
                stream = stream[pos]
                pos = 0
                pc += 1
                continue
        elif name == "POP_STREAM":
            if pos < len(stream):
                fail_message = ("expected end of list",)
            else:
                stream, pos = stream_pos_stack.pop()
                pos += 1
                pc += 1
                continue
        elif name == "MATCH_CALL_RULE":
            if pos >= len(stream):
                fail_message = ("expected any",)
            else:
                fn_name = str(stream[pos])
                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
                if key in memo:
                    if memo[key][0] is None:
                        fail_message = memo[key][1]
                    else:
                        last_action, stream_pos_stack = memo[key]
                        stream_pos_stack = stream_pos_stack[:]
                        stream, pos = stream_pos_stack.pop()
                        pc += 1
                        continue
                else:
                    call_backtrack_stack.append((pc+1, key))
                    pc = labels[fn_name]
                    pos += 1
                    continue
        elif name == "FAIL":
            fail_message = (arg1,)
        elif name == "LABEL":
            last_action = _ConstantSemanticAction(label_counter)
            label_counter += 1
            pc += 1
            continue
        elif name == "MATCH_STRING":
            if pos >= len(stream) or stream[pos] != arg1:
                fail_message = ("expected {!r}", arg1)
            else:
                last_action = _ConstantSemanticAction(arg1)
                pos += 1
                pc += 1
                continue
        else:
            raise Exception("unknown instruction {}".format(name))
        fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])
        if fail_pos >= latest_fail_pos:
            latest_fail_message = fail_message
            latest_fail_pos = fail_pos
        call_backtrack_entry = tuple()
        while call_backtrack_stack:
            call_backtrack_entry = call_backtrack_stack.pop()
            if len(call_backtrack_entry) == 4:
                break
            else:
                _, key = call_backtrack_entry
                memo[key] = (None, fail_message)
        if len(call_backtrack_entry) != 4:
            fail_pos = list(latest_fail_pos)
            fail_stream = stream_pos_stack[0][0] if stream_pos_stack else stream
            while len(fail_pos) > 1:
                fail_stream = fail_stream[fail_pos.pop(0)]
            raise _MatchError(latest_fail_message, fail_pos[0], fail_stream)
        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry
        if len(stream_pos_stack) > stream_stack_len:
            stream = stream_pos_stack[stream_stack_len][0]
        stream_pos_stack = stream_pos_stack[:stream_stack_len]
        if len(scope_stack) > scope_stack_len:
            scope = scope_stack[scope_stack_len]
        scope_stack = scope_stack[:scope_stack_len]

class _Grammar(object):

    def run(self, rule_name, input_object):
        if isinstance(input_object, basestring):
            stream = input_object
        else:
            stream = [input_object]
        result = rlmeta_vm(self._instructions, self._labels, rule_name, stream)
        if isinstance(result, _Builder):
            return result.build_string()
        else:
            return result

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

class _ConstantSemanticAction(object):

    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value

class _UserSemanticAction(object):

    def __init__(self, fn, scope):
        self.fn = fn
        self.scope = scope

    def eval(self):
        return self.fn(self.scope)

class _MatchError(Exception):

    def __init__(self, message, pos, stream):
        Exception.__init__(self)
        self.message = message
        self.pos = pos
        self.stream = stream

    def describe(self):
        message = ""
        if isinstance(self.stream, basestring):
            before = self.stream[:self.pos].splitlines()
            after = self.stream[self.pos:].splitlines()
            for context_before in before[-4:-1]:
                message += self._context(context_before)
            message += self._context(before[-1], after[0])
            message += self._arrow(len(before[-1]))
            for context_after in after[1:4]:
                message += self._context(context_after)
        else:
            message += self._context("[")
            for context_before in self.stream[:self.pos]:
                message += self._context("  ", repr(context_before), ",")
            message += self._context("  ", repr(self.stream[self.pos]), ",")
            message += self._arrow(2)
            for context_after in self.stream[self.pos+1:]:
                message += self._context("  ", repr(context_after), ",")
            message += self._context("]")
        message += "Error: "
        message += self.message[0].format(*self.message[1:])
        message += "\n"
        return message

    def _context(self, *args):
        return "> {}\n".format("".join(args))

    def _arrow(self, lenght):
        return "--{}^\n".format("-"*lenght)

def _makeList(items):
    def _addItem(depth, item):
        if depth == 0:
            result.append(item)
        else:
            for subitem in item:
                _addItem(depth-1, subitem)
    result = []
    for depth, item in items:
        _addItem(depth, item)
    return result

class Parser(_Grammar):

    def __init__(self):
        self._instructions = i = []
        self._labels = l = {}
        def I(name, x=None, y=None):
            i.append((name, x, y))
        def LABEL(name):
            l[name] = len(i)
        LABEL('grammar')
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '{')
        I('LIST_START')
        LABEL(0)
        I('BACKTRACK', 1)
        I('CALL', 'rule')
        I('LIST_APPEND')
        I('COMMIT', 0)
        LABEL(1)
        I('LIST_END')
        I('BIND', 'ys')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '}')
        I('ACTION', lambda scope: _makeList([(0, 'Grammar'), (0, scope['x'].eval()), (1, scope['ys'].eval())]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('rule')
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '=')
        I('CALL', 'choice')
        I('BIND', 'y')
        I('ACTION', lambda scope: _makeList([(0, 'Rule'), (0, scope['x'].eval()), (0, scope['y'].eval())]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('choice')
        I('PUSH_SCOPE')
        I('BACKTRACK', 2)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '|')
        I('POP_SCOPE')
        I('COMMIT', 3)
        LABEL(2)
        LABEL(3)
        I('CALL', 'sequence')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(4)
        I('BACKTRACK', 5)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '|')
        I('CALL', 'sequence')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 4)
        LABEL(5)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: _makeList([(0, 'Or'), (0, scope['x'].eval()), (1, scope['xs'].eval())]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('sequence')
        I('PUSH_SCOPE')
        I('CALL', 'expr')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(6)
        I('BACKTRACK', 7)
        I('CALL', 'expr')
        I('LIST_APPEND')
        I('COMMIT', 6)
        LABEL(7)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: _makeList([(0, 'Scope'), (0, _makeList([(0, 'And'), (0, scope['x'].eval()), (1, scope['xs'].eval())]))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('expr')
        I('BACKTRACK', 8)
        I('PUSH_SCOPE')
        I('CALL', 'expr1')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ':')
        I('CALL', 'name')
        I('BIND', 'y')
        I('ACTION', lambda scope: _makeList([(0, 'Bind'), (0, scope['y'].eval()), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 9)
        LABEL(8)
        I('PUSH_SCOPE')
        I('CALL', 'expr1')
        I('POP_SCOPE')
        LABEL(9)
        I('RETURN')
        LABEL('expr1')
        I('BACKTRACK', 16)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '*')
        I('ACTION', lambda scope: _makeList([(0, 'Star'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 17)
        LABEL(16)
        I('BACKTRACK', 14)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '?')
        I('ACTION', lambda scope: _makeList([(0, 'Or'), (0, scope['x'].eval()), (0, _makeList([(0, 'And')]))]))
        I('POP_SCOPE')
        I('COMMIT', 15)
        LABEL(14)
        I('BACKTRACK', 12)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '!')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'Not'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 13)
        LABEL(12)
        I('BACKTRACK', 10)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '#')
        I('ACTION', lambda scope: _makeList([(0, 'Label')]))
        I('POP_SCOPE')
        I('COMMIT', 11)
        LABEL(10)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('POP_SCOPE')
        LABEL(11)
        LABEL(13)
        LABEL(15)
        LABEL(17)
        I('RETURN')
        LABEL('expr2')
        I('BACKTRACK', 36)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '->')
        I('CALL', 'hostExpr')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'SemanticAction'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 37)
        LABEL(36)
        I('BACKTRACK', 34)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '%')
        I('ACTION', lambda scope: _makeList([(0, 'MatchCallRule')]))
        I('POP_SCOPE')
        I('COMMIT', 35)
        LABEL(34)
        I('BACKTRACK', 32)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('BACKTRACK', 19)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '=')
        I('POP_SCOPE')
        I('COMMIT', 18)
        LABEL(18)
        I('FAIL', 'no match expected')
        LABEL(19)
        I('ACTION', lambda scope: _makeList([(0, 'MatchRule'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 33)
        LABEL(32)
        I('BACKTRACK', 30)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'char')
        I('BIND', 'x')
        I('MATCH_CHARSEQ', '-')
        I('CALL', 'char')
        I('BIND', 'y')
        I('ACTION', lambda scope: _makeList([(0, 'MatchRange'), (0, scope['x'].eval()), (0, scope['y'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 31)
        LABEL(30)
        I('BACKTRACK', 28)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'string')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'MatchString'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 29)
        LABEL(28)
        I('BACKTRACK', 26)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'charseq')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'MatchCharseq'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 27)
        LABEL(26)
        I('BACKTRACK', 24)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '.')
        I('ACTION', lambda scope: _makeList([(0, 'MatchAny')]))
        I('POP_SCOPE')
        I('COMMIT', 25)
        LABEL(24)
        I('BACKTRACK', 22)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '(')
        I('CALL', 'choice')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ')')
        I('ACTION', lambda scope: scope['x'].eval())
        I('POP_SCOPE')
        I('COMMIT', 23)
        LABEL(22)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '[')
        I('LIST_START')
        LABEL(20)
        I('BACKTRACK', 21)
        I('CALL', 'expr')
        I('LIST_APPEND')
        I('COMMIT', 20)
        LABEL(21)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ']')
        I('ACTION', lambda scope: _makeList([(0, 'MatchList'), (0, _makeList([(0, 'And'), (1, scope['xs'].eval())]))]))
        I('POP_SCOPE')
        LABEL(23)
        LABEL(25)
        LABEL(27)
        LABEL(29)
        LABEL(31)
        LABEL(33)
        LABEL(35)
        LABEL(37)
        I('RETURN')
        LABEL('hostExpr')
        I('BACKTRACK', 52)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'string')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'String'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 53)
        LABEL(52)
        I('BACKTRACK', 50)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'number')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'Integer'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 51)
        LABEL(50)
        I('BACKTRACK', 48)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '[')
        I('LIST_START')
        LABEL(38)
        I('BACKTRACK', 39)
        I('CALL', 'hostExprListItem')
        I('LIST_APPEND')
        I('COMMIT', 38)
        LABEL(39)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ']')
        I('ACTION', lambda scope: _makeList([(0, 'List'), (1, scope['xs'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 49)
        LABEL(48)
        I('BACKTRACK', 46)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '{')
        I('LIST_START')
        LABEL(40)
        I('BACKTRACK', 41)
        I('CALL', 'buildExpr')
        I('LIST_APPEND')
        I('COMMIT', 40)
        LABEL(41)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '}')
        I('ACTION', lambda scope: _makeList([(0, 'Builder'), (1, scope['xs'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 47)
        LABEL(46)
        I('BACKTRACK', 44)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '(')
        I('LIST_START')
        LABEL(42)
        I('BACKTRACK', 43)
        I('CALL', 'hostExpr')
        I('LIST_APPEND')
        I('COMMIT', 42)
        LABEL(43)
        I('LIST_END')
        I('BIND', 'ys')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ')')
        I('ACTION', lambda scope: _makeList([(0, 'FnCall'), (0, scope['x'].eval()), (1, scope['ys'].eval())]))
        I('POP_SCOPE')
        I('COMMIT', 45)
        LABEL(44)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('ACTION', lambda scope: _makeList([(0, 'VarLookup'), (0, scope['x'].eval())]))
        I('POP_SCOPE')
        LABEL(45)
        LABEL(47)
        LABEL(49)
        LABEL(51)
        LABEL(53)
        I('RETURN')
        LABEL('hostExprListItem')
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('LIST_START')
        LABEL(54)
        I('BACKTRACK', 55)
        I('MATCH_CHARSEQ', '~')
        I('LIST_APPEND')
        I('COMMIT', 54)
        LABEL(55)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'hostExpr')
        I('BIND', 'y')
        I('ACTION', lambda scope: _makeList([(0, 'ListItemSplice'), (0, len(scope['xs'].eval())), (0, scope['y'].eval())]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('buildExpr')
        I('BACKTRACK', 58)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '>')
        I('ACTION', lambda scope: _makeList([(0, 'IndentBuilder')]))
        I('POP_SCOPE')
        I('COMMIT', 59)
        LABEL(58)
        I('BACKTRACK', 56)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '<')
        I('ACTION', lambda scope: _makeList([(0, 'DedentBuilder')]))
        I('POP_SCOPE')
        I('COMMIT', 57)
        LABEL(56)
        I('PUSH_SCOPE')
        I('CALL', 'hostExpr')
        I('POP_SCOPE')
        LABEL(57)
        LABEL(59)
        I('RETURN')
        LABEL('number')
        I('BACKTRACK', 64)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '0x')
        I('CALL', 'hex')
        I('BIND', 'y')
        I('LIST_START')
        LABEL(60)
        I('BACKTRACK', 61)
        I('CALL', 'hex')
        I('LIST_APPEND')
        I('COMMIT', 60)
        LABEL(61)
        I('LIST_END')
        I('BIND', 'ys')
        I('ACTION', lambda scope: int(join(_makeList([(0, scope['y'].eval()), (1, scope['ys'].eval())])), 16))
        I('POP_SCOPE')
        I('COMMIT', 65)
        LABEL(64)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'digit')
        I('BIND', 'y')
        I('LIST_START')
        LABEL(62)
        I('BACKTRACK', 63)
        I('CALL', 'digit')
        I('LIST_APPEND')
        I('COMMIT', 62)
        LABEL(63)
        I('LIST_END')
        I('BIND', 'ys')
        I('ACTION', lambda scope: int(join(_makeList([(0, scope['y'].eval()), (1, scope['ys'].eval())]))))
        I('POP_SCOPE')
        LABEL(65)
        I('RETURN')
        LABEL('string')
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '"')
        I('LIST_START')
        LABEL(68)
        I('BACKTRACK', 69)
        I('PUSH_SCOPE')
        I('BACKTRACK', 67)
        I('MATCH_CHARSEQ', '"')
        I('COMMIT', 66)
        LABEL(66)
        I('FAIL', 'no match expected')
        LABEL(67)
        I('CALL', 'innerChar')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 68)
        LABEL(69)
        I('LIST_END')
        I('BIND', 'xs')
        I('MATCH_CHARSEQ', '"')
        I('ACTION', lambda scope: join(scope['xs'].eval()))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('charseq')
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', "'")
        I('LIST_START')
        LABEL(72)
        I('BACKTRACK', 73)
        I('PUSH_SCOPE')
        I('BACKTRACK', 71)
        I('MATCH_CHARSEQ', "'")
        I('COMMIT', 70)
        LABEL(70)
        I('FAIL', 'no match expected')
        LABEL(71)
        I('CALL', 'innerChar')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 72)
        LABEL(73)
        I('LIST_END')
        I('BIND', 'xs')
        I('MATCH_CHARSEQ', "'")
        I('ACTION', lambda scope: join(scope['xs'].eval()))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('char')
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', "'")
        I('BACKTRACK', 75)
        I('MATCH_CHARSEQ', "'")
        I('COMMIT', 74)
        LABEL(74)
        I('FAIL', 'no match expected')
        LABEL(75)
        I('CALL', 'innerChar')
        I('BIND', 'x')
        I('MATCH_CHARSEQ', "'")
        I('ACTION', lambda scope: scope['x'].eval())
        I('POP_SCOPE')
        I('RETURN')
        LABEL('innerChar')
        I('BACKTRACK', 76)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '\\')
        I('CALL', 'escape')
        I('POP_SCOPE')
        I('COMMIT', 77)
        LABEL(76)
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('POP_SCOPE')
        LABEL(77)
        I('RETURN')
        LABEL('escape')
        I('BACKTRACK', 82)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '\\')
        I('ACTION', lambda scope: '\\')
        I('POP_SCOPE')
        I('COMMIT', 83)
        LABEL(82)
        I('BACKTRACK', 80)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', "'")
        I('ACTION', lambda scope: "'")
        I('POP_SCOPE')
        I('COMMIT', 81)
        LABEL(80)
        I('BACKTRACK', 78)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '"')
        I('ACTION', lambda scope: '"')
        I('POP_SCOPE')
        I('COMMIT', 79)
        LABEL(78)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', 'n')
        I('ACTION', lambda scope: '\n')
        I('POP_SCOPE')
        LABEL(79)
        LABEL(81)
        LABEL(83)
        I('RETURN')
        LABEL('name')
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'nameStart')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(84)
        I('BACKTRACK', 85)
        I('CALL', 'nameChar')
        I('LIST_APPEND')
        I('COMMIT', 84)
        LABEL(85)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: join(_makeList([(0, scope['x'].eval()), (1, scope['xs'].eval())])))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('hex')
        I('BACKTRACK', 88)
        I('PUSH_SCOPE')
        I('CALL', 'digit')
        I('POP_SCOPE')
        I('COMMIT', 89)
        LABEL(88)
        I('BACKTRACK', 86)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'f')
        I('POP_SCOPE')
        I('COMMIT', 87)
        LABEL(86)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'F')
        I('POP_SCOPE')
        LABEL(87)
        LABEL(89)
        I('RETURN')
        LABEL('nameStart')
        I('BACKTRACK', 90)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'z')
        I('POP_SCOPE')
        I('COMMIT', 91)
        LABEL(90)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'Z')
        I('POP_SCOPE')
        LABEL(91)
        I('RETURN')
        LABEL('nameChar')
        I('BACKTRACK', 94)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'z')
        I('POP_SCOPE')
        I('COMMIT', 95)
        LABEL(94)
        I('BACKTRACK', 92)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'Z')
        I('POP_SCOPE')
        I('COMMIT', 93)
        LABEL(92)
        I('PUSH_SCOPE')
        I('CALL', 'digit')
        I('POP_SCOPE')
        LABEL(93)
        LABEL(95)
        I('RETURN')
        LABEL('digit')
        I('PUSH_SCOPE')
        I('MATCH_RANGE', '0', '9')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('space')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(98)
        I('BACKTRACK', 99)
        I('BACKTRACK', 96)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', ' ')
        I('POP_SCOPE')
        I('COMMIT', 97)
        LABEL(96)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '\n')
        I('POP_SCOPE')
        LABEL(97)
        I('LIST_APPEND')
        I('COMMIT', 98)
        LABEL(99)
        I('LIST_END')
        I('POP_SCOPE')
        I('RETURN')

class CodeGenerator(_Grammar):

    def __init__(self):
        self._instructions = i = []
        self._labels = l = {}
        def I(name, x=None, y=None):
            i.append((name, x, y))
        def LABEL(name):
            l[name] = len(i)
        LABEL('ast')
        I('PUSH_SCOPE')
        I('PUSH_STREAM')
        I('MATCH_CALL_RULE')
        I('BIND', 'x')
        I('POP_STREAM')
        I('ACTION', lambda scope: scope['x'].eval())
        I('POP_SCOPE')
        I('RETURN')
        LABEL('py')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('ACTION', lambda scope: repr(scope['x'].eval()))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Grammar')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(0)
        I('BACKTRACK', 1)
        I('CALL', 'ast')
        I('LIST_APPEND')
        I('COMMIT', 0)
        LABEL(1)
        I('LIST_END')
        I('BIND', 'ys')
        I('ACTION', lambda scope: _Builder.create(['class ', scope['x'].eval(), '(_Grammar):\n\n', _IndentBuilder(), 'def __init__(self):\n', _IndentBuilder(), 'self._instructions = i = []\n', 'self._labels = l = {}\n', 'def I(name, x=None, y=None):\n', _IndentBuilder(), 'i.append((name, x, y))\n', _DedentBuilder(), 'def LABEL(name):\n', _IndentBuilder(), 'l[name] = len(i)\n', _DedentBuilder(), scope['ys'].eval(), _DedentBuilder(), _DedentBuilder()]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Rule')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda scope: _Builder.create(['LABEL(', scope['x'].eval(), ')\n', scope['y'].eval(), "I('RETURN')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Or')
        I('BACKTRACK', 2)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('CALL', 'Or')
        I('BIND', 'y')
        I('LABEL')
        I('BIND', 'a')
        I('LABEL')
        I('BIND', 'b')
        I('ACTION', lambda scope: _Builder.create(["I('BACKTRACK', ", scope['a'].eval(), ')\n', scope['x'].eval(), "I('COMMIT', ", scope['b'].eval(), ')\n', 'LABEL(', scope['a'].eval(), ')\n', scope['y'].eval(), 'LABEL(', scope['b'].eval(), ')\n']))
        I('POP_SCOPE')
        I('COMMIT', 3)
        LABEL(2)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('POP_SCOPE')
        LABEL(3)
        I('RETURN')
        LABEL('Scope')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(["I('PUSH_SCOPE')\n", scope['x'].eval(), "I('POP_SCOPE')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('And')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(4)
        I('BACKTRACK', 5)
        I('CALL', 'ast')
        I('LIST_APPEND')
        I('COMMIT', 4)
        LABEL(5)
        I('LIST_END')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Bind')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda scope: _Builder.create([scope['y'].eval(), "I('BIND', ", scope['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Star')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('LABEL')
        I('BIND', 'a')
        I('LABEL')
        I('BIND', 'b')
        I('ACTION', lambda scope: _Builder.create(["I('LIST_START')\n", 'LABEL(', scope['a'].eval(), ')\n', "I('BACKTRACK', ", scope['b'].eval(), ')\n', scope['x'].eval(), "I('LIST_APPEND')\n", "I('COMMIT', ", scope['a'].eval(), ')\n', 'LABEL(', scope['b'].eval(), ')\n', "I('LIST_END')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Not')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('LABEL')
        I('BIND', 'a')
        I('LABEL')
        I('BIND', 'b')
        I('ACTION', lambda scope: _Builder.create(["I('BACKTRACK', ", scope['b'].eval(), ')\n', scope['x'].eval(), "I('COMMIT', ", scope['a'].eval(), ')\n', 'LABEL(', scope['a'].eval(), ')\n', "I('FAIL', 'no match expected')\n", 'LABEL(', scope['b'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchCallRule')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: _Builder.create(["I('MATCH_CALL_RULE')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Label')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: _Builder.create(["I('LABEL')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('SemanticAction')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(["I('ACTION', lambda scope: ", scope['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('String')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Integer')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('List')
        I('PUSH_SCOPE')
        I('CALL', 'astItems')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(['_makeList([', scope['x'].eval(), '])']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('ListItemSplice')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda scope: _Builder.create(['(', scope['x'].eval(), ', ', scope['y'].eval(), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Builder')
        I('PUSH_SCOPE')
        I('CALL', 'astItems')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(['_Builder.create([', scope['x'].eval(), '])']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('IndentBuilder')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: _Builder.create(['_IndentBuilder()']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('DedentBuilder')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: _Builder.create(['_DedentBuilder()']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('FnCall')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('CALL', 'astItems')
        I('BIND', 'y')
        I('ACTION', lambda scope: _Builder.create([scope['x'].eval(), '(', scope['y'].eval(), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('VarLookup')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(['scope[', scope['x'].eval(), '].eval()']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('astItems')
        I('BACKTRACK', 8)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(6)
        I('BACKTRACK', 7)
        I('CALL', 'astItem')
        I('LIST_APPEND')
        I('COMMIT', 6)
        LABEL(7)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: _Builder.create([scope['x'].eval(), scope['xs'].eval()]))
        I('POP_SCOPE')
        I('COMMIT', 9)
        LABEL(8)
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: _Builder.create([]))
        I('POP_SCOPE')
        LABEL(9)
        I('RETURN')
        LABEL('astItem')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create([', ', scope['x'].eval()]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchRule')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(["I('CALL', ", scope['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchRange')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'py')
        I('BIND', 'y')
        I('ACTION', lambda scope: _Builder.create(["I('MATCH_RANGE', ", scope['x'].eval(), ', ', scope['y'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchString')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(["I('MATCH_STRING', ", scope['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchCharseq')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(["I('MATCH_CHARSEQ', ", scope['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchAny')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: _Builder.create(["I('MATCH_ANY')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchList')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: _Builder.create(["I('PUSH_STREAM')\n", scope['x'].eval(), "I('POP_STREAM')\n"]))
        I('POP_SCOPE')
        I('RETURN')

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
