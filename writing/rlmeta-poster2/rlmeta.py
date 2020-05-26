import sys
import pprint

SUPPORT = 'def vm(instructions, labels, start_rule, stream):\n    action = SemanticAction(None)\n    pc = labels[start_rule]\n    call_backtrack_stack = []\n    stream, pos, stream_pos_stack = (stream, 0, [])\n    scope, scope_stack = (None, [])\n    fail_message = None\n    latest_fail_message, latest_fail_pos = (None, tuple())\n    memo = {}\n    runtime = {\n        "label": Counter().next,\n    }\n    while True:\n        name, arg1, arg2 = instructions[pc]\n        if name == "PUSH_SCOPE":\n            scope_stack.append(scope)\n            scope = {}\n            pc += 1\n            continue\n        elif name == "BACKTRACK":\n            call_backtrack_stack.append((\n                labels[arg1], pos, len(stream_pos_stack), len(scope_stack)\n            ))\n            pc += 1\n            continue\n        elif name == "CALL":\n            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))\n            if key in memo:\n                if memo[key][0] is None:\n                    fail_message = memo[key][1]\n                else:\n                    action, stream_pos_stack = memo[key]\n                    stream_pos_stack = stream_pos_stack[:]\n                    stream, pos = stream_pos_stack.pop()\n                    pc += 1\n                    continue\n            else:\n                call_backtrack_stack.append((pc+1, key))\n                pc = labels[arg1]\n                continue\n        elif name == "POP_SCOPE":\n            scope = scope_stack.pop()\n            pc += 1\n            continue\n        elif name == "MATCH_OBJECT":\n            if pos >= len(stream) or stream[pos] != arg1:\n                fail_message = ("expected {!r}", arg1)\n            else:\n                action = SemanticAction(arg1)\n                pos += 1\n                pc += 1\n                continue\n        elif name == "COMMIT":\n            call_backtrack_stack.pop()\n            pc = labels[arg1]\n            continue\n        elif name == "RETURN":\n            if len(call_backtrack_stack) == 0:\n                return action.eval()\n            pc, key = call_backtrack_stack.pop()\n            memo[key] = (action, stream_pos_stack+[(stream, pos)])\n            continue\n        elif name == "LIST_APPEND":\n            scope.append(action)\n            pc += 1\n            continue\n        elif name == "BIND":\n            scope[arg1] = action\n            pc += 1\n            continue\n        elif name == "ACTION":\n            action = SemanticAction(Scope(scope, runtime), arg1)\n            pc += 1\n            continue\n        elif name == "MATCH_RANGE":\n            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):\n                fail_message = ("expected range {!r}-{!r}", arg1, arg2)\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n                pc += 1\n                continue\n        elif name == "LIST_START":\n            scope_stack.append(scope)\n            scope = []\n            pc += 1\n            continue\n        elif name == "LIST_END":\n            action = SemanticAction(scope, lambda xs: [x.eval() for x in xs])\n            scope = scope_stack.pop()\n            pc += 1\n            continue\n        elif name == "MATCH_ANY":\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n                pc += 1\n                continue\n        elif name == "PUSH_STREAM":\n            if pos >= len(stream) or not isinstance(stream[pos], list):\n                fail_message = ("expected list",)\n            else:\n                stream_pos_stack.append((stream, pos))\n                stream = stream[pos]\n                pos = 0\n                pc += 1\n                continue\n        elif name == "POP_STREAM":\n            if pos < len(stream):\n                fail_message = ("expected end of list",)\n            else:\n                stream, pos = stream_pos_stack.pop()\n                pos += 1\n                pc += 1\n                continue\n        elif name == "MATCH_CALL_RULE":\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n            else:\n                fn_name = str(stream[pos])\n                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))\n                if key in memo:\n                    if memo[key][0] is None:\n                        fail_message = memo[key][1]\n                    else:\n                        action, stream_pos_stack = memo[key]\n                        stream_pos_stack = stream_pos_stack[:]\n                        stream, pos = stream_pos_stack.pop()\n                        pc += 1\n                        continue\n                else:\n                    call_backtrack_stack.append((pc+1, key))\n                    pc = labels[fn_name]\n                    pos += 1\n                    continue\n        elif name == "FAIL":\n            fail_message = (arg1,)\n        else:\n            raise Exception("unknown instruction {}".format(name))\n        fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n        if fail_pos >= latest_fail_pos:\n            latest_fail_message = fail_message\n            latest_fail_pos = fail_pos\n        call_backtrack_entry = tuple()\n        while call_backtrack_stack:\n            call_backtrack_entry = call_backtrack_stack.pop()\n            if len(call_backtrack_entry) == 4:\n                break\n            else:\n                _, key = call_backtrack_entry\n                memo[key] = (None, fail_message)\n        if len(call_backtrack_entry) != 4:\n            raise MatchError(\n                latest_fail_message[0].format(*latest_fail_message[1:]),\n                latest_fail_pos,\n                stream_pos_stack[0][0] if stream_pos_stack else stream\n            )\n        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n        if len(stream_pos_stack) > stream_stack_len:\n            stream = stream_pos_stack[stream_stack_len][0]\n        stream_pos_stack = stream_pos_stack[:stream_stack_len]\n        if len(scope_stack) > scope_stack_len:\n            scope = scope_stack[scope_stack_len]\n        scope_stack = scope_stack[:scope_stack_len]\n\nclass Counter(object):\n\n    def __init__(self):\n        self.count = 0\n\n    def next(self):\n        result = self.count\n        self.count += 1\n        return result\n\nclass Scope(object):\n\n    def __init__(self, match, runtime):\n        self.match = match\n        self.runtime = runtime\n\n    def bind(self, name, value, continuation):\n        old = self.runtime.get(name, None)\n        self.runtime[name] = value\n        try:\n            return continuation()\n        finally:\n            self.runtime[name] = old\n\n    def lookup(self, name):\n        if name in self.match:\n            return self.match[name].eval()\n        else:\n            return self.runtime.get(name, None)\n\nclass SemanticAction(object):\n\n    def __init__(self, value, fn=lambda value: value):\n        self.value = value\n        self.fn = fn\n\n    def eval(self):\n        return self.fn(self.value)\n\nclass MatchError(Exception):\n\n    def __init__(self, message, pos, stream):\n        Exception.__init__(self)\n        self.message = message\n        self.pos = pos\n        self.stream = stream\n\nclass Grammar(object):\n\n    def run(self, rule_name, stream):\n        instructions = []\n        labels = {}\n        def I(name, arg1=None, arg2=None):\n            instructions.append((name, arg1, arg2))\n        def LABEL(name):\n            labels[name] = len(instructions)\n        self.assemble(I, LABEL)\n        return vm(instructions, labels, rule_name, stream)\n\ndef splice(depth, item):\n    if depth == 0:\n        return [item]\n    else:\n        return concat([splice(depth-1, subitem) for subitem in item])\n\ndef concat(lists):\n    return [x for xs in lists for x in xs]\n\ndef join(items, delimiter=""):\n    return delimiter.join(\n        join(item, delimiter) if isinstance(item, list) else str(item)\n        for item in items\n    )\n\ndef indent(text):\n    return join(join(["    ", line]) for line in text.splitlines(True))\n'

def vm(instructions, labels, start_rule, stream):
    action = SemanticAction(None)
    pc = labels[start_rule]
    call_backtrack_stack = []
    stream, pos, stream_pos_stack = (stream, 0, [])
    scope, scope_stack = (None, [])
    fail_message = None
    latest_fail_message, latest_fail_pos = (None, tuple())
    memo = {}
    runtime = {
        "label": Counter().next,
    }
    while True:
        name, arg1, arg2 = instructions[pc]
        if name == "PUSH_SCOPE":
            scope_stack.append(scope)
            scope = {}
            pc += 1
            continue
        elif name == "BACKTRACK":
            call_backtrack_stack.append((
                labels[arg1], pos, len(stream_pos_stack), len(scope_stack)
            ))
            pc += 1
            continue
        elif name == "CALL":
            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))
            if key in memo:
                if memo[key][0] is None:
                    fail_message = memo[key][1]
                else:
                    action, stream_pos_stack = memo[key]
                    stream_pos_stack = stream_pos_stack[:]
                    stream, pos = stream_pos_stack.pop()
                    pc += 1
                    continue
            else:
                call_backtrack_stack.append((pc+1, key))
                pc = labels[arg1]
                continue
        elif name == "POP_SCOPE":
            scope = scope_stack.pop()
            pc += 1
            continue
        elif name == "MATCH_OBJECT":
            if pos >= len(stream) or stream[pos] != arg1:
                fail_message = ("expected {!r}", arg1)
            else:
                action = SemanticAction(arg1)
                pos += 1
                pc += 1
                continue
        elif name == "COMMIT":
            call_backtrack_stack.pop()
            pc = labels[arg1]
            continue
        elif name == "RETURN":
            if len(call_backtrack_stack) == 0:
                return action.eval()
            pc, key = call_backtrack_stack.pop()
            memo[key] = (action, stream_pos_stack+[(stream, pos)])
            continue
        elif name == "LIST_APPEND":
            scope.append(action)
            pc += 1
            continue
        elif name == "BIND":
            scope[arg1] = action
            pc += 1
            continue
        elif name == "ACTION":
            action = SemanticAction(Scope(scope, runtime), arg1)
            pc += 1
            continue
        elif name == "MATCH_RANGE":
            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
                fail_message = ("expected range {!r}-{!r}", arg1, arg2)
            else:
                action = SemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "LIST_START":
            scope_stack.append(scope)
            scope = []
            pc += 1
            continue
        elif name == "LIST_END":
            action = SemanticAction(scope, lambda xs: [x.eval() for x in xs])
            scope = scope_stack.pop()
            pc += 1
            continue
        elif name == "MATCH_ANY":
            if pos >= len(stream):
                fail_message = ("expected any",)
            else:
                action = SemanticAction(stream[pos])
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
                        action, stream_pos_stack = memo[key]
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
            raise MatchError(
                latest_fail_message[0].format(*latest_fail_message[1:]),
                latest_fail_pos,
                stream_pos_stack[0][0] if stream_pos_stack else stream
            )
        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry
        if len(stream_pos_stack) > stream_stack_len:
            stream = stream_pos_stack[stream_stack_len][0]
        stream_pos_stack = stream_pos_stack[:stream_stack_len]
        if len(scope_stack) > scope_stack_len:
            scope = scope_stack[scope_stack_len]
        scope_stack = scope_stack[:scope_stack_len]

class Counter(object):

    def __init__(self):
        self.count = 0

    def next(self):
        result = self.count
        self.count += 1
        return result

class Scope(object):

    def __init__(self, match, runtime):
        self.match = match
        self.runtime = runtime

    def bind(self, name, value, continuation):
        old = self.runtime.get(name, None)
        self.runtime[name] = value
        try:
            return continuation()
        finally:
            self.runtime[name] = old

    def lookup(self, name):
        if name in self.match:
            return self.match[name].eval()
        else:
            return self.runtime.get(name, None)

class SemanticAction(object):

    def __init__(self, value, fn=lambda value: value):
        self.value = value
        self.fn = fn

    def eval(self):
        return self.fn(self.value)

class MatchError(Exception):

    def __init__(self, message, pos, stream):
        Exception.__init__(self)
        self.message = message
        self.pos = pos
        self.stream = stream

class Grammar(object):

    def run(self, rule_name, stream):
        instructions = []
        labels = {}
        def I(name, arg1=None, arg2=None):
            instructions.append((name, arg1, arg2))
        def LABEL(name):
            labels[name] = len(instructions)
        self.assemble(I, LABEL)
        return vm(instructions, labels, rule_name, stream)

def splice(depth, item):
    if depth == 0:
        return [item]
    else:
        return concat([splice(depth-1, subitem) for subitem in item])

def concat(lists):
    return [x for xs in lists for x in xs]

def join(items, delimiter=""):
    return delimiter.join(
        join(item, delimiter) if isinstance(item, list) else str(item)
        for item in items
    )

def indent(text):
    return join(join(["    ", line]) for line in text.splitlines(True))

class Parser(Grammar):

    def assemble(self, I, LABEL):
        LABEL('grammar')
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', '{')
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
        I('MATCH_OBJECT', '}')
        I('ACTION', lambda scope: concat([splice(0, 'Grammar'), splice(0, scope.lookup('x')), splice(1, scope.lookup('ys'))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('rule')
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', '=')
        I('CALL', 'choice')
        I('BIND', 'y')
        I('ACTION', lambda scope: concat([splice(0, 'Rule'), splice(0, scope.lookup('x')), splice(0, scope.lookup('y'))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('choice')
        I('PUSH_SCOPE')
        I('BACKTRACK', 2)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '|')
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
        I('MATCH_OBJECT', '|')
        I('CALL', 'sequence')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 4)
        LABEL(5)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: concat([splice(0, 'Or'), splice(0, scope.lookup('x')), splice(1, scope.lookup('xs'))]))
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
        I('ACTION', lambda scope: concat([splice(0, 'Scope'), splice(0, concat([splice(0, 'And'), splice(0, scope.lookup('x')), splice(1, scope.lookup('xs'))]))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('expr')
        I('BACKTRACK', 8)
        I('PUSH_SCOPE')
        I('CALL', 'expr1')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', ':')
        I('CALL', 'name')
        I('BIND', 'y')
        I('ACTION', lambda scope: concat([splice(0, 'Bind'), splice(0, scope.lookup('y')), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('COMMIT', 9)
        LABEL(8)
        I('PUSH_SCOPE')
        I('CALL', 'expr1')
        I('POP_SCOPE')
        LABEL(9)
        I('RETURN')
        LABEL('expr1')
        I('BACKTRACK', 10)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', '*')
        I('ACTION', lambda scope: concat([splice(0, 'Star'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('COMMIT', 11)
        LABEL(10)
        I('BACKTRACK', 12)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', '?')
        I('ACTION', lambda scope: concat([splice(0, 'Or'), splice(0, scope.lookup('x')), splice(0, concat([splice(0, 'And')]))]))
        I('POP_SCOPE')
        I('COMMIT', 13)
        LABEL(12)
        I('BACKTRACK', 14)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '!')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('ACTION', lambda scope: concat([splice(0, 'Not'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('COMMIT', 15)
        LABEL(14)
        I('BACKTRACK', 16)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '%')
        I('ACTION', lambda scope: concat([splice(0, 'MatchCallRule')]))
        I('POP_SCOPE')
        I('COMMIT', 17)
        LABEL(16)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('POP_SCOPE')
        LABEL(17)
        LABEL(15)
        LABEL(13)
        LABEL(11)
        I('RETURN')
        LABEL('expr2')
        I('BACKTRACK', 18)
        I('PUSH_SCOPE')
        I('CALL', 'action')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(20)
        I('BACKTRACK', 21)
        I('CALL', 'action')
        I('LIST_APPEND')
        I('COMMIT', 20)
        LABEL(21)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: concat([splice(0, 'Actions'), splice(1, scope.lookup('x')), splice(2, scope.lookup('xs'))]))
        I('POP_SCOPE')
        I('COMMIT', 19)
        LABEL(18)
        I('BACKTRACK', 22)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('BACKTRACK', 25)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '=')
        I('POP_SCOPE')
        I('COMMIT', 24)
        LABEL(24)
        I('FAIL', 'no match expected')
        LABEL(25)
        I('ACTION', lambda scope: concat([splice(0, 'MatchRule'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('COMMIT', 23)
        LABEL(22)
        I('BACKTRACK', 26)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'char')
        I('BIND', 'x')
        I('MATCH_OBJECT', '-')
        I('CALL', 'char')
        I('BIND', 'y')
        I('ACTION', lambda scope: concat([splice(0, 'MatchRange'), splice(0, scope.lookup('x')), splice(0, scope.lookup('y'))]))
        I('POP_SCOPE')
        I('COMMIT', 27)
        LABEL(26)
        I('BACKTRACK', 28)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'charseq')
        I('POP_SCOPE')
        I('COMMIT', 29)
        LABEL(28)
        I('BACKTRACK', 30)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '.')
        I('ACTION', lambda scope: concat([splice(0, 'MatchAny')]))
        I('POP_SCOPE')
        I('COMMIT', 31)
        LABEL(30)
        I('BACKTRACK', 32)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '(')
        I('CALL', 'choice')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', ')')
        I('ACTION', lambda scope: scope.lookup('x'))
        I('POP_SCOPE')
        I('COMMIT', 33)
        LABEL(32)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '[')
        I('LIST_START')
        LABEL(34)
        I('BACKTRACK', 35)
        I('CALL', 'expr')
        I('LIST_APPEND')
        I('COMMIT', 34)
        LABEL(35)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_OBJECT', ']')
        I('ACTION', lambda scope: concat([splice(0, 'MatchList'), splice(0, concat([splice(0, 'And'), splice(1, scope.lookup('xs'))]))]))
        I('POP_SCOPE')
        LABEL(33)
        LABEL(31)
        LABEL(29)
        LABEL(27)
        LABEL(23)
        LABEL(19)
        I('RETURN')
        LABEL('action')
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '-')
        I('MATCH_OBJECT', '>')
        I('CALL', 'hostExpr')
        I('BIND', 'x')
        I('BACKTRACK', 36)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', ':')
        I('CALL', 'name')
        I('POP_SCOPE')
        I('COMMIT', 37)
        LABEL(36)
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: '')
        I('POP_SCOPE')
        LABEL(37)
        I('BIND', 'y')
        I('ACTION', lambda scope: concat([splice(0, scope.lookup('y')), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('hostExpr')
        I('BACKTRACK', 38)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'string')
        I('BIND', 'x')
        I('ACTION', lambda scope: concat([splice(0, 'String'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('COMMIT', 39)
        LABEL(38)
        I('BACKTRACK', 40)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '[')
        I('LIST_START')
        LABEL(42)
        I('BACKTRACK', 43)
        I('CALL', 'hostExprListItem')
        I('LIST_APPEND')
        I('COMMIT', 42)
        LABEL(43)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_OBJECT', ']')
        I('ACTION', lambda scope: concat([splice(0, 'List'), splice(1, scope.lookup('xs'))]))
        I('POP_SCOPE')
        I('COMMIT', 41)
        LABEL(40)
        I('BACKTRACK', 44)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '{')
        I('LIST_START')
        LABEL(46)
        I('BACKTRACK', 47)
        I('CALL', 'formatExpr')
        I('LIST_APPEND')
        I('COMMIT', 46)
        LABEL(47)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_OBJECT', '}')
        I('ACTION', lambda scope: concat([splice(0, 'Format'), splice(1, scope.lookup('xs'))]))
        I('POP_SCOPE')
        I('COMMIT', 45)
        LABEL(44)
        I('BACKTRACK', 48)
        I('PUSH_SCOPE')
        I('CALL', 'var')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_OBJECT', '(')
        I('LIST_START')
        LABEL(50)
        I('BACKTRACK', 51)
        I('CALL', 'hostExpr')
        I('LIST_APPEND')
        I('COMMIT', 50)
        LABEL(51)
        I('LIST_END')
        I('BIND', 'ys')
        I('CALL', 'space')
        I('MATCH_OBJECT', ')')
        I('ACTION', lambda scope: concat([splice(0, 'FnCall'), splice(0, scope.lookup('x')), splice(1, scope.lookup('ys'))]))
        I('POP_SCOPE')
        I('COMMIT', 49)
        LABEL(48)
        I('PUSH_SCOPE')
        I('CALL', 'var')
        I('BIND', 'x')
        I('POP_SCOPE')
        LABEL(49)
        LABEL(45)
        LABEL(41)
        LABEL(39)
        I('RETURN')
        LABEL('var')
        I('BACKTRACK', 52)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '#')
        I('CALL', 'name')
        I('BIND', 'x')
        I('ACTION', lambda scope: concat([splice(0, 'Native'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('COMMIT', 53)
        LABEL(52)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('BACKTRACK', 55)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '=')
        I('POP_SCOPE')
        I('COMMIT', 54)
        LABEL(54)
        I('FAIL', 'no match expected')
        LABEL(55)
        I('ACTION', lambda scope: concat([splice(0, 'Lookup'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        LABEL(53)
        I('RETURN')
        LABEL('hostExprListItem')
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('LIST_START')
        LABEL(56)
        I('BACKTRACK', 57)
        I('MATCH_OBJECT', '~')
        I('LIST_APPEND')
        I('COMMIT', 56)
        LABEL(57)
        I('LIST_END')
        I('BIND', 'ys')
        I('CALL', 'hostExpr')
        I('BIND', 'x')
        I('ACTION', lambda scope: concat([splice(0, 'ListItem'), splice(0, len(scope.lookup('ys'))), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('formatExpr')
        I('BACKTRACK', 58)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_OBJECT', '>')
        I('LIST_START')
        LABEL(60)
        I('BACKTRACK', 61)
        I('CALL', 'formatExpr')
        I('LIST_APPEND')
        I('COMMIT', 60)
        LABEL(61)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_OBJECT', '<')
        I('ACTION', lambda scope: concat([splice(0, 'Indent'), splice(0, concat([splice(0, 'Format'), splice(1, scope.lookup('xs'))]))]))
        I('POP_SCOPE')
        I('COMMIT', 59)
        LABEL(58)
        I('PUSH_SCOPE')
        I('CALL', 'hostExpr')
        I('POP_SCOPE')
        LABEL(59)
        I('RETURN')
        LABEL('charseq')
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', "'")
        I('LIST_START')
        LABEL(62)
        I('BACKTRACK', 63)
        I('PUSH_SCOPE')
        I('BACKTRACK', 65)
        I('MATCH_OBJECT', "'")
        I('COMMIT', 64)
        LABEL(64)
        I('FAIL', 'no match expected')
        LABEL(65)
        I('CALL', 'matchChar')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 62)
        LABEL(63)
        I('LIST_END')
        I('BIND', 'xs')
        I('MATCH_OBJECT', "'")
        I('ACTION', lambda scope: concat([splice(0, 'And'), splice(1, scope.lookup('xs'))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('matchChar')
        I('PUSH_SCOPE')
        I('CALL', 'innerChar')
        I('BIND', 'x')
        I('ACTION', lambda scope: concat([splice(0, 'MatchObject'), splice(0, scope.lookup('x'))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('string')
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', '"')
        I('LIST_START')
        LABEL(66)
        I('BACKTRACK', 67)
        I('PUSH_SCOPE')
        I('BACKTRACK', 69)
        I('MATCH_OBJECT', '"')
        I('COMMIT', 68)
        LABEL(68)
        I('FAIL', 'no match expected')
        LABEL(69)
        I('CALL', 'innerChar')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 66)
        LABEL(67)
        I('LIST_END')
        I('BIND', 'xs')
        I('MATCH_OBJECT', '"')
        I('ACTION', lambda scope: join([scope.lookup('xs')]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('char')
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', "'")
        I('BACKTRACK', 71)
        I('MATCH_OBJECT', "'")
        I('COMMIT', 70)
        LABEL(70)
        I('FAIL', 'no match expected')
        LABEL(71)
        I('CALL', 'innerChar')
        I('BIND', 'x')
        I('MATCH_OBJECT', "'")
        I('ACTION', lambda scope: scope.lookup('x'))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('innerChar')
        I('BACKTRACK', 72)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', '\\')
        I('CALL', 'escape')
        I('POP_SCOPE')
        I('COMMIT', 73)
        LABEL(72)
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('POP_SCOPE')
        LABEL(73)
        I('RETURN')
        LABEL('escape')
        I('BACKTRACK', 74)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', '\\')
        I('ACTION', lambda scope: '\\')
        I('POP_SCOPE')
        I('COMMIT', 75)
        LABEL(74)
        I('BACKTRACK', 76)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', "'")
        I('ACTION', lambda scope: "'")
        I('POP_SCOPE')
        I('COMMIT', 77)
        LABEL(76)
        I('BACKTRACK', 78)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', '"')
        I('ACTION', lambda scope: '"')
        I('POP_SCOPE')
        I('COMMIT', 79)
        LABEL(78)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', 'n')
        I('ACTION', lambda scope: '\n')
        I('POP_SCOPE')
        LABEL(79)
        LABEL(77)
        LABEL(75)
        I('RETURN')
        LABEL('name')
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'nameStart')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(80)
        I('BACKTRACK', 81)
        I('CALL', 'nameChar')
        I('LIST_APPEND')
        I('COMMIT', 80)
        LABEL(81)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: join([scope.lookup('x'), scope.lookup('xs')]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('nameStart')
        I('BACKTRACK', 82)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'z')
        I('POP_SCOPE')
        I('COMMIT', 83)
        LABEL(82)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'Z')
        I('POP_SCOPE')
        LABEL(83)
        I('RETURN')
        LABEL('nameChar')
        I('BACKTRACK', 84)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'z')
        I('POP_SCOPE')
        I('COMMIT', 85)
        LABEL(84)
        I('BACKTRACK', 86)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'Z')
        I('POP_SCOPE')
        I('COMMIT', 87)
        LABEL(86)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', '0', '9')
        I('POP_SCOPE')
        LABEL(87)
        LABEL(85)
        I('RETURN')
        LABEL('space')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(88)
        I('BACKTRACK', 89)
        I('BACKTRACK', 90)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', ' ')
        I('POP_SCOPE')
        I('COMMIT', 91)
        LABEL(90)
        I('PUSH_SCOPE')
        I('MATCH_OBJECT', '\n')
        I('POP_SCOPE')
        LABEL(91)
        I('LIST_APPEND')
        I('COMMIT', 88)
        LABEL(89)
        I('LIST_END')
        I('POP_SCOPE')
        I('RETURN')

class CodeGenerator(Grammar):

    def assemble(self, I, LABEL):
        LABEL('ast')
        I('PUSH_SCOPE')
        I('PUSH_STREAM')
        I('MATCH_CALL_RULE')
        I('BIND', 'x')
        I('POP_STREAM')
        I('ACTION', lambda scope: scope.lookup('x'))
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
        I('ACTION', lambda scope: join(['class ', scope.lookup('x'), '(Grammar):\n\n', indent(join(['def assemble(self, I, LABEL):\n', indent(join([scope.lookup('ys')]))]))]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Rule')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda scope: join(['LABEL(', scope.lookup('x'), ')\n', scope.lookup('y'), "I('RETURN')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Or')
        I('BACKTRACK', 2)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('CALL', 'Or')
        I('BIND', 'y')
        I('ACTION', lambda scope: scope.bind('a', scope.lookup('label')(), lambda: scope.bind('b', scope.lookup('label')(), lambda: join(["I('BACKTRACK', ", scope.lookup('a'), ')\n', scope.lookup('x'), "I('COMMIT', ", scope.lookup('b'), ')\n', 'LABEL(', scope.lookup('a'), ')\n', scope.lookup('y'), 'LABEL(', scope.lookup('b'), ')\n']))))
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
        I('ACTION', lambda scope: join(["I('PUSH_SCOPE')\n", scope.lookup('x'), "I('POP_SCOPE')\n"]))
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
        I('ACTION', lambda scope: join([scope.lookup('y'), "I('BIND', ", scope.lookup('x'), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Star')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: scope.bind('a', scope.lookup('label')(), lambda: scope.bind('b', scope.lookup('label')(), lambda: join(["I('LIST_START')\n", 'LABEL(', scope.lookup('a'), ')\n', "I('BACKTRACK', ", scope.lookup('b'), ')\n', scope.lookup('x'), "I('LIST_APPEND')\n", "I('COMMIT', ", scope.lookup('a'), ')\n', 'LABEL(', scope.lookup('b'), ')\n', "I('LIST_END')\n"]))))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Not')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: scope.bind('a', scope.lookup('label')(), lambda: scope.bind('b', scope.lookup('label')(), lambda: join(["I('BACKTRACK', ", scope.lookup('b'), ')\n', scope.lookup('x'), "I('COMMIT', ", scope.lookup('a'), ')\n', 'LABEL(', scope.lookup('a'), ')\n', "I('FAIL', 'no match expected')\n", 'LABEL(', scope.lookup('b'), ')\n']))))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchCallRule')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: join(["I('MATCH_CALL_RULE')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Actions')
        I('PUSH_SCOPE')
        I('CALL', 'actions')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(["I('ACTION', lambda scope: ", scope.lookup('x'), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('actions')
        I('BACKTRACK', 6)
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('CALL', 'actions')
        I('BIND', 'z')
        I('ACTION', lambda scope: join(['scope.bind(', scope.lookup('x'), ', ', scope.lookup('y'), ', lambda: ', scope.lookup('z'), ')']))
        I('POP_SCOPE')
        I('COMMIT', 7)
        LABEL(6)
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('CALL', 'ast')
        I('POP_SCOPE')
        LABEL(7)
        I('RETURN')
        LABEL('MatchRule')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(["I('CALL', ", scope.lookup('x'), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchRange')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'py')
        I('BIND', 'y')
        I('ACTION', lambda scope: join(["I('MATCH_RANGE', ", scope.lookup('x'), ', ', scope.lookup('y'), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchObject')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(["I('MATCH_OBJECT', ", scope.lookup('x'), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchAny')
        I('PUSH_SCOPE')
        I('ACTION', lambda scope: join(["I('MATCH_ANY')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchList')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(["I('PUSH_STREAM')\n", scope.lookup('x'), "I('POP_STREAM')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('String')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('List')
        I('PUSH_SCOPE')
        I('CALL', 'astItems')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(['concat([', scope.lookup('x'), '])']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('ListItem')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda scope: join(['splice(', scope.lookup('x'), ', ', scope.lookup('y'), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Format')
        I('PUSH_SCOPE')
        I('CALL', 'astItems')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(['join([', scope.lookup('x'), '])']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Indent')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(['indent(', scope.lookup('x'), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('FnCall')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('CALL', 'astItems')
        I('BIND', 'y')
        I('ACTION', lambda scope: join([scope.lookup('x'), '(', scope.lookup('y'), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Native')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Lookup')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda scope: join(['scope.lookup(', scope.lookup('x'), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('astItems')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(8)
        I('BACKTRACK', 9)
        I('CALL', 'ast')
        I('LIST_APPEND')
        I('COMMIT', 8)
        LABEL(9)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: join(scope.lookup('xs'), ', '))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('py')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('ACTION', lambda scope: repr(scope.lookup('x')))
        I('POP_SCOPE')
        I('RETURN')

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(
                CodeGenerator().run(
                    "ast",
                    [Parser().run("grammar", sys.stdin.read())]
                )
            )
        except MatchError as e:
            stream = e.stream
            for pos in e.pos[:-1]:
                stream = stream[pos]
            pos = e.pos[-1]
            MARKER = "\033[0;31m<ERROR POSITION>\033[0m"
            if isinstance(stream, basestring):
                stream_string = stream[:pos] + MARKER + stream[pos:]
            else:
                stream_string = pprint.pformat(stream)
            sys.exit("ERROR: {}\nPOSITION: {}\nSTREAM:\n{}".format(
              e.message,
              pos,
              indent(stream_string)
            ))
