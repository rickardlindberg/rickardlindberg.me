import sys

SUPPORT = 'try:\n    from cStringIO import StringIO\nexcept:\n    from StringIO import StringIO\n\nclass _Program(object):\n\n    def run(self, rule_name, input_object):\n        instructions = self._instructions\n        labels = self._labels\n        label_counter = 0\n        envs = []\n        stack = []\n        last_action = _ConstantSemanticAction(None)\n        pc = labels[rule_name]\n        memo = {}\n        if isinstance(input_object, basestring):\n            stream = input_object\n        else:\n            stream = [input_object]\n        pos = 0\n        stream_stack = []\n        fail_message = ""\n        while True:\n            name, arg1, arg2 = instructions[pc]\n            if name == \'PUSH_SCOPE\':\n                envs.append({})\n                pc += 1\n                continue\n            elif name == \'BACKTRACK\':\n                stack.append((labels[arg1], pos, len(stream_stack), len(envs)))\n                pc += 1\n                continue\n            elif name == \'CALL\':\n                key = (arg1, tuple([x[1] for x in stream_stack]+[pos]))\n                if key in memo:\n                    last_action, stream_stack = memo[key]\n                    stream_stack = stream_stack[:]\n                    stream, pos = stream_stack.pop()\n                    pc += 1\n                else:\n                    stack.append((pc+1, key))\n                    pc = labels[arg1]\n                continue\n            elif name == \'MATCH_CHARSEQ\':\n                for char in arg1:\n                    if pos >= len(stream) or stream[pos] != char:\n                        fail_message = "match charseq"\n                        break\n                    pos += 1\n                else:\n                    last_action = _ConstantSemanticAction(arg1)\n                    pc += 1\n                    continue\n            elif name == \'COMMIT\':\n                stack.pop()\n                pc = labels[arg1]\n                continue\n            elif name == \'POP_SCOPE\':\n                envs.pop()\n                pc += 1\n                continue\n            elif name == \'RETURN\':\n                if len(stack) == 0:\n                    result = last_action.eval()\n                    if isinstance(result, _Builder):\n                        return result.build_string()\n                    else:\n                        return result\n                pc, key = stack.pop()\n                memo[key] = (last_action, stream_stack[:]+[(stream, pos)])\n                continue\n            elif name == \'LIST_APPEND\':\n                envs[-1].append(last_action)\n                pc += 1\n                continue\n            elif name == \'BIND\':\n                envs[-1][arg1] = last_action\n                pc += 1\n                continue\n            elif name == \'ACTION\':\n                last_action = _SemanticAction(arg1, envs[-1])\n                pc += 1\n                continue\n            elif name == \'MATCH_RANGE\':\n                if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):\n                    fail_message = "match range"\n                else:\n                    last_action = _ConstantSemanticAction(stream[pos])\n                    pos += 1\n                    pc += 1\n                    continue\n            elif name == \'LIST_START\':\n                envs.append([])\n                pc += 1\n                continue\n            elif name == \'LIST_END\':\n                last_action = _SemanticAction(lambda xs: [x.eval() for x in xs], envs.pop())\n                pc += 1\n                continue\n            elif name == \'MATCH_ANY\':\n                if pos >= len(stream):\n                    fail_message = "match any"\n                else:\n                    last_action = _ConstantSemanticAction(stream[pos])\n                    pos += 1\n                    pc += 1\n                    continue\n            elif name == \'PUSH_INPUT\':\n                if pos >= len(stream) or not isinstance(stream[pos], list):\n                    fail_message = "push input"\n                else:\n                    stream_stack.append((stream, pos+1))\n                    stream = stream[pos]\n                    pos = 0\n                    pc += 1\n                    continue\n            elif name == \'POP_INPUT\':\n                if pos < len(stream):\n                    fail_message = "pop input"\n                else:\n                    stream, pos = stream_stack.pop()\n                    pc += 1\n                    continue\n            elif name == \'MATCH_CALL_RULE\':\n                if pos >= len(stream):\n                    fail_message = "match call rule"\n                else:\n                    fn_name = stream[pos]\n                    key = (fn_name, tuple([x[1] for x in stream_stack]+[pos]))\n                    if key in memo:\n                        last_action, stream_stack = memo[key]\n                        stream_stack = stream_stack[:]\n                        stream, pos = stream_stack.pop()\n                        pc += 1\n                    else:\n                        stack.append((pc+1, key))\n                        pc = labels[fn_name]\n                        pos += 1\n                    continue\n            elif name == \'FAIL\':\n                fail_message = "fail"\n            elif name == \'LABEL\':\n                last_action = _ConstantSemanticAction(label_counter)\n                label_counter += 1\n                pc += 1\n                continue\n            elif name == \'MATCH_STRING\':\n                if pos >= len(stream) or stream[pos] != arg1:\n                    fail_message = "match string {}".format(arg1)\n                else:\n                    last_action = _ConstantSemanticAction(arg1)\n                    pos += 1\n                    pc += 1\n                    continue\n            else:\n                raise Exception("unknown command {}".format(name))\n            while stack and len(stack[-1]) == 2:\n                stack.pop()\n            if not stack:\n                raise Exception("totally failed: {}".format(fail_message))\n            (pc, pos, stream_stack_len, envs_len) = stack.pop()\n            if len(stream_stack) > stream_stack_len:\n                stream = stream_stack[stream_stack_len][0]\n            stream_stack = stream_stack[:stream_stack_len]\n            envs = envs[:envs_len]\n\nclass _SemanticAction(object):\n\n    def __init__(self, fn, env):\n        self.fn = fn\n        self.env = env\n\n    def eval(self):\n        return self.fn(self.env)\n\nclass _ConstantSemanticAction(object):\n\n    def __init__(self, value):\n        self.value = value\n\n    def eval(self):\n        return self.value\n\nclass _Builder(object):\n\n    def build_string(self):\n        output = _Output()\n        self.write(output)\n        return output.value\n\n    @classmethod\n    def create(self, item):\n        if isinstance(item, _Builder):\n            return item\n        elif isinstance(item, list):\n            return _ListBuilder([_Builder.create(x) for x in item])\n        else:\n            return _AtomBuilder(item)\n\nclass _Output(object):\n\n    def __init__(self):\n        self.buffer = StringIO()\n        self.indentation = 0\n        self.on_newline = True\n\n    @property\n    def value(self):\n        return self.buffer.getvalue()\n\n    def write(self, value):\n        for ch in value:\n            is_linebreak = ch == "\\n"\n            if self.indentation and self.on_newline and not is_linebreak:\n                self.buffer.write("    "*self.indentation)\n            self.buffer.write(ch)\n            self.on_newline = is_linebreak\n\nclass _ListBuilder(_Builder):\n\n    def __init__(self, builders):\n        self.builders = builders\n\n    def write(self, output):\n        for builder in self.builders:\n            builder.write(output)\n\nclass _AtomBuilder(_Builder):\n\n    def __init__(self, atom):\n        self.atom = atom\n\n    def write(self, output):\n        output.write(str(self.atom))\n\nclass _IndentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation += 1\n\nclass _DedentBuilder(_Builder):\n\n    def write(self, output):\n        output.indentation -= 1\n'

try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

class _Program(object):

    def run(self, rule_name, input_object):
        instructions = self._instructions
        labels = self._labels
        label_counter = 0
        envs = []
        stack = []
        last_action = _ConstantSemanticAction(None)
        pc = labels[rule_name]
        memo = {}
        if isinstance(input_object, basestring):
            stream = input_object
        else:
            stream = [input_object]
        pos = 0
        stream_stack = []
        fail_message = ""
        while True:
            name, arg1, arg2 = instructions[pc]
            if name == 'PUSH_SCOPE':
                envs.append({})
                pc += 1
                continue
            elif name == 'BACKTRACK':
                stack.append((labels[arg1], pos, len(stream_stack), len(envs)))
                pc += 1
                continue
            elif name == 'CALL':
                key = (arg1, tuple([x[1] for x in stream_stack]+[pos]))
                if key in memo:
                    last_action, stream_stack = memo[key]
                    stream_stack = stream_stack[:]
                    stream, pos = stream_stack.pop()
                    pc += 1
                else:
                    stack.append((pc+1, key))
                    pc = labels[arg1]
                continue
            elif name == 'MATCH_CHARSEQ':
                for char in arg1:
                    if pos >= len(stream) or stream[pos] != char:
                        fail_message = "match charseq"
                        break
                    pos += 1
                else:
                    last_action = _ConstantSemanticAction(arg1)
                    pc += 1
                    continue
            elif name == 'COMMIT':
                stack.pop()
                pc = labels[arg1]
                continue
            elif name == 'POP_SCOPE':
                envs.pop()
                pc += 1
                continue
            elif name == 'RETURN':
                if len(stack) == 0:
                    result = last_action.eval()
                    if isinstance(result, _Builder):
                        return result.build_string()
                    else:
                        return result
                pc, key = stack.pop()
                memo[key] = (last_action, stream_stack[:]+[(stream, pos)])
                continue
            elif name == 'LIST_APPEND':
                envs[-1].append(last_action)
                pc += 1
                continue
            elif name == 'BIND':
                envs[-1][arg1] = last_action
                pc += 1
                continue
            elif name == 'ACTION':
                last_action = _SemanticAction(arg1, envs[-1])
                pc += 1
                continue
            elif name == 'MATCH_RANGE':
                if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
                    fail_message = "match range"
                else:
                    last_action = _ConstantSemanticAction(stream[pos])
                    pos += 1
                    pc += 1
                    continue
            elif name == 'LIST_START':
                envs.append([])
                pc += 1
                continue
            elif name == 'LIST_END':
                last_action = _SemanticAction(lambda xs: [x.eval() for x in xs], envs.pop())
                pc += 1
                continue
            elif name == 'MATCH_ANY':
                if pos >= len(stream):
                    fail_message = "match any"
                else:
                    last_action = _ConstantSemanticAction(stream[pos])
                    pos += 1
                    pc += 1
                    continue
            elif name == 'PUSH_INPUT':
                if pos >= len(stream) or not isinstance(stream[pos], list):
                    fail_message = "push input"
                else:
                    stream_stack.append((stream, pos+1))
                    stream = stream[pos]
                    pos = 0
                    pc += 1
                    continue
            elif name == 'POP_INPUT':
                if pos < len(stream):
                    fail_message = "pop input"
                else:
                    stream, pos = stream_stack.pop()
                    pc += 1
                    continue
            elif name == 'MATCH_CALL_RULE':
                if pos >= len(stream):
                    fail_message = "match call rule"
                else:
                    fn_name = stream[pos]
                    key = (fn_name, tuple([x[1] for x in stream_stack]+[pos]))
                    if key in memo:
                        last_action, stream_stack = memo[key]
                        stream_stack = stream_stack[:]
                        stream, pos = stream_stack.pop()
                        pc += 1
                    else:
                        stack.append((pc+1, key))
                        pc = labels[fn_name]
                        pos += 1
                    continue
            elif name == 'FAIL':
                fail_message = "fail"
            elif name == 'LABEL':
                last_action = _ConstantSemanticAction(label_counter)
                label_counter += 1
                pc += 1
                continue
            elif name == 'MATCH_STRING':
                if pos >= len(stream) or stream[pos] != arg1:
                    fail_message = "match string {}".format(arg1)
                else:
                    last_action = _ConstantSemanticAction(arg1)
                    pos += 1
                    pc += 1
                    continue
            else:
                raise Exception("unknown command {}".format(name))
            while stack and len(stack[-1]) == 2:
                stack.pop()
            if not stack:
                raise Exception("totally failed: {}".format(fail_message))
            (pc, pos, stream_stack_len, envs_len) = stack.pop()
            if len(stream_stack) > stream_stack_len:
                stream = stream_stack[stream_stack_len][0]
            stream_stack = stream_stack[:stream_stack_len]
            envs = envs[:envs_len]

class _SemanticAction(object):

    def __init__(self, fn, env):
        self.fn = fn
        self.env = env

    def eval(self):
        return self.fn(self.env)

class _ConstantSemanticAction(object):

    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value

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

class Parser(_Program):
    def __init__(self):
        instructions = []
        labels = {}
        def I(name, x=None, y=None):
            instructions.append((name, x, y))
        def LABEL(name):
            labels[name] = len(instructions)
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
        I('ACTION', lambda env: (['Grammar']+[env['x'].eval()]+env['ys'].eval()+[]))
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
        I('ACTION', lambda env: (['Rule']+[env['x'].eval()]+[env['y'].eval()]+[]))
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
        I('ACTION', lambda env: (['Or']+[env['x'].eval()]+env['xs'].eval()+[]))
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
        I('ACTION', lambda env: (['Scope']+[(['And']+[env['x'].eval()]+env['xs'].eval()+[])]+[]))
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
        I('ACTION', lambda env: (['Bind']+[env['y'].eval()]+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 9)
        LABEL(8)
        I('PUSH_SCOPE')
        I('CALL', 'expr1')
        I('POP_SCOPE')
        LABEL(9)
        I('RETURN')
        LABEL('expr1')
        I('BACKTRACK', 18)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '*')
        I('ACTION', lambda env: (['Star']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 19)
        LABEL(18)
        I('BACKTRACK', 16)
        I('PUSH_SCOPE')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '?')
        I('ACTION', lambda env: (['Or']+[env['x'].eval()]+[(['And']+[])]+[]))
        I('POP_SCOPE')
        I('COMMIT', 17)
        LABEL(16)
        I('BACKTRACK', 14)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '!')
        I('CALL', 'expr2')
        I('BIND', 'x')
        I('ACTION', lambda env: (['Not']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 15)
        LABEL(14)
        I('BACKTRACK', 12)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '%')
        I('ACTION', lambda env: (['MatchCallRule']+[]))
        I('POP_SCOPE')
        I('COMMIT', 13)
        LABEL(12)
        I('BACKTRACK', 10)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '#')
        I('ACTION', lambda env: (['Label']+[]))
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
        LABEL(19)
        I('RETURN')
        LABEL('expr2')
        I('BACKTRACK', 36)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '->')
        I('CALL', 'hostExpr')
        I('BIND', 'x')
        I('ACTION', lambda env: (['SemanticAction']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 37)
        LABEL(36)
        I('BACKTRACK', 34)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('BACKTRACK', 21)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '=')
        I('POP_SCOPE')
        I('COMMIT', 20)
        LABEL(20)
        I('FAIL')
        LABEL(21)
        I('ACTION', lambda env: (['MatchRule']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 35)
        LABEL(34)
        I('BACKTRACK', 32)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'char')
        I('BIND', 'x')
        I('MATCH_CHARSEQ', '-')
        I('CALL', 'char')
        I('BIND', 'y')
        I('ACTION', lambda env: (['MatchRange']+[env['x'].eval()]+[env['y'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 33)
        LABEL(32)
        I('BACKTRACK', 30)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'string')
        I('BIND', 'x')
        I('ACTION', lambda env: (['MatchString']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 31)
        LABEL(30)
        I('BACKTRACK', 28)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'charseq')
        I('BIND', 'x')
        I('ACTION', lambda env: (['MatchCharseq']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 29)
        LABEL(28)
        I('BACKTRACK', 26)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '.')
        I('ACTION', lambda env: (['MatchAny']+[]))
        I('POP_SCOPE')
        I('COMMIT', 27)
        LABEL(26)
        I('BACKTRACK', 24)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '(')
        I('CALL', 'choice')
        I('BIND', 'x')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ')')
        I('ACTION', lambda env: env['x'].eval())
        I('POP_SCOPE')
        I('COMMIT', 25)
        LABEL(24)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '[')
        I('LIST_START')
        LABEL(22)
        I('BACKTRACK', 23)
        I('CALL', 'expr')
        I('LIST_APPEND')
        I('COMMIT', 22)
        LABEL(23)
        I('LIST_END')
        I('BIND', 'xs')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', ']')
        I('ACTION', lambda env: (['MatchList']+[(['And']+env['xs'].eval()+[])]+[]))
        I('POP_SCOPE')
        LABEL(25)
        LABEL(27)
        LABEL(29)
        LABEL(31)
        LABEL(33)
        LABEL(35)
        LABEL(37)
        I('RETURN')
        LABEL('hostExpr')
        I('BACKTRACK', 50)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'string')
        I('BIND', 'x')
        I('ACTION', lambda env: (['String']+[env['x'].eval()]+[]))
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
        I('ACTION', lambda env: (['List']+env['xs'].eval()+[]))
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
        I('ACTION', lambda env: (['Builder']+env['xs'].eval()+[]))
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
        I('ACTION', lambda env: (['FnCall']+[env['x'].eval()]+env['ys'].eval()+[]))
        I('POP_SCOPE')
        I('COMMIT', 45)
        LABEL(44)
        I('PUSH_SCOPE')
        I('CALL', 'name')
        I('BIND', 'x')
        I('ACTION', lambda env: (['VarLookup']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        LABEL(45)
        LABEL(47)
        LABEL(49)
        LABEL(51)
        I('RETURN')
        LABEL('hostExprListItem')
        I('BACKTRACK', 52)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '~')
        I('CALL', 'hostExpr')
        I('BIND', 'x')
        I('ACTION', lambda env: (['ListItemSplice']+[env['x'].eval()]+[]))
        I('POP_SCOPE')
        I('COMMIT', 53)
        LABEL(52)
        I('PUSH_SCOPE')
        I('CALL', 'hostExpr')
        I('POP_SCOPE')
        LABEL(53)
        I('RETURN')
        LABEL('buildExpr')
        I('BACKTRACK', 56)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '>')
        I('ACTION', lambda env: (['IndentBuilder']+[]))
        I('POP_SCOPE')
        I('COMMIT', 57)
        LABEL(56)
        I('BACKTRACK', 54)
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('MATCH_CHARSEQ', '<')
        I('ACTION', lambda env: (['DedentBuilder']+[]))
        I('POP_SCOPE')
        I('COMMIT', 55)
        LABEL(54)
        I('PUSH_SCOPE')
        I('CALL', 'hostExpr')
        I('POP_SCOPE')
        LABEL(55)
        LABEL(57)
        I('RETURN')
        LABEL('string')
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '"')
        I('LIST_START')
        LABEL(60)
        I('BACKTRACK', 61)
        I('PUSH_SCOPE')
        I('BACKTRACK', 59)
        I('MATCH_CHARSEQ', '"')
        I('COMMIT', 58)
        LABEL(58)
        I('FAIL')
        LABEL(59)
        I('CALL', 'innerChar')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 60)
        LABEL(61)
        I('LIST_END')
        I('BIND', 'xs')
        I('MATCH_CHARSEQ', '"')
        I('ACTION', lambda env: join(env['xs'].eval()))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('charseq')
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', "'")
        I('LIST_START')
        LABEL(64)
        I('BACKTRACK', 65)
        I('PUSH_SCOPE')
        I('BACKTRACK', 63)
        I('MATCH_CHARSEQ', "'")
        I('COMMIT', 62)
        LABEL(62)
        I('FAIL')
        LABEL(63)
        I('CALL', 'innerChar')
        I('POP_SCOPE')
        I('LIST_APPEND')
        I('COMMIT', 64)
        LABEL(65)
        I('LIST_END')
        I('BIND', 'xs')
        I('MATCH_CHARSEQ', "'")
        I('ACTION', lambda env: join(env['xs'].eval()))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('char')
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', "'")
        I('BACKTRACK', 67)
        I('MATCH_CHARSEQ', "'")
        I('COMMIT', 66)
        LABEL(66)
        I('FAIL')
        LABEL(67)
        I('CALL', 'innerChar')
        I('BIND', 'x')
        I('MATCH_CHARSEQ', "'")
        I('ACTION', lambda env: env['x'].eval())
        I('POP_SCOPE')
        I('RETURN')
        LABEL('innerChar')
        I('BACKTRACK', 68)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '\\')
        I('CALL', 'escape')
        I('POP_SCOPE')
        I('COMMIT', 69)
        LABEL(68)
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('POP_SCOPE')
        LABEL(69)
        I('RETURN')
        LABEL('escape')
        I('BACKTRACK', 74)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '\\')
        I('ACTION', lambda env: '\\')
        I('POP_SCOPE')
        I('COMMIT', 75)
        LABEL(74)
        I('BACKTRACK', 72)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', "'")
        I('ACTION', lambda env: "'")
        I('POP_SCOPE')
        I('COMMIT', 73)
        LABEL(72)
        I('BACKTRACK', 70)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '"')
        I('ACTION', lambda env: '"')
        I('POP_SCOPE')
        I('COMMIT', 71)
        LABEL(70)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', 'n')
        I('ACTION', lambda env: '\n')
        I('POP_SCOPE')
        LABEL(71)
        LABEL(73)
        LABEL(75)
        I('RETURN')
        LABEL('name')
        I('PUSH_SCOPE')
        I('CALL', 'space')
        I('CALL', 'nameStart')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(76)
        I('BACKTRACK', 77)
        I('CALL', 'nameChar')
        I('LIST_APPEND')
        I('COMMIT', 76)
        LABEL(77)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda env: join(([env['x'].eval()]+env['xs'].eval()+[])))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('nameStart')
        I('BACKTRACK', 78)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'z')
        I('POP_SCOPE')
        I('COMMIT', 79)
        LABEL(78)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'Z')
        I('POP_SCOPE')
        LABEL(79)
        I('RETURN')
        LABEL('nameChar')
        I('BACKTRACK', 82)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'a', 'z')
        I('POP_SCOPE')
        I('COMMIT', 83)
        LABEL(82)
        I('BACKTRACK', 80)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', 'A', 'Z')
        I('POP_SCOPE')
        I('COMMIT', 81)
        LABEL(80)
        I('PUSH_SCOPE')
        I('MATCH_RANGE', '0', '9')
        I('POP_SCOPE')
        LABEL(81)
        LABEL(83)
        I('RETURN')
        LABEL('space')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(86)
        I('BACKTRACK', 87)
        I('BACKTRACK', 84)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', ' ')
        I('POP_SCOPE')
        I('COMMIT', 85)
        LABEL(84)
        I('PUSH_SCOPE')
        I('MATCH_CHARSEQ', '\n')
        I('POP_SCOPE')
        LABEL(85)
        I('LIST_APPEND')
        I('COMMIT', 86)
        LABEL(87)
        I('LIST_END')
        I('POP_SCOPE')
        I('RETURN')
        self._instructions = instructions
        self._labels = labels

class CodeGenerator(_Program):
    def __init__(self):
        instructions = []
        labels = {}
        def I(name, x=None, y=None):
            instructions.append((name, x, y))
        def LABEL(name):
            labels[name] = len(instructions)
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
        I('ACTION', lambda env: _Builder.create(['class ', env['x'].eval(), '(_Program):\n', _IndentBuilder(), 'def __init__(self):\n', _IndentBuilder(), 'instructions = []\n', 'labels = {}\n', 'def I(name, x=None, y=None):\n', _IndentBuilder(), 'instructions.append((name, x, y))\n', _DedentBuilder(), 'def LABEL(name):\n', _IndentBuilder(), 'labels[name] = len(instructions)\n', _DedentBuilder(), env['ys'].eval(), 'self._instructions = instructions\n', 'self._labels = labels\n', _DedentBuilder(), _DedentBuilder()]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Rule')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda env: _Builder.create(['LABEL(', env['x'].eval(), ')\n', env['y'].eval(), "I('RETURN')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Or')
        I('BACKTRACK', 4)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('BACKTRACK', 3)
        I('MATCH_ANY')
        I('COMMIT', 2)
        LABEL(2)
        I('FAIL')
        LABEL(3)
        I('ACTION', lambda env: env['x'].eval())
        I('POP_SCOPE')
        I('COMMIT', 5)
        LABEL(4)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('CALL', 'Or')
        I('BIND', 'y')
        I('LABEL')
        I('BIND', 'a')
        I('LABEL')
        I('BIND', 'b')
        I('ACTION', lambda env: _Builder.create(["I('BACKTRACK', ", env['a'].eval(), ')\n', env['x'].eval(), "I('COMMIT', ", env['b'].eval(), ')\n', 'LABEL(', env['a'].eval(), ')\n', env['y'].eval(), 'LABEL(', env['b'].eval(), ')\n']))
        I('POP_SCOPE')
        LABEL(5)
        I('RETURN')
        LABEL('Scope')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(["I('PUSH_SCOPE')\n", env['x'].eval(), "I('POP_SCOPE')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('And')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(6)
        I('BACKTRACK', 7)
        I('CALL', 'ast')
        I('LIST_APPEND')
        I('COMMIT', 6)
        LABEL(7)
        I('LIST_END')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Bind')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'ast')
        I('BIND', 'y')
        I('ACTION', lambda env: _Builder.create([env['y'].eval(), "I('BIND', ", env['x'].eval(), ')\n']))
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
        I('ACTION', lambda env: _Builder.create(["I('LIST_START')\n", 'LABEL(', env['a'].eval(), ')\n', "I('BACKTRACK', ", env['b'].eval(), ')\n', env['x'].eval(), "I('LIST_APPEND')\n", "I('COMMIT', ", env['a'].eval(), ')\n', 'LABEL(', env['b'].eval(), ')\n', "I('LIST_END')\n"]))
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
        I('ACTION', lambda env: _Builder.create(["I('BACKTRACK', ", env['b'].eval(), ')\n', env['x'].eval(), "I('COMMIT', ", env['a'].eval(), ')\n', 'LABEL(', env['a'].eval(), ')\n', "I('FAIL')\n", 'LABEL(', env['b'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchCallRule')
        I('PUSH_SCOPE')
        I('ACTION', lambda env: _Builder.create(["I('MATCH_CALL_RULE')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Label')
        I('PUSH_SCOPE')
        I('ACTION', lambda env: _Builder.create(["I('LABEL')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('SemanticAction')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(["I('ACTION', lambda env: ", env['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchRule')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(["I('CALL', ", env['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchRange')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('CALL', 'py')
        I('BIND', 'y')
        I('ACTION', lambda env: _Builder.create(["I('MATCH_RANGE', ", env['x'].eval(), ', ', env['y'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchString')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(["I('MATCH_STRING', ", env['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchCharseq')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(["I('MATCH_CHARSEQ', ", env['x'].eval(), ')\n']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchAny')
        I('PUSH_SCOPE')
        I('ACTION', lambda env: _Builder.create(["I('MATCH_ANY')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('MatchList')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(["I('PUSH_INPUT')\n", env['x'].eval(), "I('POP_INPUT')\n"]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('String')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('POP_SCOPE')
        I('RETURN')
        LABEL('List')
        I('PUSH_SCOPE')
        I('CALL', 'astList')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('Builder')
        I('PUSH_SCOPE')
        I('CALL', 'astItems')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(['_Builder.create([', env['x'].eval(), '])']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('IndentBuilder')
        I('PUSH_SCOPE')
        I('ACTION', lambda env: _Builder.create(['_IndentBuilder()']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('DedentBuilder')
        I('PUSH_SCOPE')
        I('ACTION', lambda env: _Builder.create(['_DedentBuilder()']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('FnCall')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('CALL', 'astItems')
        I('BIND', 'y')
        I('ACTION', lambda env: _Builder.create([env['x'].eval(), '(', env['y'].eval(), ')']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('VarLookup')
        I('PUSH_SCOPE')
        I('CALL', 'py')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(['env[', env['x'].eval(), '].eval()']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('ast')
        I('PUSH_SCOPE')
        I('PUSH_INPUT')
        I('MATCH_CALL_RULE')
        I('BIND', 'x')
        I('POP_INPUT')
        I('ACTION', lambda env: _Builder.create([env['x'].eval()]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('astItems')
        I('BACKTRACK', 10)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('LIST_START')
        LABEL(8)
        I('BACKTRACK', 9)
        I('CALL', 'astItem')
        I('LIST_APPEND')
        I('COMMIT', 8)
        LABEL(9)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda env: _Builder.create([env['x'].eval(), env['xs'].eval()]))
        I('POP_SCOPE')
        I('COMMIT', 11)
        LABEL(10)
        I('PUSH_SCOPE')
        I('ACTION', lambda env: _Builder.create([]))
        I('POP_SCOPE')
        LABEL(11)
        I('RETURN')
        LABEL('astItem')
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create([', ', env['x'].eval()]))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('astList')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(12)
        I('BACKTRACK', 13)
        I('CALL', 'astListItem')
        I('LIST_APPEND')
        I('COMMIT', 12)
        LABEL(13)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda env: _Builder.create(['(', env['xs'].eval(), '[])']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('astListItem')
        I('BACKTRACK', 14)
        I('PUSH_SCOPE')
        I('PUSH_INPUT')
        I('MATCH_STRING', 'ListItemSplice')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('POP_INPUT')
        I('ACTION', lambda env: _Builder.create([env['x'].eval(), '+']))
        I('POP_SCOPE')
        I('COMMIT', 15)
        LABEL(14)
        I('PUSH_SCOPE')
        I('CALL', 'ast')
        I('BIND', 'x')
        I('ACTION', lambda env: _Builder.create(['[', env['x'].eval(), ']+']))
        I('POP_SCOPE')
        LABEL(15)
        I('RETURN')
        LABEL('py')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('ACTION', lambda env: repr(env['x'].eval()))
        I('POP_SCOPE')
        I('RETURN')
        self._instructions = instructions
        self._labels = labels

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        sys.stdout.write(compile_grammar(sys.stdin.read()))