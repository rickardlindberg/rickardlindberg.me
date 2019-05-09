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
        self._action = _ConstantSemanticAction(None)
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
                self._action = _ConstantSemanticAction(self._label_counter)
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
                    self._action = _ConstantSemanticAction(arg1)
            elif name == 'MATCH_RANGE':
                if self._pos >= len(self._input) or not (arg1 <= self._input[self._pos] <= arg2):
                    fail = "match range"
                else:
                    self._action = _ConstantSemanticAction(self._input[self._pos])
                    self._pos += 1
            elif name == 'MATCH_ANY':
                if self._pos >= len(self._input):
                    fail = "match any"
                else:
                    self._action = _ConstantSemanticAction(self._input[self._pos])
                    self._pos += 1
            elif name == 'MATCH_STRING':
                if self._pos >= len(self._input) or self._input[self._pos] != arg1:
                    fail = "match string {}".format(arg1)
                else:
                    self._action = _ConstantSemanticAction(arg1)
                    self._pos += 1
            elif name == 'BIND':
                self._vars[-1][arg1] = self._action
            elif name == 'ACTION':
                self._action = _SemanticAction(arg1, self._vars[-1])
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
                self._action = _SemanticAction(lambda xs: [x.eval() for x in xs], self._vars.pop())
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
