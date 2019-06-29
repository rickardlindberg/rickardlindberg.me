try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

def rlmeta_vm(instructions, labels, start_rule, stream):
    label_counter = 0
    envs = []
    stack = []
    last_action = _ConstantSemanticAction(None)
    pc = labels[start_rule]
    memo = {}
    stream, pos, stream_pos_stack  = (stream, 0, [])
    fail_message = ""
    while True:
        name, arg1, arg2 = instructions[pc]
        if name == "PUSH_SCOPE":
            envs.append({})
            pc += 1
            continue
        elif name == "BACKTRACK":
            stack.append((labels[arg1], pos, len(stream_pos_stack), len(envs)))
            pc += 1
            continue
        elif name == "CALL":
            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))
            if key in memo:
                last_action, stream_pos_stack = memo[key]
                stream_pos_stack = stream_pos_stack[:]
                stream, pos = stream_pos_stack.pop()
                pc += 1
            else:
                stack.append((pc+1, key))
                pc = labels[arg1]
            continue
        elif name == "MATCH_CHARSEQ":
            for char in arg1:
                if pos >= len(stream) or stream[pos] != char:
                    fail_message = "match charseq"
                    break
                pos += 1
            else:
                last_action = _ConstantSemanticAction(arg1)
                pc += 1
                continue
        elif name == "COMMIT":
            stack.pop()
            pc = labels[arg1]
            continue
        elif name == "POP_SCOPE":
            envs.pop()
            pc += 1
            continue
        elif name == "RETURN":
            if len(stack) == 0:
                return last_action.eval()
            pc, key = stack.pop()
            memo[key] = (last_action, stream_pos_stack+[(stream, pos)])
            continue
        elif name == "LIST_APPEND":
            envs[-1].append(last_action)
            pc += 1
            continue
        elif name == "BIND":
            envs[-1][arg1] = last_action
            pc += 1
            continue
        elif name == "ACTION":
            last_action = _SemanticAction(arg1, envs[-1])
            pc += 1
            continue
        elif name == "MATCH_RANGE":
            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
                fail_message = "match range"
            else:
                last_action = _ConstantSemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "LIST_START":
            envs.append([])
            pc += 1
            continue
        elif name == "LIST_END":
            last_action = _SemanticAction(lambda xs: [x.eval() for x in xs], envs.pop())
            pc += 1
            continue
        elif name == "MATCH_ANY":
            if pos >= len(stream):
                fail_message = "match any"
            else:
                last_action = _ConstantSemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "PUSH_STREAM":
            if pos >= len(stream) or not isinstance(stream[pos], list):
                fail_message = "push stream"
            else:
                stream_pos_stack.append((stream, pos+1))
                stream = stream[pos]
                pos = 0
                pc += 1
                continue
        elif name == "POP_STREAM":
            if pos < len(stream):
                fail_message = "pop stream"
            else:
                stream, pos = stream_pos_stack.pop()
                pc += 1
                continue
        elif name == "MATCH_CALL_RULE":
            if pos >= len(stream):
                fail_message = "match call rule"
            else:
                fn_name = str(stream[pos])
                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
                if key in memo:
                    last_action, stream_pos_stack = memo[key]
                    stream_pos_stack = stream_pos_stack[:]
                    stream, pos = stream_pos_stack.pop()
                    pc += 1
                else:
                    stack.append((pc+1, key))
                    pc = labels[fn_name]
                    pos += 1
                continue
        elif name == "FAIL":
            fail_message = "fail"
        elif name == "LABEL":
            last_action = _ConstantSemanticAction(label_counter)
            label_counter += 1
            pc += 1
            continue
        elif name == "MATCH_STRING":
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
        if len(stream_pos_stack) > stream_stack_len:
            stream = stream_pos_stack[stream_stack_len][0]
        stream_pos_stack = stream_pos_stack[:stream_stack_len]
        envs = envs[:envs_len]

class _SemanticAction(object):

    def __init__(self, fn, value):
        self.fn = fn
        self.value = value

    def eval(self):
        return self.fn(self.value)

class _ConstantSemanticAction(object):

    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value

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
