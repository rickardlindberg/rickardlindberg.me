class VM:

    def __init__(self, code, rules):
        self.code = code
        self.rules = rules

    def run(self, start_rule, stream):
        self.action = SemanticAction(None)
        self.pc = self.rules[start_rule]
        self.call_backtrack_stack = []
        self.stream, self.stream_rest = (stream, None)
        self.pos, self.pos_rest = (0, tuple())
        self.scope, self.scope_rest = (None, None)
        self.fail_message = None
        self.latest_fail_message, self.latest_fail_pos = (None, tuple())
        self.memo = {}
        while True:
            result = self.pop_code()(self)
            if result:
                return result

    def pop_code(self):
        code = self.code[self.pc]
        self.pc += 1
        return code

def PUSH_SCOPE(vm):
    vm.scope_rest = (vm.scope, vm.scope_rest)
    vm.scope = {}

def POP_SCOPE(vm):
    vm.scope, vm.scope_rest = vm.scope_rest

def BACKTRACK(vm):
    vm.call_backtrack_stack.append((
        vm.pop_code(), vm.stream, vm.stream_rest, vm.pos, vm.pos_rest, vm.scope, vm.scope_rest
    ))

def COMMIT(vm):
    vm.call_backtrack_stack.pop()
    vm.pc = vm.pop_code()

def CALL(vm):
    CALL_(vm, vm.pop_code())

def MATCH_CALL_RULE(vm):
    if vm.pos >= len(vm.stream):
        vm.fail_message = ("expected any",)
        FAIL(vm)
    else:
        x = str(vm.stream[vm.pos])
        vm.pos += 1
        CALL_(vm, vm.rules[x])

def CALL_(vm, pc):
    key = (pc, vm.pos_rest+(vm.pos,))
    if key in vm.memo:
        if vm.memo[key][0] is None:
            vm.fail_message = vm.memo[key][1]
            FAIL(vm)
        else:
            vm.action, vm.stream, vm.stream_rest, vm.pos, vm.pos_rest = vm.memo[key]
    else:
        vm.call_backtrack_stack.append((vm.pc, key))
        vm.pc = pc

def MATCH_OBJECT(vm):
    arg_object = vm.pop_code()
    if vm.pos >= len(vm.stream) or vm.stream[vm.pos] != arg_object:
        vm.fail_message = ("expected {!r}", arg_object)
        FAIL(vm)
    else:
        vm.action = SemanticAction(arg_object)
        vm.pos += 1

def RETURN(vm):
    if not vm.call_backtrack_stack:
        return vm.action
    vm.pc, key = vm.call_backtrack_stack.pop()
    vm.memo[key] = (vm.action, vm.stream, vm.stream_rest, vm.pos, vm.pos_rest)

def LIST_APPEND(vm):
    vm.scope.append(vm.action)

def BIND(vm):
    vm.scope[vm.pop_code()] = vm.action

def ACTION(vm):
    vm.action = SemanticAction(vm.scope, vm.pop_code())

def MATCH_RANGE(vm):
    arg_start = vm.pop_code()
    arg_end = vm.pop_code()
    if vm.pos >= len(vm.stream) or not (arg_start <= vm.stream[vm.pos] <= arg_end):
        vm.fail_message = ("expected range {!r}-{!r}", arg_start, arg_end)
        FAIL(vm)
    else:
        vm.action = SemanticAction(vm.stream[vm.pos])
        vm.pos += 1

def LIST_START(vm):
    vm.scope_rest = (vm.scope, vm.scope_rest)
    vm.scope = []

def LIST_END(vm):
    vm.action = SemanticAction(vm.scope, lambda self: [x.eval(self.runtime) for x in self.value])
    vm.scope, vm.scope_rest = vm.scope_rest

def MATCH_ANY(vm):
    if vm.pos >= len(vm.stream):
        vm.fail_message = ("expected any",)
        FAIL(vm)
    else:
        vm.action = SemanticAction(vm.stream[vm.pos])
        vm.pos += 1

def PUSH_STREAM(vm):
    if vm.pos >= len(vm.stream) or not isinstance(vm.stream[vm.pos], list):
        vm.fail_message = ("expected list",)
        FAIL(vm)
    else:
        vm.stream_rest = (vm.stream, vm.stream_rest)
        vm.pos_rest = vm.pos_rest + (vm.pos,)
        vm.stream = vm.stream[vm.pos]
        vm.pos = 0

def POP_STREAM(vm):
    if vm.pos < len(vm.stream):
        vm.fail_message = ("expected end of list",)
        FAIL(vm)
    else:
        vm.stream, vm.stream_rest = vm.stream_rest
        vm.pos, vm.pos_rest = vm.pos_rest[-1], vm.pos_rest[:-1]
        vm.pos += 1

def FAIL(vm):
    vm.fail_message = (vm.pop_code(),)
    FAIL(vm)

def FAIL(vm):
    fail_pos = vm.pos_rest+(vm.pos,)
    if fail_pos >= vm.latest_fail_pos:
        vm.latest_fail_message = vm.fail_message
        vm.latest_fail_pos = fail_pos
    call_backtrack_entry = tuple()
    while vm.call_backtrack_stack:
        call_backtrack_entry = vm.call_backtrack_stack.pop()
        if len(call_backtrack_entry) == 7:
            break
        else:
            vm.memo[call_backtrack_entry[1]] = (None, vm.fail_message)
    if len(call_backtrack_entry) != 7:
        raise MatchError(
            vm.latest_fail_message[0].format(*vm.latest_fail_message[1:]),
            vm.latest_fail_pos[-1],
            vm.stream
        )
    (vm.pc, vm.stream, vm.stream_rest, vm.pos, vm.pos_rest, vm.scope, vm.scope_rest) = call_backtrack_entry

class SemanticAction(object):

    def __init__(self, value, fn=lambda self: self.value):
        self.value = value
        self.fn = fn

    def eval(self, runtime):
        self.runtime = runtime
        return self.fn(self)

    def bind(self, name, value, continuation):
        self.runtime = self.runtime.set(name, value)
        return continuation()

    def lookup(self, name):
        if name in self.value:
            return self.value[name].eval(self.runtime)
        else:
            return self.runtime[name]

class MatchError(Exception):

    def __init__(self, message, pos, stream):
        Exception.__init__(self)
        self.message = message
        self.pos = pos
        self.stream = stream

class Grammar(object):

    def run(self, rule, stream, runtime={}):
        return Runtime(self, dict(runtime, **{
            "label": Counter(),
            "indentprefix": "    ",
            "list": list,
            "dict": dict,
            "append": lambda x, y: x.append(y),
            "get": lambda x, y: x[y],
            "set": lambda x, y, z: x.__setitem__(y, z),
            "len": len,
            "repr": repr,
            "join": join,
        })).run(rule, stream)

class Runtime(dict):

    def __init__(self, grammar, values):
        dict.__init__(self, dict(values, run=self.run))
        self.grammar = grammar

    def set(self, key, value):
        return Runtime(self.grammar, dict(self, **{key: value}))

    def run(self, rule, stream):
        return VM(self.grammar.code, self.grammar.rules).run(rule, stream).eval(self)
        return vm(self.grammar.code, self.grammar.rules, rule, stream).eval(self)

class Counter(object):

    def __init__(self):
        self.value = 0

    def __call__(self):
        result = self.value
        self.value += 1
        return result

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

def indent(text, prefix="    "):
    return "".join(prefix+line for line in text.splitlines(True))

def compile_chain(grammars, source):
    import sys
    import pprint
    for grammar, rule in grammars:
        try:
            source = grammar().run(rule, source)
        except MatchError as e:
            MARKER = "\033[0;31m<ERROR POSITION>\033[0m"
            if isinstance(e.stream, str):
                stream_string = e.stream[:e.pos] + MARKER + e.stream[e.pos:]
            else:
                stream_string = pprint.pformat(e.stream)
            sys.exit("ERROR: {}\nPOSITION: {}\nSTREAM:\n{}".format(
                e.message,
                e.pos,
                indent(stream_string)
            ))
    return source
