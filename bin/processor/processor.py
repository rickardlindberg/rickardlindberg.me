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
        self.latest_fail_message, self.latest_fail_pos = (None, tuple())
        self.memo = {}
        while True:
            result = self.pop_arg()(self)
            if result:
                return result

    def pop_arg(self):
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
        vm.pop_arg(), vm.stream, vm.stream_rest, vm.pos, vm.pos_rest, vm.scope, vm.scope_rest
    ))

def COMMIT(vm):
    vm.call_backtrack_stack.pop()
    vm.pc = vm.pop_arg()

def CALL(vm):
    CALL_(vm, vm.pop_arg())

def CALL_(vm, pc):
    key = (pc, vm.pos_rest+(vm.pos,))
    if key in vm.memo:
        if vm.memo[key][0] is None:
            FAIL_(vm, vm.memo[key][1])
        else:
            vm.action, vm.stream, vm.stream_rest, vm.pos, vm.pos_rest = vm.memo[key]
    else:
        vm.call_backtrack_stack.append((vm.pc, key))
        vm.pc = pc

def RETURN(vm):
    if not vm.call_backtrack_stack:
        return vm.action
    vm.pc, key = vm.call_backtrack_stack.pop()
    vm.memo[key] = (vm.action, vm.stream, vm.stream_rest, vm.pos, vm.pos_rest)

def MATCH(vm):
    object_description = vm.pop_arg()
    fn = vm.pop_arg()
    MATCH_(vm, fn, ("expected {}", object_description))

def MATCH_(vm, fn, message):
    if vm.pos >= len(vm.stream) or not fn(vm.stream[vm.pos]):
        FAIL_(vm, message)
    else:
        vm.action = SemanticAction(vm.stream[vm.pos])
        vm.pos += 1
        return True

def MATCH_CALL_RULE(vm):
    if MATCH_(vm, lambda x: x in vm.rules, ("expected rule name",)):
        CALL_(vm, vm.rules[vm.action.value])

def LIST_START(vm):
    vm.scope_rest = (vm.scope, vm.scope_rest)
    vm.scope = []

def LIST_APPEND(vm):
    vm.scope.append(vm.action)

def LIST_END(vm):
    vm.action = SemanticAction(vm.scope, lambda self: [x.eval(self.runtime) for x in self.value])
    vm.scope, vm.scope_rest = vm.scope_rest

def BIND(vm):
    vm.scope[vm.pop_arg()] = vm.action

def ACTION(vm):
    vm.action = SemanticAction(vm.scope, vm.pop_arg())

def PUSH_STREAM(vm):
    if vm.pos >= len(vm.stream) or not isinstance(vm.stream[vm.pos], list):
        FAIL_(vm, ("expected list",))
    else:
        vm.stream_rest = (vm.stream, vm.stream_rest)
        vm.pos_rest = vm.pos_rest + (vm.pos,)
        vm.stream = vm.stream[vm.pos]
        vm.pos = 0

def POP_STREAM(vm):
    if vm.pos < len(vm.stream):
        FAIL_(vm, ("expected end of list",))
    else:
        vm.stream, vm.stream_rest = vm.stream_rest
        vm.pos, vm.pos_rest = vm.pos_rest[-1], vm.pos_rest[:-1]
        vm.pos += 1

def FAIL(vm):
    FAIL_(vm, (vm.pop_arg(),))

def FAIL_(vm, fail_message):
    fail_pos = vm.pos_rest+(vm.pos,)
    if fail_pos >= vm.latest_fail_pos:
        vm.latest_fail_message = fail_message
        vm.latest_fail_pos = fail_pos
    call_backtrack_entry = tuple()
    while vm.call_backtrack_stack:
        call_backtrack_entry = vm.call_backtrack_stack.pop()
        if len(call_backtrack_entry) == 7:
            break
        else:
            vm.memo[call_backtrack_entry[1]] = (None, fail_message)
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
            "add": lambda x, y: x.append(y),
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

def compile_chain(grammars, source, runtime={}):
    import os
    import sys
    import pprint
    for grammar, rule in grammars:
        try:
            source = grammar().run(rule, source, runtime)
        except MatchError as e:
            marker = "<ERROR POSITION>"
            if os.isatty(sys.stderr.fileno()):
                marker = f"\033[0;31m{marker}\033[0m"
            if isinstance(e.stream, str):
                stream_string = e.stream[:e.pos] + marker + e.stream[e.pos:]
            else:
                stream_string = pprint.pformat(e.stream)
            sys.exit("ERROR: {}\nPOSITION: {}\nSTREAM:\n{}".format(
                e.message,
                e.pos,
                indent(stream_string)
            ))
    return source
class Processor(Grammar):
    rules = {
        'file': 0,
        'header': 29,
        'keys': 42,
        'sep': 58,
        'key': 73,
        'commasep': 155,
        'item': 164,
        'delimiter': 191,
        'block': 215,
        'outputLines': 376,
        'line': 438,
        'linex': 450
    }
    code = [
        PUSH_SCOPE,
        CALL,
        29,
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        13,
        CALL,
        215,
        LIST_APPEND,
        COMMIT,
        6,
        LIST_END,
        BIND,
        'xs',
        BACKTRACK,
        25,
        MATCH,
        'any',
        lambda x: True,
        COMMIT,
        23,
        FAIL,
        'no match',
        ACTION,
        lambda self: join([self.lookup('pre')(self.lookup('x')), self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        58,
        CALL,
        42,
        BIND,
        'x',
        CALL,
        58,
        ACTION,
        lambda self: self.bind('keys', self.lookup('dict')(), lambda: self.bind('', self.lookup('x'), lambda: self.lookup('keys'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        51,
        CALL,
        73,
        LIST_APPEND,
        COMMIT,
        44,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: self.lookup('xs'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH,
        '-',
        lambda x: x == '-',
        MATCH,
        '-',
        lambda x: x == '-',
        MATCH,
        '-',
        lambda x: x == '-',
        MATCH,
        '\n',
        lambda x: x == '\n',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        106,
        PUSH_SCOPE,
        MATCH,
        't',
        lambda x: x == 't',
        MATCH,
        'a',
        lambda x: x == 'a',
        MATCH,
        'g',
        lambda x: x == 'g',
        MATCH,
        's',
        lambda x: x == 's',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        ' ',
        lambda x: x == ' ',
        CALL,
        155,
        BIND,
        'x',
        MATCH,
        '\n',
        lambda x: x == '\n',
        ACTION,
        lambda self: self.lookup('set')(self.lookup('keys'), 'tags', self.lookup('x')),
        POP_SCOPE,
        COMMIT,
        154,
        PUSH_SCOPE,
        BACKTRACK,
        115,
        CALL,
        58,
        COMMIT,
        113,
        FAIL,
        'no match',
        LIST_START,
        BACKTRACK,
        135,
        PUSH_SCOPE,
        BACKTRACK,
        128,
        MATCH,
        ':',
        lambda x: x == ':',
        COMMIT,
        126,
        FAIL,
        'no match',
        MATCH,
        'any',
        lambda x: True,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        116,
        LIST_END,
        BIND,
        'xs',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        ' ',
        lambda x: x == ' ',
        CALL,
        450,
        BIND,
        'y',
        MATCH,
        '\n',
        lambda x: x == '\n',
        ACTION,
        lambda self: self.lookup('set')(self.lookup('keys'), join([self.lookup('xs')]), self.lookup('y')),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        164,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        184,
        PUSH_SCOPE,
        BACKTRACK,
        177,
        CALL,
        191,
        COMMIT,
        175,
        FAIL,
        'no match',
        MATCH,
        'any',
        lambda x: True,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        166,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: join([self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        200,
        PUSH_SCOPE,
        MATCH,
        ',',
        lambda x: x == ',',
        POP_SCOPE,
        COMMIT,
        214,
        BACKTRACK,
        209,
        PUSH_SCOPE,
        MATCH,
        ' ',
        lambda x: x == ' ',
        POP_SCOPE,
        COMMIT,
        214,
        PUSH_SCOPE,
        MATCH,
        '\n',
        lambda x: x == '\n',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        248,
        PUSH_SCOPE,
        MATCH,
        '$',
        lambda x: x == '$',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        'c',
        lambda x: x == 'c',
        MATCH,
        'o',
        lambda x: x == 'o',
        MATCH,
        'd',
        lambda x: x == 'd',
        MATCH,
        'e',
        lambda x: x == 'e',
        MATCH,
        ':',
        lambda x: x == ':',
        CALL,
        450,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('code')(self.lookup('x')),
        POP_SCOPE,
        COMMIT,
        375,
        BACKTRACK,
        328,
        PUSH_SCOPE,
        MATCH,
        '$',
        lambda x: x == '$',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        'o',
        lambda x: x == 'o',
        MATCH,
        'u',
        lambda x: x == 'u',
        MATCH,
        't',
        lambda x: x == 't',
        MATCH,
        'p',
        lambda x: x == 'p',
        MATCH,
        'u',
        lambda x: x == 'u',
        MATCH,
        't',
        lambda x: x == 't',
        MATCH,
        ':',
        lambda x: x == ':',
        LIST_START,
        BACKTRACK,
        309,
        PUSH_SCOPE,
        BACKTRACK,
        302,
        BACKTRACK,
        293,
        PUSH_SCOPE,
        MATCH,
        ':',
        lambda x: x == ':',
        POP_SCOPE,
        COMMIT,
        298,
        PUSH_SCOPE,
        MATCH,
        '\n',
        lambda x: x == '\n',
        POP_SCOPE,
        COMMIT,
        300,
        FAIL,
        'no match',
        MATCH,
        'any',
        lambda x: True,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        279,
        LIST_END,
        BIND,
        'syntax',
        MATCH,
        ':',
        lambda x: x == ':',
        CALL,
        438,
        BIND,
        'title',
        CALL,
        376,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('output')(self.lookup('title'), self.lookup('x'), join([self.lookup('syntax')])),
        POP_SCOPE,
        COMMIT,
        375,
        BACKTRACK,
        371,
        PUSH_SCOPE,
        MATCH,
        '$',
        lambda x: x == '$',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        'o',
        lambda x: x == 'o',
        MATCH,
        'u',
        lambda x: x == 'u',
        MATCH,
        't',
        lambda x: x == 't',
        MATCH,
        'p',
        lambda x: x == 'p',
        MATCH,
        'u',
        lambda x: x == 'u',
        MATCH,
        't',
        lambda x: x == 't',
        MATCH,
        ':',
        lambda x: x == ':',
        CALL,
        438,
        BIND,
        'title',
        CALL,
        376,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('output')(self.lookup('title'), self.lookup('x')),
        POP_SCOPE,
        COMMIT,
        375,
        PUSH_SCOPE,
        CALL,
        438,
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        413,
        PUSH_SCOPE,
        BACKTRACK,
        407,
        PUSH_SCOPE,
        MATCH,
        '$',
        lambda x: x == '$',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        'E',
        lambda x: x == 'E',
        MATCH,
        'N',
        lambda x: x == 'N',
        MATCH,
        'D',
        lambda x: x == 'D',
        MATCH,
        '\n',
        lambda x: x == '\n',
        POP_SCOPE,
        COMMIT,
        405,
        FAIL,
        'no match',
        CALL,
        438,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        378,
        LIST_END,
        BIND,
        'xs',
        MATCH,
        '$',
        lambda x: x == '$',
        MATCH,
        ':',
        lambda x: x == ':',
        MATCH,
        'E',
        lambda x: x == 'E',
        MATCH,
        'N',
        lambda x: x == 'N',
        MATCH,
        'D',
        lambda x: x == 'D',
        MATCH,
        '\n',
        lambda x: x == '\n',
        ACTION,
        lambda self: join([self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        450,
        BIND,
        'x',
        MATCH,
        '\n',
        lambda x: x == '\n',
        ACTION,
        lambda self: join([self.lookup('x'), '\n']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        471,
        PUSH_SCOPE,
        BACKTRACK,
        464,
        MATCH,
        '\n',
        lambda x: x == '\n',
        COMMIT,
        462,
        FAIL,
        'no match',
        MATCH,
        'any',
        lambda x: True,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        452,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: join([self.lookup('xs')]),
        POP_SCOPE,
        RETURN
    ]
def pre(header):
    lines = []
    lines.append("---\n")
    if "draft" in header.get("tags", []):
        header["title"] = repr(f"DRAFT: {header['title']}")
        header["date"] = datetime.datetime.now().isoformat()[:10]
    for key, value in header.items():
        if isinstance(value, list):
            lines.append(f"{key}: {','.join(value)}\n")
        else:
            lines.append(f"{key}: {value}\n")
    lines.append("---\n")
    if "draft" in header.get("tags", []):
        lines.append("\n")
        lines.append("**This is a work in progress that will change. Like to see it finished? Let me know by sending me an email.**\n")
    return lines

def code(path, start=None, end=None):
    pygments_cmd = ["pygmentize"]
    language = ""
    if path.endswith(".rlmeta"):
        pygments_cmd.extend(["-l", "rlmeta_lexer.py:RLMetaLexer", "-x"])
    else:
        language = os.path.splitext(path)[1][1:]
        pygments_cmd.extend(["-l", language])
    pygments_cmd.extend(["-f", "html"])
    with open(path) as f:
        lines = f.read().splitlines(True)
    if start is not None:
        while lines and not re.search(start, lines[0]):
            lines.pop(0)
    if end is not None and lines:
        keep = [lines.pop(0)]
        while lines and not re.search(end, lines[0]):
            keep.append(lines.pop(0))
        lines = keep
    joined = "".join(lines)
    if start is not None or end is not None:
        joined = textwrap.dedent(joined)
    assert joined.endswith("\n")
    return f"```{language}\n{joined}```\n"

def output(title, text, syntax="text"):
    pygments_cmd = ["pygmentize"]
    pygments_cmd.extend(["-f", "html"])
    pygments_cmd.extend(["-l", syntax])
    joined = text
    assert joined.endswith("\n")
    title_code = []
    if title.strip():
        title_text = f"`{title.strip()}`:\n\n"
    else:
        title_text = ""
    if syntax == "text":
        language = ""
    else:
        language = syntax
    return f"{title_text}```{language}\n{joined}```\n"

if __name__ == "__main__":
    import sys
    import datetime
    import os
    import subprocess
    for path in sys.argv[1:]:
        if path == "index.template.markdown":
            source_path = path
            destination_path = "index.markdown"
        else:
            source_path = path+".template"
            destination_path = path
        with open(source_path) as source:
            with open(destination_path, "w") as dest:
                dest.write(compile_chain(
                    [(Processor, "file")],
                    source.read(),
                    {"pre": pre, "code": code, "output": output}
                ))
