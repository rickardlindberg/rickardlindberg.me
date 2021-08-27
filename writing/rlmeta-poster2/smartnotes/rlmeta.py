SUPPORT = 'PUSH_SCOPE = 0\nBACKTRACK = 1\nCALL = 2\nPOP_SCOPE = 3\nMATCH_OBJECT = 4\nCOMMIT = 5\nRETURN = 6\nLIST_APPEND = 7\nBIND = 8\nACTION = 9\nMATCH_RANGE = 10\nLIST_START = 11\nLIST_END = 12\nMATCH_ANY = 13\nPUSH_STREAM = 14\nPOP_STREAM = 15\nMATCH_CALL_RULE = 16\nFAIL = 17\ndef vm(code, rules, start_rule, stream):\n    action = SemanticAction(None)\n    pc = rules[start_rule]\n    call_backtrack_stack = []\n    stream, stream_rest = (stream, None)\n    pos, pos_rest = (0, tuple())\n    scope, scope_rest = (None, None)\n    fail_message = None\n    latest_fail_message, latest_fail_pos = (None, tuple())\n    memo = {}\n    while True:\n        opcode = code[pc]\n        pc += 1\n        if opcode == PUSH_SCOPE:\n            scope_rest = (scope, scope_rest)\n            scope = {}\n            continue\n        elif opcode == BACKTRACK:\n            arg_pc = code[pc]\n            pc += 1\n            call_backtrack_stack.append((\n                arg_pc, stream, stream_rest, pos, pos_rest, scope, scope_rest\n            ))\n            continue\n        elif opcode == CALL:\n            arg_fn_pc = code[pc]\n            pc += 1\n            key = (arg_fn_pc, pos_rest+(pos,))\n            if key in memo:\n                if memo[key][0] is None:\n                    fail_message = memo[key][1]\n                    fail_pos = pos_rest+(pos,)\n                    if fail_pos >= latest_fail_pos:\n                        latest_fail_message = fail_message\n                        latest_fail_pos = fail_pos\n                    call_backtrack_entry = tuple()\n                    while call_backtrack_stack:\n                        call_backtrack_entry = call_backtrack_stack.pop()\n                        if len(call_backtrack_entry) == 7:\n                            break\n                        else:\n                            _, key = call_backtrack_entry\n                            memo[key] = (None, fail_message)\n                    if len(call_backtrack_entry) != 7:\n                        raise MatchError(\n                            latest_fail_message[0].format(*latest_fail_message[1:]),\n                            latest_fail_pos[-1],\n                            stream\n                        )\n                    (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n                else:\n                    action, stream, stream_rest, pos, pos_rest = memo[key]\n            else:\n                call_backtrack_stack.append((pc, key))\n                pc = arg_fn_pc\n            continue\n        elif opcode == POP_SCOPE:\n            scope, scope_rest = scope_rest\n            continue\n        elif opcode == MATCH_OBJECT:\n            arg_object = code[pc]\n            pc += 1\n            if pos >= len(stream) or stream[pos] != arg_object:\n                fail_message = ("expected {!r}", arg_object)\n                fail_pos = pos_rest+(pos,)\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 7:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 7:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos[-1],\n                        stream\n                    )\n                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            else:\n                action = SemanticAction(arg_object)\n                pos += 1\n            continue\n        elif opcode == COMMIT:\n            arg_pc = code[pc]\n            pc += 1\n            call_backtrack_stack.pop()\n            pc = arg_pc\n            continue\n        elif opcode == RETURN:\n            if not call_backtrack_stack:\n                return action\n            pc, key = call_backtrack_stack.pop()\n            memo[key] = (action, stream, stream_rest, pos, pos_rest)\n            continue\n        elif opcode == LIST_APPEND:\n            scope.append(action)\n            continue\n        elif opcode == BIND:\n            arg_name = code[pc]\n            pc += 1\n            scope[arg_name] = action\n            continue\n        elif opcode == ACTION:\n            arg_fn = code[pc]\n            pc += 1\n            action = SemanticAction(scope, arg_fn)\n            continue\n        elif opcode == MATCH_RANGE:\n            arg_start = code[pc]\n            pc += 1\n            arg_end = code[pc]\n            pc += 1\n            if pos >= len(stream) or not (arg_start <= stream[pos] <= arg_end):\n                fail_message = ("expected range {!r}-{!r}", arg_start, arg_end)\n                fail_pos = pos_rest+(pos,)\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 7:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 7:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos[-1],\n                        stream\n                    )\n                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n            continue\n        elif opcode == LIST_START:\n            scope_rest = (scope, scope_rest)\n            scope = []\n            continue\n        elif opcode == LIST_END:\n            action = SemanticAction(scope, lambda self: [x.eval(self.runtime) for x in self.value])\n            scope, scope_rest = scope_rest\n            continue\n        elif opcode == MATCH_ANY:\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n                fail_pos = pos_rest+(pos,)\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 7:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 7:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos[-1],\n                        stream\n                    )\n                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n            continue\n        elif opcode == PUSH_STREAM:\n            if pos >= len(stream) or not isinstance(stream[pos], list):\n                fail_message = ("expected list",)\n                fail_pos = pos_rest+(pos,)\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 7:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 7:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos[-1],\n                        stream\n                    )\n                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            else:\n                stream_rest = (stream, stream_rest)\n                pos_rest = pos_rest + (pos,)\n                stream = stream[pos]\n                pos = 0\n            continue\n        elif opcode == POP_STREAM:\n            if pos < len(stream):\n                fail_message = ("expected end of list",)\n                fail_pos = pos_rest+(pos,)\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 7:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 7:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos[-1],\n                        stream\n                    )\n                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            else:\n                stream, stream_rest = stream_rest\n                pos, pos_rest = pos_rest[-1], pos_rest[:-1]\n                pos += 1\n            continue\n        elif opcode == MATCH_CALL_RULE:\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n                fail_pos = pos_rest+(pos,)\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 7:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 7:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos[-1],\n                        stream\n                    )\n                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            else:\n                arg_fn_pc = rules[str(stream[pos])]\n                pos += 1\n                key = (arg_fn_pc, pos_rest+(pos,))\n                if key in memo:\n                    if memo[key][0] is None:\n                        fail_message = memo[key][1]\n                        fail_pos = pos_rest+(pos,)\n                        if fail_pos >= latest_fail_pos:\n                            latest_fail_message = fail_message\n                            latest_fail_pos = fail_pos\n                        call_backtrack_entry = tuple()\n                        while call_backtrack_stack:\n                            call_backtrack_entry = call_backtrack_stack.pop()\n                            if len(call_backtrack_entry) == 7:\n                                break\n                            else:\n                                _, key = call_backtrack_entry\n                                memo[key] = (None, fail_message)\n                        if len(call_backtrack_entry) != 7:\n                            raise MatchError(\n                                latest_fail_message[0].format(*latest_fail_message[1:]),\n                                latest_fail_pos[-1],\n                                stream\n                            )\n                        (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n                    else:\n                        action, stream, stream_rest, pos, pos_rest = memo[key]\n                else:\n                    call_backtrack_stack.append((pc, key))\n                    pc = arg_fn_pc\n            continue\n        elif opcode == FAIL:\n            arg_message = code[pc]\n            pc += 1\n            fail_message = (arg_message,)\n            fail_pos = pos_rest+(pos,)\n            if fail_pos >= latest_fail_pos:\n                latest_fail_message = fail_message\n                latest_fail_pos = fail_pos\n            call_backtrack_entry = tuple()\n            while call_backtrack_stack:\n                call_backtrack_entry = call_backtrack_stack.pop()\n                if len(call_backtrack_entry) == 7:\n                    break\n                else:\n                    _, key = call_backtrack_entry\n                    memo[key] = (None, fail_message)\n            if len(call_backtrack_entry) != 7:\n                raise MatchError(\n                    latest_fail_message[0].format(*latest_fail_message[1:]),\n                    latest_fail_pos[-1],\n                    stream\n                )\n            (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry\n            continue\n        else:\n            raise Exception("unknown opcode {}".format(opcode))\nclass SemanticAction(object):\n\n    def __init__(self, value, fn=lambda self: self.value):\n        self.value = value\n        self.fn = fn\n\n    def eval(self, runtime):\n        self.runtime = runtime\n        return self.fn(self)\n\n    def bind(self, name, value, continuation):\n        self.runtime = self.runtime.set(name, value)\n        return continuation()\n\n    def lookup(self, name):\n        if name in self.value:\n            return self.value[name].eval(self.runtime)\n        else:\n            return self.runtime[name]\n\nclass MatchError(Exception):\n\n    def __init__(self, message, pos, stream):\n        Exception.__init__(self)\n        self.message = message\n        self.pos = pos\n        self.stream = stream\n\nclass Grammar(object):\n\n    def run(self, rule, stream, runtime={}):\n        return Runtime(self, dict(runtime, **{\n            "label": Counter(),\n            "indentprefix": "    ",\n            "list": list,\n            "dict": dict,\n            "append": lambda x, y: x.append(y),\n            "get": lambda x, y: x[y],\n            "set": lambda x, y, z: x.__setitem__(y, z),\n            "len": len,\n            "repr": repr,\n            "join": join,\n        })).run(rule, stream)\n\nclass Runtime(dict):\n\n    def __init__(self, grammar, values):\n        dict.__init__(self, dict(values, run=self.run))\n        self.grammar = grammar\n\n    def set(self, key, value):\n        return Runtime(self.grammar, dict(self, **{key: value}))\n\n    def run(self, rule, stream):\n        return vm(self.grammar.code, self.grammar.rules, rule, stream).eval(self)\n\nclass Counter(object):\n\n    def __init__(self):\n        self.value = 0\n\n    def __call__(self):\n        result = self.value\n        self.value += 1\n        return result\n\ndef splice(depth, item):\n    if depth == 0:\n        return [item]\n    else:\n        return concat([splice(depth-1, subitem) for subitem in item])\n\ndef concat(lists):\n    return [x for xs in lists for x in xs]\n\ndef join(items, delimiter=""):\n    return delimiter.join(\n        join(item, delimiter) if isinstance(item, list) else str(item)\n        for item in items\n    )\n\ndef indent(text, prefix="    "):\n    return "".join(prefix+line for line in text.splitlines(True))\n\ndef compile_chain(grammars, source):\n    import sys\n    import pprint\n    for grammar, rule in grammars:\n        try:\n            source = grammar().run(rule, source)\n        except MatchError as e:\n            MARKER = "\\033[0;31m<ERROR POSITION>\\033[0m"\n            if isinstance(e.stream, str):\n                stream_string = e.stream[:e.pos] + MARKER + e.stream[e.pos:]\n            else:\n                stream_string = pprint.pformat(e.stream)\n            sys.exit("ERROR: {}\\nPOSITION: {}\\nSTREAM:\\n{}".format(\n                e.message,\n                e.pos,\n                indent(stream_string)\n            ))\n    return source\n'
PUSH_SCOPE = 0
BACKTRACK = 1
CALL = 2
POP_SCOPE = 3
MATCH_OBJECT = 4
COMMIT = 5
RETURN = 6
LIST_APPEND = 7
BIND = 8
ACTION = 9
MATCH_RANGE = 10
LIST_START = 11
LIST_END = 12
MATCH_ANY = 13
PUSH_STREAM = 14
POP_STREAM = 15
MATCH_CALL_RULE = 16
FAIL = 17
def vm(code, rules, start_rule, stream):
    action = SemanticAction(None)
    pc = rules[start_rule]
    call_backtrack_stack = []
    stream, stream_rest = (stream, None)
    pos, pos_rest = (0, tuple())
    scope, scope_rest = (None, None)
    fail_message = None
    latest_fail_message, latest_fail_pos = (None, tuple())
    memo = {}
    while True:
        opcode = code[pc]
        pc += 1
        if opcode == PUSH_SCOPE:
            scope_rest = (scope, scope_rest)
            scope = {}
            continue
        elif opcode == BACKTRACK:
            arg_pc = code[pc]
            pc += 1
            call_backtrack_stack.append((
                arg_pc, stream, stream_rest, pos, pos_rest, scope, scope_rest
            ))
            continue
        elif opcode == CALL:
            arg_fn_pc = code[pc]
            pc += 1
            key = (arg_fn_pc, pos_rest+(pos,))
            if key in memo:
                if memo[key][0] is None:
                    fail_message = memo[key][1]
                    fail_pos = pos_rest+(pos,)
                    if fail_pos >= latest_fail_pos:
                        latest_fail_message = fail_message
                        latest_fail_pos = fail_pos
                    call_backtrack_entry = tuple()
                    while call_backtrack_stack:
                        call_backtrack_entry = call_backtrack_stack.pop()
                        if len(call_backtrack_entry) == 7:
                            break
                        else:
                            _, key = call_backtrack_entry
                            memo[key] = (None, fail_message)
                    if len(call_backtrack_entry) != 7:
                        raise MatchError(
                            latest_fail_message[0].format(*latest_fail_message[1:]),
                            latest_fail_pos[-1],
                            stream
                        )
                    (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
                else:
                    action, stream, stream_rest, pos, pos_rest = memo[key]
            else:
                call_backtrack_stack.append((pc, key))
                pc = arg_fn_pc
            continue
        elif opcode == POP_SCOPE:
            scope, scope_rest = scope_rest
            continue
        elif opcode == MATCH_OBJECT:
            arg_object = code[pc]
            pc += 1
            if pos >= len(stream) or stream[pos] != arg_object:
                fail_message = ("expected {!r}", arg_object)
                fail_pos = pos_rest+(pos,)
                if fail_pos >= latest_fail_pos:
                    latest_fail_message = fail_message
                    latest_fail_pos = fail_pos
                call_backtrack_entry = tuple()
                while call_backtrack_stack:
                    call_backtrack_entry = call_backtrack_stack.pop()
                    if len(call_backtrack_entry) == 7:
                        break
                    else:
                        _, key = call_backtrack_entry
                        memo[key] = (None, fail_message)
                if len(call_backtrack_entry) != 7:
                    raise MatchError(
                        latest_fail_message[0].format(*latest_fail_message[1:]),
                        latest_fail_pos[-1],
                        stream
                    )
                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            else:
                action = SemanticAction(arg_object)
                pos += 1
            continue
        elif opcode == COMMIT:
            arg_pc = code[pc]
            pc += 1
            call_backtrack_stack.pop()
            pc = arg_pc
            continue
        elif opcode == RETURN:
            if not call_backtrack_stack:
                return action
            pc, key = call_backtrack_stack.pop()
            memo[key] = (action, stream, stream_rest, pos, pos_rest)
            continue
        elif opcode == LIST_APPEND:
            scope.append(action)
            continue
        elif opcode == BIND:
            arg_name = code[pc]
            pc += 1
            scope[arg_name] = action
            continue
        elif opcode == ACTION:
            arg_fn = code[pc]
            pc += 1
            action = SemanticAction(scope, arg_fn)
            continue
        elif opcode == MATCH_RANGE:
            arg_start = code[pc]
            pc += 1
            arg_end = code[pc]
            pc += 1
            if pos >= len(stream) or not (arg_start <= stream[pos] <= arg_end):
                fail_message = ("expected range {!r}-{!r}", arg_start, arg_end)
                fail_pos = pos_rest+(pos,)
                if fail_pos >= latest_fail_pos:
                    latest_fail_message = fail_message
                    latest_fail_pos = fail_pos
                call_backtrack_entry = tuple()
                while call_backtrack_stack:
                    call_backtrack_entry = call_backtrack_stack.pop()
                    if len(call_backtrack_entry) == 7:
                        break
                    else:
                        _, key = call_backtrack_entry
                        memo[key] = (None, fail_message)
                if len(call_backtrack_entry) != 7:
                    raise MatchError(
                        latest_fail_message[0].format(*latest_fail_message[1:]),
                        latest_fail_pos[-1],
                        stream
                    )
                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            else:
                action = SemanticAction(stream[pos])
                pos += 1
            continue
        elif opcode == LIST_START:
            scope_rest = (scope, scope_rest)
            scope = []
            continue
        elif opcode == LIST_END:
            action = SemanticAction(scope, lambda self: [x.eval(self.runtime) for x in self.value])
            scope, scope_rest = scope_rest
            continue
        elif opcode == MATCH_ANY:
            if pos >= len(stream):
                fail_message = ("expected any",)
                fail_pos = pos_rest+(pos,)
                if fail_pos >= latest_fail_pos:
                    latest_fail_message = fail_message
                    latest_fail_pos = fail_pos
                call_backtrack_entry = tuple()
                while call_backtrack_stack:
                    call_backtrack_entry = call_backtrack_stack.pop()
                    if len(call_backtrack_entry) == 7:
                        break
                    else:
                        _, key = call_backtrack_entry
                        memo[key] = (None, fail_message)
                if len(call_backtrack_entry) != 7:
                    raise MatchError(
                        latest_fail_message[0].format(*latest_fail_message[1:]),
                        latest_fail_pos[-1],
                        stream
                    )
                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            else:
                action = SemanticAction(stream[pos])
                pos += 1
            continue
        elif opcode == PUSH_STREAM:
            if pos >= len(stream) or not isinstance(stream[pos], list):
                fail_message = ("expected list",)
                fail_pos = pos_rest+(pos,)
                if fail_pos >= latest_fail_pos:
                    latest_fail_message = fail_message
                    latest_fail_pos = fail_pos
                call_backtrack_entry = tuple()
                while call_backtrack_stack:
                    call_backtrack_entry = call_backtrack_stack.pop()
                    if len(call_backtrack_entry) == 7:
                        break
                    else:
                        _, key = call_backtrack_entry
                        memo[key] = (None, fail_message)
                if len(call_backtrack_entry) != 7:
                    raise MatchError(
                        latest_fail_message[0].format(*latest_fail_message[1:]),
                        latest_fail_pos[-1],
                        stream
                    )
                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            else:
                stream_rest = (stream, stream_rest)
                pos_rest = pos_rest + (pos,)
                stream = stream[pos]
                pos = 0
            continue
        elif opcode == POP_STREAM:
            if pos < len(stream):
                fail_message = ("expected end of list",)
                fail_pos = pos_rest+(pos,)
                if fail_pos >= latest_fail_pos:
                    latest_fail_message = fail_message
                    latest_fail_pos = fail_pos
                call_backtrack_entry = tuple()
                while call_backtrack_stack:
                    call_backtrack_entry = call_backtrack_stack.pop()
                    if len(call_backtrack_entry) == 7:
                        break
                    else:
                        _, key = call_backtrack_entry
                        memo[key] = (None, fail_message)
                if len(call_backtrack_entry) != 7:
                    raise MatchError(
                        latest_fail_message[0].format(*latest_fail_message[1:]),
                        latest_fail_pos[-1],
                        stream
                    )
                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            else:
                stream, stream_rest = stream_rest
                pos, pos_rest = pos_rest[-1], pos_rest[:-1]
                pos += 1
            continue
        elif opcode == MATCH_CALL_RULE:
            if pos >= len(stream):
                fail_message = ("expected any",)
                fail_pos = pos_rest+(pos,)
                if fail_pos >= latest_fail_pos:
                    latest_fail_message = fail_message
                    latest_fail_pos = fail_pos
                call_backtrack_entry = tuple()
                while call_backtrack_stack:
                    call_backtrack_entry = call_backtrack_stack.pop()
                    if len(call_backtrack_entry) == 7:
                        break
                    else:
                        _, key = call_backtrack_entry
                        memo[key] = (None, fail_message)
                if len(call_backtrack_entry) != 7:
                    raise MatchError(
                        latest_fail_message[0].format(*latest_fail_message[1:]),
                        latest_fail_pos[-1],
                        stream
                    )
                (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            else:
                arg_fn_pc = rules[str(stream[pos])]
                pos += 1
                key = (arg_fn_pc, pos_rest+(pos,))
                if key in memo:
                    if memo[key][0] is None:
                        fail_message = memo[key][1]
                        fail_pos = pos_rest+(pos,)
                        if fail_pos >= latest_fail_pos:
                            latest_fail_message = fail_message
                            latest_fail_pos = fail_pos
                        call_backtrack_entry = tuple()
                        while call_backtrack_stack:
                            call_backtrack_entry = call_backtrack_stack.pop()
                            if len(call_backtrack_entry) == 7:
                                break
                            else:
                                _, key = call_backtrack_entry
                                memo[key] = (None, fail_message)
                        if len(call_backtrack_entry) != 7:
                            raise MatchError(
                                latest_fail_message[0].format(*latest_fail_message[1:]),
                                latest_fail_pos[-1],
                                stream
                            )
                        (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
                    else:
                        action, stream, stream_rest, pos, pos_rest = memo[key]
                else:
                    call_backtrack_stack.append((pc, key))
                    pc = arg_fn_pc
            continue
        elif opcode == FAIL:
            arg_message = code[pc]
            pc += 1
            fail_message = (arg_message,)
            fail_pos = pos_rest+(pos,)
            if fail_pos >= latest_fail_pos:
                latest_fail_message = fail_message
                latest_fail_pos = fail_pos
            call_backtrack_entry = tuple()
            while call_backtrack_stack:
                call_backtrack_entry = call_backtrack_stack.pop()
                if len(call_backtrack_entry) == 7:
                    break
                else:
                    _, key = call_backtrack_entry
                    memo[key] = (None, fail_message)
            if len(call_backtrack_entry) != 7:
                raise MatchError(
                    latest_fail_message[0].format(*latest_fail_message[1:]),
                    latest_fail_pos[-1],
                    stream
                )
            (pc, stream, stream_rest, pos, pos_rest, scope, scope_rest) = call_backtrack_entry
            continue
        else:
            raise Exception("unknown opcode {}".format(opcode))
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
class Parser(Grammar):
    rules = {
        'file': 0,
        'grammar': 29,
        'rule': 57,
        'choice': 74,
        'sequence': 110,
        'expr': 130,
        'expr1': 155,
        'expr2': 220,
        'matchChar': 355,
        'maybeAction': 364,
        'actionExpr': 381,
        'hostExpr': 436,
        'hostListItem': 542,
        'formatExpr': 564,
        'var': 596,
        'string': 617,
        'char': 647,
        'innerChar': 668,
        'escape': 682,
        'name': 719,
        'nameStart': 741,
        'nameChar': 756,
        'space': 780
    }
    code = [
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        13,
        PUSH_SCOPE,
        CALL,
        780,
        CALL,
        29,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        2,
        LIST_END,
        BIND,
        'xs',
        CALL,
        780,
        BACKTRACK,
        25,
        MATCH_ANY,
        COMMIT,
        23,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: self.lookup('xs'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        719,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        '{',
        LIST_START,
        BACKTRACK,
        46,
        CALL,
        57,
        LIST_APPEND,
        COMMIT,
        39,
        LIST_END,
        BIND,
        'ys',
        CALL,
        780,
        MATCH_OBJECT,
        '}',
        ACTION,
        lambda self: concat([splice(0, 'Grammar'), splice(0, self.lookup('x')), splice(1, self.lookup('ys'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        719,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        '=',
        CALL,
        74,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, 'Rule'), splice(0, self.lookup('x')), splice(0, self.lookup('y'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        BACKTRACK,
        85,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '|',
        POP_SCOPE,
        COMMIT,
        85,
        CALL,
        110,
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        103,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '|',
        CALL,
        110,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        90,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: concat([splice(0, 'Or'), splice(0, self.lookup('x')), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        119,
        CALL,
        130,
        LIST_APPEND,
        COMMIT,
        112,
        LIST_END,
        BIND,
        'xs',
        CALL,
        364,
        BIND,
        'ys',
        ACTION,
        lambda self: concat([splice(0, 'Scope'), splice(0, concat([splice(0, 'And'), splice(1, self.lookup('xs')), splice(1, self.lookup('ys'))]))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        150,
        PUSH_SCOPE,
        CALL,
        155,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        ':',
        CALL,
        719,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, 'Bind'), splice(0, self.lookup('y')), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        154,
        PUSH_SCOPE,
        CALL,
        155,
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        171,
        PUSH_SCOPE,
        CALL,
        220,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        '*',
        ACTION,
        lambda self: concat([splice(0, 'Star'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        219,
        BACKTRACK,
        187,
        PUSH_SCOPE,
        CALL,
        220,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        '?',
        ACTION,
        lambda self: concat([splice(0, 'Or'), splice(0, self.lookup('x')), splice(0, concat([splice(0, 'And')]))]),
        POP_SCOPE,
        COMMIT,
        219,
        BACKTRACK,
        203,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '!',
        CALL,
        220,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'Not'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        219,
        BACKTRACK,
        215,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '%',
        ACTION,
        lambda self: concat([splice(0, 'MatchCallRule')]),
        POP_SCOPE,
        COMMIT,
        219,
        PUSH_SCOPE,
        CALL,
        220,
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        244,
        PUSH_SCOPE,
        CALL,
        719,
        BIND,
        'x',
        BACKTRACK,
        239,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '=',
        POP_SCOPE,
        COMMIT,
        237,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: concat([splice(0, 'MatchRule'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        354,
        BACKTRACK,
        264,
        PUSH_SCOPE,
        CALL,
        780,
        CALL,
        647,
        BIND,
        'x',
        MATCH_OBJECT,
        '-',
        CALL,
        647,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, 'MatchRange'), splice(0, self.lookup('x')), splice(0, self.lookup('y'))]),
        POP_SCOPE,
        COMMIT,
        354,
        BACKTRACK,
        299,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        "'",
        LIST_START,
        BACKTRACK,
        289,
        PUSH_SCOPE,
        BACKTRACK,
        283,
        MATCH_OBJECT,
        "'",
        COMMIT,
        281,
        FAIL,
        'no match expected',
        CALL,
        355,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        272,
        LIST_END,
        BIND,
        'xs',
        MATCH_OBJECT,
        "'",
        ACTION,
        lambda self: concat([splice(0, 'And'), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        COMMIT,
        354,
        BACKTRACK,
        311,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '.',
        ACTION,
        lambda self: concat([splice(0, 'MatchAny')]),
        POP_SCOPE,
        COMMIT,
        354,
        BACKTRACK,
        331,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '(',
        CALL,
        74,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        ')',
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        COMMIT,
        354,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '[',
        LIST_START,
        BACKTRACK,
        344,
        CALL,
        130,
        LIST_APPEND,
        COMMIT,
        337,
        LIST_END,
        BIND,
        'xs',
        CALL,
        780,
        MATCH_OBJECT,
        ']',
        ACTION,
        lambda self: concat([splice(0, 'MatchList'), splice(0, concat([splice(0, 'And'), splice(1, self.lookup('xs'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        668,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'MatchObject'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        376,
        PUSH_SCOPE,
        CALL,
        381,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'Action'), splice(0, self.lookup('x'))]))]),
        POP_SCOPE,
        COMMIT,
        380,
        PUSH_SCOPE,
        ACTION,
        lambda self: concat([]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        421,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '-',
        MATCH_OBJECT,
        '>',
        CALL,
        436,
        BIND,
        'x',
        BACKTRACK,
        406,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        ':',
        CALL,
        719,
        POP_SCOPE,
        COMMIT,
        410,
        PUSH_SCOPE,
        ACTION,
        lambda self: '',
        POP_SCOPE,
        BIND,
        'y',
        CALL,
        381,
        BIND,
        'z',
        ACTION,
        lambda self: concat([splice(0, 'Set'), splice(0, self.lookup('y')), splice(0, self.lookup('x')), splice(0, self.lookup('z'))]),
        POP_SCOPE,
        COMMIT,
        435,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '-',
        MATCH_OBJECT,
        '>',
        CALL,
        436,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        450,
        PUSH_SCOPE,
        CALL,
        780,
        CALL,
        617,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'String'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        541,
        BACKTRACK,
        477,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '[',
        LIST_START,
        BACKTRACK,
        465,
        CALL,
        542,
        LIST_APPEND,
        COMMIT,
        458,
        LIST_END,
        BIND,
        'xs',
        CALL,
        780,
        MATCH_OBJECT,
        ']',
        ACTION,
        lambda self: concat([splice(0, 'List'), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        COMMIT,
        541,
        BACKTRACK,
        504,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '{',
        LIST_START,
        BACKTRACK,
        492,
        CALL,
        564,
        LIST_APPEND,
        COMMIT,
        485,
        LIST_END,
        BIND,
        'xs',
        CALL,
        780,
        MATCH_OBJECT,
        '}',
        ACTION,
        lambda self: concat([splice(0, 'Format'), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        COMMIT,
        541,
        BACKTRACK,
        535,
        PUSH_SCOPE,
        CALL,
        596,
        BIND,
        'x',
        CALL,
        780,
        MATCH_OBJECT,
        '(',
        LIST_START,
        BACKTRACK,
        523,
        CALL,
        436,
        LIST_APPEND,
        COMMIT,
        516,
        LIST_END,
        BIND,
        'ys',
        CALL,
        780,
        MATCH_OBJECT,
        ')',
        ACTION,
        lambda self: concat([splice(0, 'Call'), splice(0, self.lookup('x')), splice(1, self.lookup('ys'))]),
        POP_SCOPE,
        COMMIT,
        541,
        PUSH_SCOPE,
        CALL,
        596,
        BIND,
        'x',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        780,
        LIST_START,
        BACKTRACK,
        553,
        MATCH_OBJECT,
        '~',
        LIST_APPEND,
        COMMIT,
        546,
        LIST_END,
        BIND,
        'ys',
        CALL,
        436,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'ListItem'), splice(0, self.lookup('len')(self.lookup('ys'))), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        591,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '>',
        LIST_START,
        BACKTRACK,
        579,
        CALL,
        564,
        LIST_APPEND,
        COMMIT,
        572,
        LIST_END,
        BIND,
        'xs',
        CALL,
        780,
        MATCH_OBJECT,
        '<',
        ACTION,
        lambda self: concat([splice(0, 'Indent'), splice(0, concat([splice(0, 'Format'), splice(1, self.lookup('xs'))]))]),
        POP_SCOPE,
        COMMIT,
        595,
        PUSH_SCOPE,
        CALL,
        436,
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        719,
        BIND,
        'x',
        BACKTRACK,
        613,
        PUSH_SCOPE,
        CALL,
        780,
        MATCH_OBJECT,
        '=',
        POP_SCOPE,
        COMMIT,
        611,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: concat([splice(0, 'Lookup'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '"',
        LIST_START,
        BACKTRACK,
        638,
        PUSH_SCOPE,
        BACKTRACK,
        632,
        MATCH_OBJECT,
        '"',
        COMMIT,
        630,
        FAIL,
        'no match expected',
        CALL,
        668,
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        621,
        LIST_END,
        BIND,
        'xs',
        MATCH_OBJECT,
        '"',
        ACTION,
        lambda self: join([self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_OBJECT,
        "'",
        BACKTRACK,
        658,
        MATCH_OBJECT,
        "'",
        COMMIT,
        656,
        FAIL,
        'no match expected',
        CALL,
        668,
        BIND,
        'x',
        MATCH_OBJECT,
        "'",
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        678,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '\\',
        CALL,
        682,
        POP_SCOPE,
        COMMIT,
        681,
        PUSH_SCOPE,
        MATCH_ANY,
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        692,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '\\',
        ACTION,
        lambda self: '\\',
        POP_SCOPE,
        COMMIT,
        718,
        BACKTRACK,
        702,
        PUSH_SCOPE,
        MATCH_OBJECT,
        "'",
        ACTION,
        lambda self: "'",
        POP_SCOPE,
        COMMIT,
        718,
        BACKTRACK,
        712,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '"',
        ACTION,
        lambda self: '"',
        POP_SCOPE,
        COMMIT,
        718,
        PUSH_SCOPE,
        MATCH_OBJECT,
        'n',
        ACTION,
        lambda self: '\n',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        780,
        CALL,
        741,
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        734,
        CALL,
        756,
        LIST_APPEND,
        COMMIT,
        727,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: join([self.lookup('x'), self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        750,
        PUSH_SCOPE,
        MATCH_RANGE,
        'a',
        'z',
        POP_SCOPE,
        COMMIT,
        755,
        PUSH_SCOPE,
        MATCH_RANGE,
        'A',
        'Z',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        765,
        PUSH_SCOPE,
        MATCH_RANGE,
        'a',
        'z',
        POP_SCOPE,
        COMMIT,
        779,
        BACKTRACK,
        774,
        PUSH_SCOPE,
        MATCH_RANGE,
        'A',
        'Z',
        POP_SCOPE,
        COMMIT,
        779,
        PUSH_SCOPE,
        MATCH_RANGE,
        '0',
        '9',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        799,
        BACKTRACK,
        792,
        PUSH_SCOPE,
        MATCH_OBJECT,
        ' ',
        POP_SCOPE,
        COMMIT,
        796,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '\n',
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        782,
        LIST_END,
        POP_SCOPE,
        RETURN
    ]
class CodeGenerator(Grammar):
    rules = {
        'Grammar': 0,
        'Rule': 19,
        'Or': 31,
        'Scope': 52,
        'And': 61,
        'Bind': 77,
        'Star': 89,
        'Not': 98,
        'MatchCallRule': 107,
        'MatchRule': 112,
        'MatchRange': 120,
        'MatchAny': 131,
        'MatchList': 136,
        'MatchObject': 145,
        'Action': 153,
        'asts': 161,
        'ast': 184
    }
    code = [
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        12,
        CALL,
        184,
        LIST_APPEND,
        COMMIT,
        5,
        LIST_END,
        BIND,
        'ys',
        ACTION,
        lambda self: concat([splice(0, 'Grammar'), splice(0, self.lookup('x')), splice(2, self.lookup('ys'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        CALL,
        184,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'Rule'), splice(0, self.lookup('x'))])), splice(1, self.lookup('y')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'RETURN')]))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        47,
        PUSH_SCOPE,
        CALL,
        184,
        BIND,
        'x',
        CALL,
        31,
        BIND,
        'y',
        ACTION,
        lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'BACKTRACK')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('a'))])), splice(1, self.lookup('x')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'COMMIT')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('b'))])), splice(0, concat([splice(0, 'Label'), splice(0, self.lookup('a'))])), splice(1, self.lookup('y')), splice(0, concat([splice(0, 'Label'), splice(0, self.lookup('b'))]))]))),
        POP_SCOPE,
        COMMIT,
        51,
        PUSH_SCOPE,
        CALL,
        184,
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        184,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'PUSH_SCOPE')])), splice(1, self.lookup('x')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'POP_SCOPE')]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        70,
        CALL,
        184,
        LIST_APPEND,
        COMMIT,
        63,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: concat([splice(2, self.lookup('xs'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        CALL,
        184,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(1, self.lookup('y')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'BIND')])), splice(0, concat([splice(0, 'Value'), splice(0, self.lookup('x'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        184,
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'LIST_START')])), splice(0, concat([splice(0, 'Label'), splice(0, self.lookup('a'))])), splice(0, concat([splice(0, 'OpCode'), splice(0, 'BACKTRACK')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('b'))])), splice(1, self.lookup('x')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'LIST_APPEND')])), splice(0, concat([splice(0, 'OpCode'), splice(0, 'COMMIT')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('a'))])), splice(0, concat([splice(0, 'Label'), splice(0, self.lookup('b'))])), splice(0, concat([splice(0, 'OpCode'), splice(0, 'LIST_END')]))]))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        184,
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'BACKTRACK')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('b'))])), splice(1, self.lookup('x')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'COMMIT')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('a'))])), splice(0, concat([splice(0, 'Label'), splice(0, self.lookup('a'))])), splice(0, concat([splice(0, 'OpCode'), splice(0, 'FAIL')])), splice(0, concat([splice(0, 'Value'), splice(0, 'no match expected')])), splice(0, concat([splice(0, 'Label'), splice(0, self.lookup('b'))]))]))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'MATCH_CALL_RULE')]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'CALL')])), splice(0, concat([splice(0, 'Target'), splice(0, self.lookup('x'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        MATCH_ANY,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'MATCH_RANGE')])), splice(0, concat([splice(0, 'Value'), splice(0, self.lookup('x'))])), splice(0, concat([splice(0, 'Value'), splice(0, self.lookup('y'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'MATCH_ANY')]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        184,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'PUSH_STREAM')])), splice(1, self.lookup('x')), splice(0, concat([splice(0, 'OpCode'), splice(0, 'POP_STREAM')]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'MATCH_OBJECT')])), splice(0, concat([splice(0, 'Value'), splice(0, self.lookup('x'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'OpCode'), splice(0, 'ACTION')])), splice(0, concat([splice(0, 'Action'), splice(0, self.lookup('x'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        170,
        CALL,
        184,
        LIST_APPEND,
        COMMIT,
        163,
        LIST_END,
        BIND,
        'xs',
        BACKTRACK,
        180,
        MATCH_ANY,
        COMMIT,
        178,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: self.lookup('xs'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        PUSH_STREAM,
        MATCH_CALL_RULE,
        BIND,
        'x',
        POP_STREAM,
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        RETURN
    ]
class Assembler(Grammar):
    rules = {
        'Grammar': 0,
        'Rule': 19,
        'Label': 27,
        'Target': 35,
        'Patch': 43,
        'OpCode': 54,
        'Value': 62,
        'Action': 71,
        'Set': 80,
        'String': 97,
        'List': 102,
        'ListItem': 111,
        'Format': 124,
        'Indent': 133,
        'Call': 142,
        'Lookup': 155,
        'asts': 164,
        'astList': 187,
        'ast': 203,
        'py': 213
    }
    code = [
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        12,
        CALL,
        203,
        LIST_APPEND,
        COMMIT,
        5,
        LIST_END,
        BIND,
        'ys',
        ACTION,
        lambda self: self.bind('rules', self.lookup('list')(), lambda: self.bind('code', self.lookup('list')(), lambda: self.bind('labels', self.lookup('dict')(), lambda: self.bind('patches', self.lookup('list')(), lambda: self.bind('', self.lookup('ys'), lambda: self.bind('', self.lookup('run')('asts', self.lookup('patches')), lambda: join(['class ', self.lookup('x'), '(Grammar):\n', indent(join(['rules = {\n', indent(join([self.lookup('join')(self.lookup('rules'), ',\n')]), self.lookup('indentprefix')), '\n}\n', 'code = [\n', indent(join([self.lookup('join')(self.lookup('code'), ',\n')]), self.lookup('indentprefix')), '\n]\n']), self.lookup('indentprefix'))]))))))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('append')(self.lookup('rules'), join([self.lookup('repr')(self.lookup('x')), ': ', self.lookup('len')(self.lookup('code'))])), lambda: self.lookup('set')(self.lookup('labels'), self.lookup('x'), self.lookup('len')(self.lookup('code')))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('set')(self.lookup('labels'), self.lookup('x'), self.lookup('len')(self.lookup('code'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('append')(self.lookup('patches'), concat([splice(0, 'Patch'), splice(0, self.lookup('len')(self.lookup('code'))), splice(0, self.lookup('x'))])), lambda: self.lookup('append')(self.lookup('code'), 'placeholder')),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        MATCH_ANY,
        BIND,
        'y',
        ACTION,
        lambda self: self.lookup('set')(self.lookup('code'), self.lookup('x'), self.lookup('get')(self.lookup('labels'), self.lookup('y'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('append')(self.lookup('code'), self.lookup('x')),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        213,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('append')(self.lookup('code'), self.lookup('x')),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        203,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('append')(self.lookup('code'), join(['lambda self: ', self.lookup('x')])),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        213,
        BIND,
        'x',
        CALL,
        203,
        BIND,
        'y',
        CALL,
        203,
        BIND,
        'z',
        ACTION,
        lambda self: join(['self.bind(', self.lookup('x'), ', ', self.lookup('y'), ', lambda: ', self.lookup('z'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        213,
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        187,
        BIND,
        'x',
        ACTION,
        lambda self: join(['concat([', self.lookup('x'), '])']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        213,
        BIND,
        'x',
        CALL,
        203,
        BIND,
        'y',
        ACTION,
        lambda self: join(['splice(', self.lookup('x'), ', ', self.lookup('y'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        187,
        BIND,
        'x',
        ACTION,
        lambda self: join(['join([', self.lookup('x'), '])']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        203,
        BIND,
        'x',
        ACTION,
        lambda self: join(['indent(', self.lookup('x'), ', ', "self.lookup('indentprefix'))"]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        203,
        BIND,
        'x',
        CALL,
        187,
        BIND,
        'y',
        ACTION,
        lambda self: join([self.lookup('x'), '(', self.lookup('y'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        213,
        BIND,
        'x',
        ACTION,
        lambda self: join(['self.lookup(', self.lookup('x'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        173,
        CALL,
        203,
        LIST_APPEND,
        COMMIT,
        166,
        LIST_END,
        BIND,
        'xs',
        BACKTRACK,
        183,
        MATCH_ANY,
        COMMIT,
        181,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: join([self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        196,
        CALL,
        203,
        LIST_APPEND,
        COMMIT,
        189,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: self.lookup('join')(self.lookup('xs'), ', '),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        PUSH_STREAM,
        MATCH_CALL_RULE,
        BIND,
        'x',
        POP_STREAM,
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: self.lookup('repr')(self.lookup('x')),
        POP_SCOPE,
        RETURN
    ]
if __name__ == "__main__":
    import sys
    def read(path):
        if path == "-":
            return sys.stdin.read()
        with open(path) as f:
            return f.read()
    args = sys.argv[1:] or ["--compile", "-"]
    while args:
        command = args.pop(0)
        if command == "--support":
            sys.stdout.write(SUPPORT)
        elif command == "--copy":
            sys.stdout.write(read(args.pop(0)))
        elif command == "--embed":
            sys.stdout.write("{} = {}\n".format(
                args.pop(0),
                repr(read(args.pop(0)))
            ))
        elif command == "--compile":
            sys.stdout.write(compile_chain(
                [(Parser, "file"), (CodeGenerator, "asts"), (Assembler, "asts")],
                read(args.pop(0))
            ))
        else:
            sys.exit("ERROR: Unknown command '{}'".format(command))
