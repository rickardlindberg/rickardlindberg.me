SUPPORT = 'PUSH_SCOPE = 0\nBACKTRACK = 1\nCALL = 2\nPOP_SCOPE = 3\nMATCH_OBJECT = 4\nCOMMIT = 5\nRETURN = 6\nLIST_APPEND = 7\nBIND = 8\nACTION = 9\nMATCH_RANGE = 10\nLIST_START = 11\nLIST_END = 12\nMATCH_ANY = 13\nPUSH_STREAM = 14\nPOP_STREAM = 15\nMATCH_CALL_RULE = 16\nFAIL = 17\ndef vm(instructions, labels, start_rule, stream):\n    action = SemanticAction(None)\n    pc = labels[start_rule]\n    call_backtrack_stack = []\n    stream, pos, stream_pos_stack = (stream, 0, [])\n    scope, scope_stack = (None, [])\n    fail_message = None\n    latest_fail_message, latest_fail_pos = (None, tuple())\n    memo = {}\n    while True:\n        name = instructions[pc]\n        pc += 1\n        if name == PUSH_SCOPE:\n            scope_stack.append(scope)\n            scope = {}\n            continue\n        elif name == BACKTRACK:\n            arg_label = instructions[pc]\n            pc += 1\n            call_backtrack_stack.append((\n                labels[arg_label], pos, len(stream_pos_stack), len(scope_stack)\n            ))\n            continue\n        elif name == CALL:\n            arg_fn_name = instructions[pc]\n            pc += 1\n            key = (arg_fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))\n            if key in memo:\n                if memo[key][0] is None:\n                    fail_message = memo[key][1]\n                    fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                    if fail_pos >= latest_fail_pos:\n                        latest_fail_message = fail_message\n                        latest_fail_pos = fail_pos\n                    call_backtrack_entry = tuple()\n                    while call_backtrack_stack:\n                        call_backtrack_entry = call_backtrack_stack.pop()\n                        if len(call_backtrack_entry) == 4:\n                            break\n                        else:\n                            _, key = call_backtrack_entry\n                            memo[key] = (None, fail_message)\n                    if len(call_backtrack_entry) != 4:\n                        raise MatchError(\n                            latest_fail_message[0].format(*latest_fail_message[1:]),\n                            latest_fail_pos,\n                            stream_pos_stack[0][0] if stream_pos_stack else stream\n                        )\n                    (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                    if len(stream_pos_stack) > stream_stack_len:\n                        stream = stream_pos_stack[stream_stack_len][0]\n                    stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                    if len(scope_stack) > scope_stack_len:\n                        scope = scope_stack[scope_stack_len]\n                    scope_stack = scope_stack[:scope_stack_len]\n                else:\n                    action, stream_pos_stack = memo[key]\n                    stream_pos_stack = stream_pos_stack[:]\n                    stream, pos = stream_pos_stack.pop()\n            else:\n                call_backtrack_stack.append((pc, key))\n                pc = labels[arg_fn_name]\n            continue\n        elif name == POP_SCOPE:\n            scope = scope_stack.pop()\n            continue\n        elif name == MATCH_OBJECT:\n            arg_object = instructions[pc]\n            pc += 1\n            if pos >= len(stream) or stream[pos] != arg_object:\n                fail_message = ("expected {!r}", arg_object)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                action = SemanticAction(arg_object)\n                pos += 1\n            continue\n        elif name == COMMIT:\n            arg_label = instructions[pc]\n            pc += 1\n            call_backtrack_stack.pop()\n            pc = labels[arg_label]\n            continue\n        elif name == RETURN:\n            if len(call_backtrack_stack) == 0:\n                return action\n            pc, key = call_backtrack_stack.pop()\n            memo[key] = (action, stream_pos_stack+[(stream, pos)])\n            continue\n        elif name == LIST_APPEND:\n            scope.append(action)\n            continue\n        elif name == BIND:\n            arg_name = instructions[pc]\n            pc += 1\n            scope[arg_name] = action\n            continue\n        elif name == ACTION:\n            arg_fn = instructions[pc]\n            pc += 1\n            action = SemanticAction(scope, arg_fn)\n            continue\n        elif name == MATCH_RANGE:\n            arg_start = instructions[pc]\n            pc += 1\n            arg_end = instructions[pc]\n            pc += 1\n            if pos >= len(stream) or not (arg_start <= stream[pos] <= arg_end):\n                fail_message = ("expected range {!r}-{!r}", arg_start, arg_end)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n            continue\n        elif name == LIST_START:\n            scope_stack.append(scope)\n            scope = []\n            continue\n        elif name == LIST_END:\n            action = SemanticAction(scope, lambda self: [x.eval(self.runtime) for x in self.value])\n            scope = scope_stack.pop()\n            continue\n        elif name == MATCH_ANY:\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n            continue\n        elif name == PUSH_STREAM:\n            if pos >= len(stream) or not isinstance(stream[pos], list):\n                fail_message = ("expected list",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                stream_pos_stack.append((stream, pos))\n                stream = stream[pos]\n                pos = 0\n            continue\n        elif name == POP_STREAM:\n            if pos < len(stream):\n                fail_message = ("expected end of list",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                stream, pos = stream_pos_stack.pop()\n                pos += 1\n            continue\n        elif name == MATCH_CALL_RULE:\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                arg_fn_name = str(stream[pos])\n                pos += 1\n                key = (arg_fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))\n                if key in memo:\n                    if memo[key][0] is None:\n                        fail_message = memo[key][1]\n                        fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                        if fail_pos >= latest_fail_pos:\n                            latest_fail_message = fail_message\n                            latest_fail_pos = fail_pos\n                        call_backtrack_entry = tuple()\n                        while call_backtrack_stack:\n                            call_backtrack_entry = call_backtrack_stack.pop()\n                            if len(call_backtrack_entry) == 4:\n                                break\n                            else:\n                                _, key = call_backtrack_entry\n                                memo[key] = (None, fail_message)\n                        if len(call_backtrack_entry) != 4:\n                            raise MatchError(\n                                latest_fail_message[0].format(*latest_fail_message[1:]),\n                                latest_fail_pos,\n                                stream_pos_stack[0][0] if stream_pos_stack else stream\n                            )\n                        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                        if len(stream_pos_stack) > stream_stack_len:\n                            stream = stream_pos_stack[stream_stack_len][0]\n                        stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                        if len(scope_stack) > scope_stack_len:\n                            scope = scope_stack[scope_stack_len]\n                        scope_stack = scope_stack[:scope_stack_len]\n                    else:\n                        action, stream_pos_stack = memo[key]\n                        stream_pos_stack = stream_pos_stack[:]\n                        stream, pos = stream_pos_stack.pop()\n                else:\n                    call_backtrack_stack.append((pc, key))\n                    pc = labels[arg_fn_name]\n            continue\n        elif name == FAIL:\n            arg_message = instructions[pc]\n            pc += 1\n            fail_message = (arg_message,)\n            fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n            if fail_pos >= latest_fail_pos:\n                latest_fail_message = fail_message\n                latest_fail_pos = fail_pos\n            call_backtrack_entry = tuple()\n            while call_backtrack_stack:\n                call_backtrack_entry = call_backtrack_stack.pop()\n                if len(call_backtrack_entry) == 4:\n                    break\n                else:\n                    _, key = call_backtrack_entry\n                    memo[key] = (None, fail_message)\n            if len(call_backtrack_entry) != 4:\n                raise MatchError(\n                    latest_fail_message[0].format(*latest_fail_message[1:]),\n                    latest_fail_pos,\n                    stream_pos_stack[0][0] if stream_pos_stack else stream\n                )\n            (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n            if len(stream_pos_stack) > stream_stack_len:\n                stream = stream_pos_stack[stream_stack_len][0]\n            stream_pos_stack = stream_pos_stack[:stream_stack_len]\n            if len(scope_stack) > scope_stack_len:\n                scope = scope_stack[scope_stack_len]\n            scope_stack = scope_stack[:scope_stack_len]\n            continue\n        else:\n            raise Exception("unknown instruction {}".format(name))\nclass SemanticAction(object):\n\n    def __init__(self, value, fn=lambda self: self.value):\n        self.value = value\n        self.fn = fn\n\n    def eval(self, runtime):\n        self.runtime = runtime\n        return self.fn(self)\n\n    def bind(self, name, value, continuation):\n        self.runtime = self.runtime.set(name, value)\n        return continuation()\n\n    def lookup(self, name):\n        if name in self.value:\n            return self.value[name].eval(self.runtime)\n        else:\n            return self.runtime[name]\n\nclass MatchError(Exception):\n\n    def __init__(self, message, pos, stream):\n        Exception.__init__(self)\n        self.message = message\n        self.pos = pos\n        self.stream = stream\n\nclass Grammar(object):\n\n    def run(self, rule, stream):\n        return Runtime(self, {\n            "label": Counter(),\n            "indentprefix": "    ",\n        }).run(rule, stream)\n\nclass Runtime(dict):\n\n    def __init__(self, grammar, values):\n        dict.__init__(self, dict(values, run=self.run))\n        self.grammar = grammar\n\n    def set(self, key, value):\n        return Runtime(self.grammar, dict(self, **{key: value}))\n\n    def run(self, rule, stream):\n        return vm(self.grammar.instructions, self.grammar.labels, rule, stream).eval(self)\n\nclass Counter(object):\n\n    def __init__(self):\n        self.value = 0\n\n    def __call__(self):\n        result = self.value\n        self.value += 1\n        return result\n\nclass List(list):\n\n    def __call__(self, item):\n        self.append(item)\n\nclass Dict(dict):\n\n    def __call__(self, *args):\n        if len(args) == 1:\n            return self[args[0]]\n        else:\n            self[args[0]] = args[1]\n\ndef splice(depth, item):\n    if depth == 0:\n        return [item]\n    else:\n        return concat([splice(depth-1, subitem) for subitem in item])\n\ndef concat(lists):\n    return [x for xs in lists for x in xs]\n\ndef join(items, delimiter=""):\n    return delimiter.join(\n        join(item, delimiter) if isinstance(item, list) else str(item)\n        for item in items\n    )\n\ndef indent(text, prefix="    "):\n    return "".join(prefix+line for line in text.splitlines(True))\n\ndef compile_chain(grammars, source):\n    import sys\n    import pprint\n    for grammar, rule in grammars:\n        try:\n            source = grammar().run(rule, source)\n        except MatchError as e:\n            stream = e.stream\n            for pos in e.pos[:-1]:\n                stream = stream[pos]\n            pos = e.pos[-1]\n            MARKER = "\\033[0;31m<ERROR POSITION>\\033[0m"\n            if isinstance(stream, str):\n                stream_string = stream[:pos] + MARKER + stream[pos:]\n            else:\n                stream_string = pprint.pformat(stream)\n            sys.exit("ERROR: {}\\nPOSITION: {}\\nSTREAM:\\n{}".format(\n                e.message,\n                pos,\n                indent(stream_string)\n            ))\n    return source\n'
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
def vm(instructions, labels, start_rule, stream):
    action = SemanticAction(None)
    pc = labels[start_rule]
    call_backtrack_stack = []
    stream, pos, stream_pos_stack = (stream, 0, [])
    scope, scope_stack = (None, [])
    fail_message = None
    latest_fail_message, latest_fail_pos = (None, tuple())
    memo = {}
    while True:
        name = instructions[pc]
        pc += 1
        if name == PUSH_SCOPE:
            scope_stack.append(scope)
            scope = {}
            continue
        elif name == BACKTRACK:
            arg_label = instructions[pc]
            pc += 1
            call_backtrack_stack.append((
                labels[arg_label], pos, len(stream_pos_stack), len(scope_stack)
            ))
            continue
        elif name == CALL:
            arg_fn_name = instructions[pc]
            pc += 1
            key = (arg_fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
            if key in memo:
                if memo[key][0] is None:
                    fail_message = memo[key][1]
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
                else:
                    action, stream_pos_stack = memo[key]
                    stream_pos_stack = stream_pos_stack[:]
                    stream, pos = stream_pos_stack.pop()
            else:
                call_backtrack_stack.append((pc, key))
                pc = labels[arg_fn_name]
            continue
        elif name == POP_SCOPE:
            scope = scope_stack.pop()
            continue
        elif name == MATCH_OBJECT:
            arg_object = instructions[pc]
            pc += 1
            if pos >= len(stream) or stream[pos] != arg_object:
                fail_message = ("expected {!r}", arg_object)
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
            else:
                action = SemanticAction(arg_object)
                pos += 1
            continue
        elif name == COMMIT:
            arg_label = instructions[pc]
            pc += 1
            call_backtrack_stack.pop()
            pc = labels[arg_label]
            continue
        elif name == RETURN:
            if len(call_backtrack_stack) == 0:
                return action
            pc, key = call_backtrack_stack.pop()
            memo[key] = (action, stream_pos_stack+[(stream, pos)])
            continue
        elif name == LIST_APPEND:
            scope.append(action)
            continue
        elif name == BIND:
            arg_name = instructions[pc]
            pc += 1
            scope[arg_name] = action
            continue
        elif name == ACTION:
            arg_fn = instructions[pc]
            pc += 1
            action = SemanticAction(scope, arg_fn)
            continue
        elif name == MATCH_RANGE:
            arg_start = instructions[pc]
            pc += 1
            arg_end = instructions[pc]
            pc += 1
            if pos >= len(stream) or not (arg_start <= stream[pos] <= arg_end):
                fail_message = ("expected range {!r}-{!r}", arg_start, arg_end)
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
            else:
                action = SemanticAction(stream[pos])
                pos += 1
            continue
        elif name == LIST_START:
            scope_stack.append(scope)
            scope = []
            continue
        elif name == LIST_END:
            action = SemanticAction(scope, lambda self: [x.eval(self.runtime) for x in self.value])
            scope = scope_stack.pop()
            continue
        elif name == MATCH_ANY:
            if pos >= len(stream):
                fail_message = ("expected any",)
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
            else:
                action = SemanticAction(stream[pos])
                pos += 1
            continue
        elif name == PUSH_STREAM:
            if pos >= len(stream) or not isinstance(stream[pos], list):
                fail_message = ("expected list",)
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
            else:
                stream_pos_stack.append((stream, pos))
                stream = stream[pos]
                pos = 0
            continue
        elif name == POP_STREAM:
            if pos < len(stream):
                fail_message = ("expected end of list",)
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
            else:
                stream, pos = stream_pos_stack.pop()
                pos += 1
            continue
        elif name == MATCH_CALL_RULE:
            if pos >= len(stream):
                fail_message = ("expected any",)
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
            else:
                arg_fn_name = str(stream[pos])
                pos += 1
                key = (arg_fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
                if key in memo:
                    if memo[key][0] is None:
                        fail_message = memo[key][1]
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
                    else:
                        action, stream_pos_stack = memo[key]
                        stream_pos_stack = stream_pos_stack[:]
                        stream, pos = stream_pos_stack.pop()
                else:
                    call_backtrack_stack.append((pc, key))
                    pc = labels[arg_fn_name]
            continue
        elif name == FAIL:
            arg_message = instructions[pc]
            pc += 1
            fail_message = (arg_message,)
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
            continue
        else:
            raise Exception("unknown instruction {}".format(name))
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

    def run(self, rule, stream):
        return Runtime(self, {
            "label": Counter(),
            "indentprefix": "    ",
        }).run(rule, stream)

class Runtime(dict):

    def __init__(self, grammar, values):
        dict.__init__(self, dict(values, run=self.run))
        self.grammar = grammar

    def set(self, key, value):
        return Runtime(self.grammar, dict(self, **{key: value}))

    def run(self, rule, stream):
        return vm(self.grammar.instructions, self.grammar.labels, rule, stream).eval(self)

class Counter(object):

    def __init__(self):
        self.value = 0

    def __call__(self):
        result = self.value
        self.value += 1
        return result

class List(list):

    def __call__(self, item):
        self.append(item)

class Dict(dict):

    def __call__(self, *args):
        if len(args) == 1:
            return self[args[0]]
        else:
            self[args[0]] = args[1]

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
            stream = e.stream
            for pos in e.pos[:-1]:
                stream = stream[pos]
            pos = e.pos[-1]
            MARKER = "\033[0;31m<ERROR POSITION>\033[0m"
            if isinstance(stream, str):
                stream_string = stream[:pos] + MARKER + stream[pos:]
            else:
                stream_string = pprint.pformat(stream)
            sys.exit("ERROR: {}\nPOSITION: {}\nSTREAM:\n{}".format(
                e.message,
                pos,
                indent(stream_string)
            ))
    return source
class Parser(Grammar):

    labels = {'file': 0, 0: 2, 1: 13, 2: 23, 3: 25, 'grammar': 29, 4: 39, 5: 46, 'rule': 57, 'choice': 74, 6: 85, 7: 85, 8: 90, 9: 103, 'sequence': 110, 10: 112, 11: 119, 'expr': 130, 12: 150, 13: 154, 'expr1': 155, 14: 171, 16: 187, 18: 203, 20: 215, 21: 219, 19: 219, 17: 219, 15: 219, 'expr2': 220, 24: 237, 25: 239, 22: 244, 26: 264, 30: 272, 32: 281, 33: 283, 31: 289, 28: 299, 34: 311, 36: 331, 38: 337, 39: 344, 37: 354, 35: 354, 29: 354, 27: 354, 23: 354, 'matchChar': 355, 'maybeAction': 364, 42: 372, 43: 379, 40: 387, 41: 391, 'actionExpr': 392, 44: 415, 45: 419, 'hostExpr': 425, 46: 439, 50: 447, 51: 454, 48: 466, 54: 474, 55: 481, 52: 493, 58: 505, 59: 512, 56: 524, 57: 530, 53: 530, 49: 530, 47: 530, 'hostListItem': 531, 60: 535, 61: 542, 'formatExpr': 553, 64: 561, 65: 568, 62: 580, 63: 584, 'var': 585, 66: 601, 68: 616, 69: 618, 67: 621, 'string': 622, 70: 626, 72: 635, 73: 637, 71: 643, 'char': 652, 74: 661, 75: 663, 'innerChar': 673, 76: 683, 77: 686, 'escape': 687, 78: 697, 80: 707, 82: 717, 83: 723, 81: 723, 79: 723, 'name': 724, 84: 732, 85: 739, 'nameStart': 746, 86: 755, 87: 760, 'nameChar': 761, 88: 770, 90: 779, 91: 784, 89: 784, 'space': 785, 92: 787, 94: 797, 95: 801, 93: 804}
    instructions = [
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        1,
        PUSH_SCOPE,
        CALL,
        'space',
        CALL,
        'grammar',
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        0,
        LIST_END,
        BIND,
        'xs',
        CALL,
        'space',
        BACKTRACK,
        3,
        MATCH_ANY,
        COMMIT,
        2,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: self.lookup('xs'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'name',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        '{',
        LIST_START,
        BACKTRACK,
        5,
        CALL,
        'rule',
        LIST_APPEND,
        COMMIT,
        4,
        LIST_END,
        BIND,
        'ys',
        CALL,
        'space',
        MATCH_OBJECT,
        '}',
        ACTION,
        lambda self: concat([splice(0, 'Grammar'), splice(0, self.lookup('x')), splice(1, self.lookup('ys'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'name',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        '=',
        CALL,
        'choice',
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, 'Rule'), splice(0, self.lookup('x')), splice(0, self.lookup('y'))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        BACKTRACK,
        6,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '|',
        POP_SCOPE,
        COMMIT,
        7,
        CALL,
        'sequence',
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        9,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '|',
        CALL,
        'sequence',
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        8,
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
        11,
        CALL,
        'expr',
        LIST_APPEND,
        COMMIT,
        10,
        LIST_END,
        BIND,
        'xs',
        CALL,
        'maybeAction',
        BIND,
        'ys',
        ACTION,
        lambda self: concat([splice(0, 'Scope'), splice(0, concat([splice(0, 'And'), splice(1, self.lookup('xs')), splice(1, self.lookup('ys'))]))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        12,
        PUSH_SCOPE,
        CALL,
        'expr1',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        ':',
        CALL,
        'name',
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, 'Bind'), splice(0, self.lookup('y')), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        13,
        PUSH_SCOPE,
        CALL,
        'expr1',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        14,
        PUSH_SCOPE,
        CALL,
        'expr2',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        '*',
        ACTION,
        lambda self: concat([splice(0, 'Star'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        15,
        BACKTRACK,
        16,
        PUSH_SCOPE,
        CALL,
        'expr2',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        '?',
        ACTION,
        lambda self: concat([splice(0, 'Or'), splice(0, self.lookup('x')), splice(0, concat([splice(0, 'And')]))]),
        POP_SCOPE,
        COMMIT,
        17,
        BACKTRACK,
        18,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '!',
        CALL,
        'expr2',
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'Not'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        19,
        BACKTRACK,
        20,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '%',
        ACTION,
        lambda self: concat([splice(0, 'MatchCallRule')]),
        POP_SCOPE,
        COMMIT,
        21,
        PUSH_SCOPE,
        CALL,
        'expr2',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        22,
        PUSH_SCOPE,
        CALL,
        'name',
        BIND,
        'x',
        BACKTRACK,
        25,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '=',
        POP_SCOPE,
        COMMIT,
        24,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: concat([splice(0, 'MatchRule'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        23,
        BACKTRACK,
        26,
        PUSH_SCOPE,
        CALL,
        'space',
        CALL,
        'char',
        BIND,
        'x',
        MATCH_OBJECT,
        '-',
        CALL,
        'char',
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, 'MatchRange'), splice(0, self.lookup('x')), splice(0, self.lookup('y'))]),
        POP_SCOPE,
        COMMIT,
        27,
        BACKTRACK,
        28,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        "'",
        LIST_START,
        BACKTRACK,
        31,
        PUSH_SCOPE,
        BACKTRACK,
        33,
        MATCH_OBJECT,
        "'",
        COMMIT,
        32,
        FAIL,
        'no match expected',
        CALL,
        'matchChar',
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        30,
        LIST_END,
        BIND,
        'xs',
        MATCH_OBJECT,
        "'",
        ACTION,
        lambda self: concat([splice(0, 'And'), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        COMMIT,
        29,
        BACKTRACK,
        34,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '.',
        ACTION,
        lambda self: concat([splice(0, 'MatchAny')]),
        POP_SCOPE,
        COMMIT,
        35,
        BACKTRACK,
        36,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '(',
        CALL,
        'choice',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        ')',
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        COMMIT,
        37,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '[',
        LIST_START,
        BACKTRACK,
        39,
        CALL,
        'expr',
        LIST_APPEND,
        COMMIT,
        38,
        LIST_END,
        BIND,
        'xs',
        CALL,
        'space',
        MATCH_OBJECT,
        ']',
        ACTION,
        lambda self: concat([splice(0, 'MatchList'), splice(0, concat([splice(0, 'And'), splice(1, self.lookup('xs'))]))]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'innerChar',
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'MatchObject'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        40,
        PUSH_SCOPE,
        CALL,
        'actionExpr',
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        43,
        CALL,
        'actionExpr',
        LIST_APPEND,
        COMMIT,
        42,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: concat([splice(0, concat([splice(0, 'Action'), splice(1, self.lookup('x')), splice(2, self.lookup('xs'))]))]),
        POP_SCOPE,
        COMMIT,
        41,
        PUSH_SCOPE,
        ACTION,
        lambda self: concat([]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '-',
        MATCH_OBJECT,
        '>',
        CALL,
        'hostExpr',
        BIND,
        'x',
        BACKTRACK,
        44,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        ':',
        CALL,
        'name',
        POP_SCOPE,
        COMMIT,
        45,
        PUSH_SCOPE,
        ACTION,
        lambda self: '',
        POP_SCOPE,
        BIND,
        'y',
        ACTION,
        lambda self: concat([splice(0, self.lookup('y')), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        46,
        PUSH_SCOPE,
        CALL,
        'space',
        CALL,
        'string',
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'String'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        47,
        BACKTRACK,
        48,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '[',
        LIST_START,
        BACKTRACK,
        51,
        CALL,
        'hostListItem',
        LIST_APPEND,
        COMMIT,
        50,
        LIST_END,
        BIND,
        'xs',
        CALL,
        'space',
        MATCH_OBJECT,
        ']',
        ACTION,
        lambda self: concat([splice(0, 'List'), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        COMMIT,
        49,
        BACKTRACK,
        52,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '{',
        LIST_START,
        BACKTRACK,
        55,
        CALL,
        'formatExpr',
        LIST_APPEND,
        COMMIT,
        54,
        LIST_END,
        BIND,
        'xs',
        CALL,
        'space',
        MATCH_OBJECT,
        '}',
        ACTION,
        lambda self: concat([splice(0, 'Format'), splice(1, self.lookup('xs'))]),
        POP_SCOPE,
        COMMIT,
        53,
        BACKTRACK,
        56,
        PUSH_SCOPE,
        CALL,
        'var',
        BIND,
        'x',
        CALL,
        'space',
        MATCH_OBJECT,
        '(',
        LIST_START,
        BACKTRACK,
        59,
        CALL,
        'hostExpr',
        LIST_APPEND,
        COMMIT,
        58,
        LIST_END,
        BIND,
        'ys',
        CALL,
        'space',
        MATCH_OBJECT,
        ')',
        ACTION,
        lambda self: concat([splice(0, 'Call'), splice(0, self.lookup('x')), splice(1, self.lookup('ys'))]),
        POP_SCOPE,
        COMMIT,
        57,
        PUSH_SCOPE,
        CALL,
        'var',
        BIND,
        'x',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'space',
        LIST_START,
        BACKTRACK,
        61,
        MATCH_OBJECT,
        '~',
        LIST_APPEND,
        COMMIT,
        60,
        LIST_END,
        BIND,
        'ys',
        CALL,
        'hostExpr',
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'ListItem'), splice(0, len(self.lookup('ys'))), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        62,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '>',
        LIST_START,
        BACKTRACK,
        65,
        CALL,
        'formatExpr',
        LIST_APPEND,
        COMMIT,
        64,
        LIST_END,
        BIND,
        'xs',
        CALL,
        'space',
        MATCH_OBJECT,
        '<',
        ACTION,
        lambda self: concat([splice(0, 'Indent'), splice(0, concat([splice(0, 'Format'), splice(1, self.lookup('xs'))]))]),
        POP_SCOPE,
        COMMIT,
        63,
        PUSH_SCOPE,
        CALL,
        'hostExpr',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        66,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '#',
        CALL,
        'name',
        BIND,
        'x',
        ACTION,
        lambda self: concat([splice(0, 'Native'), splice(0, self.lookup('x'))]),
        POP_SCOPE,
        COMMIT,
        67,
        PUSH_SCOPE,
        CALL,
        'name',
        BIND,
        'x',
        BACKTRACK,
        69,
        PUSH_SCOPE,
        CALL,
        'space',
        MATCH_OBJECT,
        '=',
        POP_SCOPE,
        COMMIT,
        68,
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
        71,
        PUSH_SCOPE,
        BACKTRACK,
        73,
        MATCH_OBJECT,
        '"',
        COMMIT,
        72,
        FAIL,
        'no match expected',
        CALL,
        'innerChar',
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        70,
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
        75,
        MATCH_OBJECT,
        "'",
        COMMIT,
        74,
        FAIL,
        'no match expected',
        CALL,
        'innerChar',
        BIND,
        'x',
        MATCH_OBJECT,
        "'",
        ACTION,
        lambda self: self.lookup('x'),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        76,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '\\',
        CALL,
        'escape',
        POP_SCOPE,
        COMMIT,
        77,
        PUSH_SCOPE,
        MATCH_ANY,
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        78,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '\\',
        ACTION,
        lambda self: '\\',
        POP_SCOPE,
        COMMIT,
        79,
        BACKTRACK,
        80,
        PUSH_SCOPE,
        MATCH_OBJECT,
        "'",
        ACTION,
        lambda self: "'",
        POP_SCOPE,
        COMMIT,
        81,
        BACKTRACK,
        82,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '"',
        ACTION,
        lambda self: '"',
        POP_SCOPE,
        COMMIT,
        83,
        PUSH_SCOPE,
        MATCH_OBJECT,
        'n',
        ACTION,
        lambda self: '\n',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'space',
        CALL,
        'nameStart',
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        85,
        CALL,
        'nameChar',
        LIST_APPEND,
        COMMIT,
        84,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: join([self.lookup('x'), self.lookup('xs')]),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        86,
        PUSH_SCOPE,
        MATCH_RANGE,
        'a',
        'z',
        POP_SCOPE,
        COMMIT,
        87,
        PUSH_SCOPE,
        MATCH_RANGE,
        'A',
        'Z',
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        88,
        PUSH_SCOPE,
        MATCH_RANGE,
        'a',
        'z',
        POP_SCOPE,
        COMMIT,
        89,
        BACKTRACK,
        90,
        PUSH_SCOPE,
        MATCH_RANGE,
        'A',
        'Z',
        POP_SCOPE,
        COMMIT,
        91,
        PUSH_SCOPE,
        MATCH_RANGE,
        '0',
        '9',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        93,
        BACKTRACK,
        94,
        PUSH_SCOPE,
        MATCH_OBJECT,
        ' ',
        POP_SCOPE,
        COMMIT,
        95,
        PUSH_SCOPE,
        MATCH_OBJECT,
        '\n',
        POP_SCOPE,
        LIST_APPEND,
        COMMIT,
        92,
        LIST_END,
        POP_SCOPE,
        RETURN
    ]
class CodeGenerator(Grammar):

    labels = {'Grammar': 0, 0: 5, 1: 12, 'Rule': 19, 'Or': 31, 2: 47, 3: 51, 'Scope': 52, 'And': 61, 4: 63, 5: 70, 'Bind': 73, 'Star': 86, 'Not': 95, 'MatchCallRule': 104, 'MatchRule': 109, 'MatchRange': 118, 'MatchAny': 131, 'MatchList': 136, 'MatchObject': 145, 'Action': 154, 'actionExpr': 163, 6: 183, 7: 188, 'String': 189, 'List': 194, 'ListItem': 203, 'Format': 216, 'Indent': 225, 'Call': 234, 'Native': 247, 'Lookup': 251, 'asts': 260, 8: 262, 9: 269, 10: 277, 11: 279, 'ast': 283, 'astList': 293, 12: 295, 13: 302, 'py': 309}
    instructions = [
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        LIST_START,
        BACKTRACK,
        1,
        CALL,
        'ast',
        LIST_APPEND,
        COMMIT,
        0,
        LIST_END,
        BIND,
        'ys',
        ACTION,
        lambda self: self.bind('I', List(), lambda: self.bind('LABEL', Dict(), lambda: self.bind('', self.lookup('ys'), lambda: join(['class ', self.lookup('x'), '(Grammar):\n\n', indent(join(['labels = ', repr(self.lookup('LABEL')), '\n', 'instructions = [\n', indent(join([join(self.lookup('I'), ',\n')]), self.lookup('indentprefix')), '\n]\n']), self.lookup('indentprefix'))])))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        CALL,
        'ast',
        BIND,
        'y',
        ACTION,
        lambda self: self.bind('', self.lookup('LABEL')(self.lookup('x'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('y'), lambda: self.lookup('I')('RETURN'))),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        2,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        CALL,
        'Or',
        BIND,
        'y',
        ACTION,
        lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: self.bind('', self.lookup('I')('BACKTRACK'), lambda: self.bind('', self.lookup('I')(self.lookup('a')), lambda: self.bind('', self.lookup('x'), lambda: self.bind('', self.lookup('I')('COMMIT'), lambda: self.bind('', self.lookup('I')(self.lookup('b')), lambda: self.bind('', self.lookup('LABEL')(self.lookup('a'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('y'), lambda: self.lookup('LABEL')(self.lookup('b'), len(self.lookup('I')))))))))))),
        POP_SCOPE,
        COMMIT,
        3,
        PUSH_SCOPE,
        CALL,
        'ast',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('I')('PUSH_SCOPE'), lambda: self.bind('', self.lookup('x'), lambda: self.lookup('I')('POP_SCOPE'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        5,
        CALL,
        'ast',
        LIST_APPEND,
        COMMIT,
        4,
        LIST_END,
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        CALL,
        'ast',
        BIND,
        'y',
        ACTION,
        lambda self: self.bind('', self.lookup('y'), lambda: self.bind('', self.lookup('I')('BIND'), lambda: self.lookup('I')(self.lookup('x')))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: self.bind('', self.lookup('I')('LIST_START'), lambda: self.bind('', self.lookup('LABEL')(self.lookup('a'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('I')('BACKTRACK'), lambda: self.bind('', self.lookup('I')(self.lookup('b')), lambda: self.bind('', self.lookup('x'), lambda: self.bind('', self.lookup('I')('LIST_APPEND'), lambda: self.bind('', self.lookup('I')('COMMIT'), lambda: self.bind('', self.lookup('I')(self.lookup('a')), lambda: self.bind('', self.lookup('LABEL')(self.lookup('b'), len(self.lookup('I'))), lambda: self.lookup('I')('LIST_END')))))))))))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: self.bind('', self.lookup('I')('BACKTRACK'), lambda: self.bind('', self.lookup('I')(self.lookup('b')), lambda: self.bind('', self.lookup('x'), lambda: self.bind('', self.lookup('I')('COMMIT'), lambda: self.bind('', self.lookup('I')(self.lookup('a')), lambda: self.bind('', self.lookup('LABEL')(self.lookup('a'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('I')('FAIL'), lambda: self.bind('', self.lookup('I')("'no match expected'"), lambda: self.lookup('LABEL')(self.lookup('b'), len(self.lookup('I'))))))))))))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        ACTION,
        lambda self: self.lookup('I')('MATCH_CALL_RULE'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('I')('CALL'), lambda: self.lookup('I')(self.lookup('x'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        CALL,
        'py',
        BIND,
        'y',
        ACTION,
        lambda self: self.bind('', self.lookup('I')('MATCH_RANGE'), lambda: self.bind('', self.lookup('I')(self.lookup('x')), lambda: self.lookup('I')(self.lookup('y')))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        ACTION,
        lambda self: self.lookup('I')('MATCH_ANY'),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('I')('PUSH_STREAM'), lambda: self.bind('', self.lookup('x'), lambda: self.lookup('I')('POP_STREAM'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('I')('MATCH_OBJECT'), lambda: self.lookup('I')(self.lookup('x'))),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'actionExpr',
        BIND,
        'x',
        ACTION,
        lambda self: self.bind('', self.lookup('I')('ACTION'), lambda: self.lookup('I')(join(['lambda self: ', self.lookup('x')]))),
        POP_SCOPE,
        RETURN,
        BACKTRACK,
        6,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        CALL,
        'ast',
        BIND,
        'y',
        CALL,
        'actionExpr',
        BIND,
        'z',
        ACTION,
        lambda self: join(['self.bind(', self.lookup('x'), ', ', self.lookup('y'), ', lambda: ', self.lookup('z'), ')']),
        POP_SCOPE,
        COMMIT,
        7,
        PUSH_SCOPE,
        MATCH_ANY,
        CALL,
        'ast',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'astList',
        BIND,
        'x',
        ACTION,
        lambda self: join(['concat([', self.lookup('x'), '])']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        CALL,
        'ast',
        BIND,
        'y',
        ACTION,
        lambda self: join(['splice(', self.lookup('x'), ', ', self.lookup('y'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'astList',
        BIND,
        'x',
        ACTION,
        lambda self: join(['join([', self.lookup('x'), '])']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        ACTION,
        lambda self: join(['indent(', self.lookup('x'), ', ', "self.lookup('indentprefix'))"]),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'ast',
        BIND,
        'x',
        CALL,
        'astList',
        BIND,
        'y',
        ACTION,
        lambda self: join([self.lookup('x'), '(', self.lookup('y'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        CALL,
        'py',
        BIND,
        'x',
        ACTION,
        lambda self: join(['self.lookup(', self.lookup('x'), ')']),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        LIST_START,
        BACKTRACK,
        9,
        CALL,
        'ast',
        LIST_APPEND,
        COMMIT,
        8,
        LIST_END,
        BIND,
        'xs',
        BACKTRACK,
        11,
        MATCH_ANY,
        COMMIT,
        10,
        FAIL,
        'no match expected',
        ACTION,
        lambda self: join([self.lookup('xs')]),
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
        LIST_START,
        BACKTRACK,
        13,
        CALL,
        'ast',
        LIST_APPEND,
        COMMIT,
        12,
        LIST_END,
        BIND,
        'xs',
        ACTION,
        lambda self: join(self.lookup('xs'), ', '),
        POP_SCOPE,
        RETURN,
        PUSH_SCOPE,
        MATCH_ANY,
        BIND,
        'x',
        ACTION,
        lambda self: repr(self.lookup('x')),
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
                [(Parser, "file"), (CodeGenerator, "asts")],
                read(args.pop(0))
            ))
        else:
            sys.exit("ERROR: Unknown command '{}'".format(command))
