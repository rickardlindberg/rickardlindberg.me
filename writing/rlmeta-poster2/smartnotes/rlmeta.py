SUPPORT = 'PUSH_SCOPE = 0\nBACKTRACK = 1\nCALL = 2\nPOP_SCOPE = 3\nMATCH_OBJECT = 4\nCOMMIT = 5\nRETURN = 6\nLIST_APPEND = 7\nBIND = 8\nACTION = 9\nMATCH_RANGE = 10\nLIST_START = 11\nLIST_END = 12\nMATCH_ANY = 13\nPUSH_STREAM = 14\nPOP_STREAM = 15\nMATCH_CALL_RULE = 16\nFAIL = 17\ndef vm(instructions, labels, start_rule, stream):\n    action = SemanticAction(None)\n    pc = labels[start_rule]\n    call_backtrack_stack = []\n    stream, pos, stream_pos_stack = (stream, 0, [])\n    scope, scope_stack = (None, [])\n    fail_message = None\n    latest_fail_message, latest_fail_pos = (None, tuple())\n    memo = {}\n    while True:\n        name, arg1, arg2 = instructions[pc]\n        if name == PUSH_SCOPE:\n            scope_stack.append(scope)\n            scope = {}\n            pc += 1\n        elif name == BACKTRACK:\n            call_backtrack_stack.append((\n                labels[arg1], pos, len(stream_pos_stack), len(scope_stack)\n            ))\n            pc += 1\n        elif name == CALL:\n            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))\n            if key in memo:\n                if memo[key][0] is None:\n                    fail_message = memo[key][1]\n                    fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                    if fail_pos >= latest_fail_pos:\n                        latest_fail_message = fail_message\n                        latest_fail_pos = fail_pos\n                    call_backtrack_entry = tuple()\n                    while call_backtrack_stack:\n                        call_backtrack_entry = call_backtrack_stack.pop()\n                        if len(call_backtrack_entry) == 4:\n                            break\n                        else:\n                            _, key = call_backtrack_entry\n                            memo[key] = (None, fail_message)\n                    if len(call_backtrack_entry) != 4:\n                        raise MatchError(\n                            latest_fail_message[0].format(*latest_fail_message[1:]),\n                            latest_fail_pos,\n                            stream_pos_stack[0][0] if stream_pos_stack else stream\n                        )\n                    (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                    if len(stream_pos_stack) > stream_stack_len:\n                        stream = stream_pos_stack[stream_stack_len][0]\n                    stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                    if len(scope_stack) > scope_stack_len:\n                        scope = scope_stack[scope_stack_len]\n                    scope_stack = scope_stack[:scope_stack_len]\n                else:\n                    action, stream_pos_stack = memo[key]\n                    stream_pos_stack = stream_pos_stack[:]\n                    stream, pos = stream_pos_stack.pop()\n                    pc += 1\n            else:\n                call_backtrack_stack.append((pc+1, key))\n                pc = labels[arg1]\n        elif name == POP_SCOPE:\n            scope = scope_stack.pop()\n            pc += 1\n        elif name == MATCH_OBJECT:\n            if pos >= len(stream) or stream[pos] != arg1:\n                fail_message = ("expected {!r}", arg1)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                action = SemanticAction(arg1)\n                pos += 1\n                pc += 1\n        elif name == COMMIT:\n            call_backtrack_stack.pop()\n            pc = labels[arg1]\n        elif name == RETURN:\n            if len(call_backtrack_stack) == 0:\n                return action\n            pc, key = call_backtrack_stack.pop()\n            memo[key] = (action, stream_pos_stack+[(stream, pos)])\n        elif name == LIST_APPEND:\n            scope.append(action)\n            pc += 1\n        elif name == BIND:\n            scope[arg1] = action\n            pc += 1\n        elif name == ACTION:\n            action = SemanticAction(scope, arg1)\n            pc += 1\n        elif name == MATCH_RANGE:\n            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):\n                fail_message = ("expected range {!r}-{!r}", arg1, arg2)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n                pc += 1\n        elif name == LIST_START:\n            scope_stack.append(scope)\n            scope = []\n            pc += 1\n        elif name == LIST_END:\n            action = SemanticAction(scope, lambda self: [x.eval(self.runtime) for x in self.value])\n            scope = scope_stack.pop()\n            pc += 1\n        elif name == MATCH_ANY:\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                action = SemanticAction(stream[pos])\n                pos += 1\n                pc += 1\n        elif name == PUSH_STREAM:\n            if pos >= len(stream) or not isinstance(stream[pos], list):\n                fail_message = ("expected list",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                stream_pos_stack.append((stream, pos))\n                stream = stream[pos]\n                pos = 0\n                pc += 1\n        elif name == POP_STREAM:\n            if pos < len(stream):\n                fail_message = ("expected end of list",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                stream, pos = stream_pos_stack.pop()\n                pos += 1\n                pc += 1\n        elif name == MATCH_CALL_RULE:\n            if pos >= len(stream):\n                fail_message = ("expected any",)\n                fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                if fail_pos >= latest_fail_pos:\n                    latest_fail_message = fail_message\n                    latest_fail_pos = fail_pos\n                call_backtrack_entry = tuple()\n                while call_backtrack_stack:\n                    call_backtrack_entry = call_backtrack_stack.pop()\n                    if len(call_backtrack_entry) == 4:\n                        break\n                    else:\n                        _, key = call_backtrack_entry\n                        memo[key] = (None, fail_message)\n                if len(call_backtrack_entry) != 4:\n                    raise MatchError(\n                        latest_fail_message[0].format(*latest_fail_message[1:]),\n                        latest_fail_pos,\n                        stream_pos_stack[0][0] if stream_pos_stack else stream\n                    )\n                (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                if len(stream_pos_stack) > stream_stack_len:\n                    stream = stream_pos_stack[stream_stack_len][0]\n                stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                if len(scope_stack) > scope_stack_len:\n                    scope = scope_stack[scope_stack_len]\n                scope_stack = scope_stack[:scope_stack_len]\n            else:\n                fn_name = str(stream[pos])\n                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))\n                if key in memo:\n                    if memo[key][0] is None:\n                        fail_message = memo[key][1]\n                        fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n                        if fail_pos >= latest_fail_pos:\n                            latest_fail_message = fail_message\n                            latest_fail_pos = fail_pos\n                        call_backtrack_entry = tuple()\n                        while call_backtrack_stack:\n                            call_backtrack_entry = call_backtrack_stack.pop()\n                            if len(call_backtrack_entry) == 4:\n                                break\n                            else:\n                                _, key = call_backtrack_entry\n                                memo[key] = (None, fail_message)\n                        if len(call_backtrack_entry) != 4:\n                            raise MatchError(\n                                latest_fail_message[0].format(*latest_fail_message[1:]),\n                                latest_fail_pos,\n                                stream_pos_stack[0][0] if stream_pos_stack else stream\n                            )\n                        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n                        if len(stream_pos_stack) > stream_stack_len:\n                            stream = stream_pos_stack[stream_stack_len][0]\n                        stream_pos_stack = stream_pos_stack[:stream_stack_len]\n                        if len(scope_stack) > scope_stack_len:\n                            scope = scope_stack[scope_stack_len]\n                        scope_stack = scope_stack[:scope_stack_len]\n                    else:\n                        action, stream_pos_stack = memo[key]\n                        stream_pos_stack = stream_pos_stack[:]\n                        stream, pos = stream_pos_stack.pop()\n                        pc += 1\n                else:\n                    call_backtrack_stack.append((pc+1, key))\n                    pc = labels[fn_name]\n                    pos += 1\n        elif name == FAIL:\n            fail_message = (arg1,)\n            fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])\n            if fail_pos >= latest_fail_pos:\n                latest_fail_message = fail_message\n                latest_fail_pos = fail_pos\n            call_backtrack_entry = tuple()\n            while call_backtrack_stack:\n                call_backtrack_entry = call_backtrack_stack.pop()\n                if len(call_backtrack_entry) == 4:\n                    break\n                else:\n                    _, key = call_backtrack_entry\n                    memo[key] = (None, fail_message)\n            if len(call_backtrack_entry) != 4:\n                raise MatchError(\n                    latest_fail_message[0].format(*latest_fail_message[1:]),\n                    latest_fail_pos,\n                    stream_pos_stack[0][0] if stream_pos_stack else stream\n                )\n            (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry\n            if len(stream_pos_stack) > stream_stack_len:\n                stream = stream_pos_stack[stream_stack_len][0]\n            stream_pos_stack = stream_pos_stack[:stream_stack_len]\n            if len(scope_stack) > scope_stack_len:\n                scope = scope_stack[scope_stack_len]\n            scope_stack = scope_stack[:scope_stack_len]\n        else:\n            raise Exception("unknown instruction {}".format(name))\nclass SemanticAction(object):\n\n    def __init__(self, value, fn=lambda self: self.value):\n        self.value = value\n        self.fn = fn\n\n    def eval(self, runtime):\n        self.runtime = runtime\n        return self.fn(self)\n\n    def bind(self, name, value, continuation):\n        self.runtime = dict(self.runtime, **{name: value})\n        return continuation()\n\n    def lookup(self, name):\n        if name in self.value:\n            return self.value[name].eval(self.runtime)\n        else:\n            return self.runtime[name]\n\nclass MatchError(Exception):\n\n    def __init__(self, message, pos, stream):\n        Exception.__init__(self)\n        self.message = message\n        self.pos = pos\n        self.stream = stream\n\nclass Grammar(object):\n\n    def run(self, rule_name, stream):\n        return vm(self.instructions, self.labels, rule_name, stream).eval({\n            "label": Counter(),\n            "indentprefix": "    ",\n        })\n\nclass Counter(object):\n\n    def __init__(self):\n        self.value = 0\n\n    def __call__(self):\n        result = self.value\n        self.value += 1\n        return result\n\nclass List(list):\n\n    def __call__(self, item):\n        self.append(item)\n\nclass Dict(dict):\n\n    def __call__(self, *args):\n        if len(args) == 1:\n            return self[args[0]]\n        else:\n            self[args[0]] = args[1]\n\ndef splice(depth, item):\n    if depth == 0:\n        return [item]\n    else:\n        return concat([splice(depth-1, subitem) for subitem in item])\n\ndef concat(lists):\n    return [x for xs in lists for x in xs]\n\ndef join(items, delimiter=""):\n    return delimiter.join(\n        join(item, delimiter) if isinstance(item, list) else str(item)\n        for item in items\n    )\n\ndef indent(text, prefix="    "):\n    return "".join(prefix+line for line in text.splitlines(True))\n\ndef compile_chain(grammars, source):\n    import sys\n    import pprint\n    for grammar, rule in grammars:\n        try:\n            source = grammar().run(rule, source)\n        except MatchError as e:\n            stream = e.stream\n            for pos in e.pos[:-1]:\n                stream = stream[pos]\n            pos = e.pos[-1]\n            MARKER = "\\033[0;31m<ERROR POSITION>\\033[0m"\n            if isinstance(stream, str):\n                stream_string = stream[:pos] + MARKER + stream[pos:]\n            else:\n                stream_string = pprint.pformat(stream)\n            sys.exit("ERROR: {}\\nPOSITION: {}\\nSTREAM:\\n{}".format(\n                e.message,\n                pos,\n                indent(stream_string)\n            ))\n    return source\n'
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
        name, arg1, arg2 = instructions[pc]
        if name == PUSH_SCOPE:
            scope_stack.append(scope)
            scope = {}
            pc += 1
        elif name == BACKTRACK:
            call_backtrack_stack.append((
                labels[arg1], pos, len(stream_pos_stack), len(scope_stack)
            ))
            pc += 1
        elif name == CALL:
            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))
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
                    pc += 1
            else:
                call_backtrack_stack.append((pc+1, key))
                pc = labels[arg1]
        elif name == POP_SCOPE:
            scope = scope_stack.pop()
            pc += 1
        elif name == MATCH_OBJECT:
            if pos >= len(stream) or stream[pos] != arg1:
                fail_message = ("expected {!r}", arg1)
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
                action = SemanticAction(arg1)
                pos += 1
                pc += 1
        elif name == COMMIT:
            call_backtrack_stack.pop()
            pc = labels[arg1]
        elif name == RETURN:
            if len(call_backtrack_stack) == 0:
                return action
            pc, key = call_backtrack_stack.pop()
            memo[key] = (action, stream_pos_stack+[(stream, pos)])
        elif name == LIST_APPEND:
            scope.append(action)
            pc += 1
        elif name == BIND:
            scope[arg1] = action
            pc += 1
        elif name == ACTION:
            action = SemanticAction(scope, arg1)
            pc += 1
        elif name == MATCH_RANGE:
            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
                fail_message = ("expected range {!r}-{!r}", arg1, arg2)
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
                pc += 1
        elif name == LIST_START:
            scope_stack.append(scope)
            scope = []
            pc += 1
        elif name == LIST_END:
            action = SemanticAction(scope, lambda self: [x.eval(self.runtime) for x in self.value])
            scope = scope_stack.pop()
            pc += 1
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
                pc += 1
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
                pc += 1
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
                pc += 1
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
                fn_name = str(stream[pos])
                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
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
                        pc += 1
                else:
                    call_backtrack_stack.append((pc+1, key))
                    pc = labels[fn_name]
                    pos += 1
        elif name == FAIL:
            fail_message = (arg1,)
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
            raise Exception("unknown instruction {}".format(name))
class SemanticAction(object):

    def __init__(self, value, fn=lambda self: self.value):
        self.value = value
        self.fn = fn

    def eval(self, runtime):
        self.runtime = runtime
        return self.fn(self)

    def bind(self, name, value, continuation):
        self.runtime = dict(self.runtime, **{name: value})
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

    def run(self, rule_name, stream):
        return vm(self.instructions, self.labels, rule_name, stream).eval({
            "label": Counter(),
            "indentprefix": "    ",
        })

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

    labels = {'file': 0, 0: 2, 1: 9, 2: 15, 3: 16, 'grammar': 19, 4: 25, 5: 29, 'rule': 36, 'choice': 46, 6: 53, 7: 53, 8: 56, 9: 64, 'sequence': 69, 10: 71, 11: 75, 'expr': 82, 12: 93, 13: 96, 'expr1': 97, 14: 106, 16: 115, 18: 124, 20: 131, 21: 134, 19: 134, 17: 134, 15: 134, 'expr2': 135, 24: 145, 25: 146, 22: 149, 26: 160, 30: 165, 32: 170, 33: 171, 31: 175, 28: 181, 34: 188, 36: 199, 38: 203, 39: 207, 37: 213, 35: 213, 29: 213, 27: 213, 23: 213, 'matchChar': 214, 'maybeAction': 220, 42: 225, 43: 229, 40: 234, 41: 237, 'actionExpr': 238, 44: 251, 45: 254, 'hostExpr': 258, 46: 266, 50: 271, 51: 275, 48: 282, 54: 287, 55: 291, 52: 298, 58: 305, 59: 309, 56: 316, 57: 320, 53: 320, 49: 320, 47: 320, 'hostListItem': 321, 60: 324, 61: 328, 'formatExpr': 335, 64: 340, 65: 344, 62: 351, 63: 354, 'var': 355, 66: 364, 68: 373, 69: 374, 67: 376, 'string': 377, 70: 380, 72: 385, 73: 386, 71: 390, 'char': 396, 74: 401, 75: 402, 'innerChar': 408, 76: 414, 77: 417, 'escape': 418, 78: 424, 80: 430, 82: 436, 83: 440, 81: 440, 79: 440, 'name': 441, 84: 446, 85: 450, 'nameStart': 455, 86: 460, 87: 463, 'nameChar': 464, 88: 469, 90: 474, 91: 477, 89: 477, 'space': 478, 92: 480, 94: 486, 95: 489, 93: 491}
    instructions = [
        (PUSH_SCOPE, None, None),
        (LIST_START, None, None),
        (BACKTRACK, 1, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (CALL, 'grammar', None),
        (POP_SCOPE, None, None),
        (LIST_APPEND, None, None),
        (COMMIT, 0, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (CALL, 'space', None),
        (BACKTRACK, 3, None),
        (MATCH_ANY, None, None),
        (COMMIT, 2, None),
        (FAIL, 'no match expected', None),
        (ACTION, lambda self: self.lookup('xs'), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'name', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '{', None),
        (LIST_START, None, None),
        (BACKTRACK, 5, None),
        (CALL, 'rule', None),
        (LIST_APPEND, None, None),
        (COMMIT, 4, None),
        (LIST_END, None, None),
        (BIND, 'ys', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '}', None),
        (ACTION, lambda self: concat([splice(0, 'Grammar'), splice(0, self.lookup('x')), splice(1, self.lookup('ys'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'name', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '=', None),
        (CALL, 'choice', None),
        (BIND, 'y', None),
        (ACTION, lambda self: concat([splice(0, 'Rule'), splice(0, self.lookup('x')), splice(0, self.lookup('y'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (BACKTRACK, 6, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '|', None),
        (POP_SCOPE, None, None),
        (COMMIT, 7, None),
        (CALL, 'sequence', None),
        (BIND, 'x', None),
        (LIST_START, None, None),
        (BACKTRACK, 9, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '|', None),
        (CALL, 'sequence', None),
        (POP_SCOPE, None, None),
        (LIST_APPEND, None, None),
        (COMMIT, 8, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (ACTION, lambda self: concat([splice(0, 'Or'), splice(0, self.lookup('x')), splice(1, self.lookup('xs'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (LIST_START, None, None),
        (BACKTRACK, 11, None),
        (CALL, 'expr', None),
        (LIST_APPEND, None, None),
        (COMMIT, 10, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (CALL, 'maybeAction', None),
        (BIND, 'ys', None),
        (ACTION, lambda self: concat([splice(0, 'Scope'), splice(0, concat([splice(0, 'And'), splice(1, self.lookup('xs')), splice(1, self.lookup('ys'))]))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 12, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'expr1', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, ':', None),
        (CALL, 'name', None),
        (BIND, 'y', None),
        (ACTION, lambda self: concat([splice(0, 'Bind'), splice(0, self.lookup('y')), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 13, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'expr1', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 14, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'expr2', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '*', None),
        (ACTION, lambda self: concat([splice(0, 'Star'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 15, None),
        (BACKTRACK, 16, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'expr2', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '?', None),
        (ACTION, lambda self: concat([splice(0, 'Or'), splice(0, self.lookup('x')), splice(0, concat([splice(0, 'And')]))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 17, None),
        (BACKTRACK, 18, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '!', None),
        (CALL, 'expr2', None),
        (BIND, 'x', None),
        (ACTION, lambda self: concat([splice(0, 'Not'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 19, None),
        (BACKTRACK, 20, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '%', None),
        (ACTION, lambda self: concat([splice(0, 'MatchCallRule')]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 21, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'expr2', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 22, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'name', None),
        (BIND, 'x', None),
        (BACKTRACK, 25, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '=', None),
        (POP_SCOPE, None, None),
        (COMMIT, 24, None),
        (FAIL, 'no match expected', None),
        (ACTION, lambda self: concat([splice(0, 'MatchRule'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 23, None),
        (BACKTRACK, 26, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (CALL, 'char', None),
        (BIND, 'x', None),
        (MATCH_OBJECT, '-', None),
        (CALL, 'char', None),
        (BIND, 'y', None),
        (ACTION, lambda self: concat([splice(0, 'MatchRange'), splice(0, self.lookup('x')), splice(0, self.lookup('y'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 27, None),
        (BACKTRACK, 28, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, "'", None),
        (LIST_START, None, None),
        (BACKTRACK, 31, None),
        (PUSH_SCOPE, None, None),
        (BACKTRACK, 33, None),
        (MATCH_OBJECT, "'", None),
        (COMMIT, 32, None),
        (FAIL, 'no match expected', None),
        (CALL, 'matchChar', None),
        (POP_SCOPE, None, None),
        (LIST_APPEND, None, None),
        (COMMIT, 30, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (MATCH_OBJECT, "'", None),
        (ACTION, lambda self: concat([splice(0, 'And'), splice(1, self.lookup('xs'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 29, None),
        (BACKTRACK, 34, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '.', None),
        (ACTION, lambda self: concat([splice(0, 'MatchAny')]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 35, None),
        (BACKTRACK, 36, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '(', None),
        (CALL, 'choice', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, ')', None),
        (ACTION, lambda self: self.lookup('x'), None),
        (POP_SCOPE, None, None),
        (COMMIT, 37, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '[', None),
        (LIST_START, None, None),
        (BACKTRACK, 39, None),
        (CALL, 'expr', None),
        (LIST_APPEND, None, None),
        (COMMIT, 38, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, ']', None),
        (ACTION, lambda self: concat([splice(0, 'MatchList'), splice(0, concat([splice(0, 'And'), splice(1, self.lookup('xs'))]))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'innerChar', None),
        (BIND, 'x', None),
        (ACTION, lambda self: concat([splice(0, 'MatchObject'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 40, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'actionExpr', None),
        (BIND, 'x', None),
        (LIST_START, None, None),
        (BACKTRACK, 43, None),
        (CALL, 'actionExpr', None),
        (LIST_APPEND, None, None),
        (COMMIT, 42, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (ACTION, lambda self: concat([splice(0, concat([splice(0, 'Action'), splice(1, self.lookup('x')), splice(2, self.lookup('xs'))]))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 41, None),
        (PUSH_SCOPE, None, None),
        (ACTION, lambda self: concat([]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '-', None),
        (MATCH_OBJECT, '>', None),
        (CALL, 'hostExpr', None),
        (BIND, 'x', None),
        (BACKTRACK, 44, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, ':', None),
        (CALL, 'name', None),
        (POP_SCOPE, None, None),
        (COMMIT, 45, None),
        (PUSH_SCOPE, None, None),
        (ACTION, lambda self: '', None),
        (POP_SCOPE, None, None),
        (BIND, 'y', None),
        (ACTION, lambda self: concat([splice(0, self.lookup('y')), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 46, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (CALL, 'string', None),
        (BIND, 'x', None),
        (ACTION, lambda self: concat([splice(0, 'String'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 47, None),
        (BACKTRACK, 48, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '[', None),
        (LIST_START, None, None),
        (BACKTRACK, 51, None),
        (CALL, 'hostListItem', None),
        (LIST_APPEND, None, None),
        (COMMIT, 50, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, ']', None),
        (ACTION, lambda self: concat([splice(0, 'List'), splice(1, self.lookup('xs'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 49, None),
        (BACKTRACK, 52, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '{', None),
        (LIST_START, None, None),
        (BACKTRACK, 55, None),
        (CALL, 'formatExpr', None),
        (LIST_APPEND, None, None),
        (COMMIT, 54, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '}', None),
        (ACTION, lambda self: concat([splice(0, 'Format'), splice(1, self.lookup('xs'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 53, None),
        (BACKTRACK, 56, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'var', None),
        (BIND, 'x', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '(', None),
        (LIST_START, None, None),
        (BACKTRACK, 59, None),
        (CALL, 'hostExpr', None),
        (LIST_APPEND, None, None),
        (COMMIT, 58, None),
        (LIST_END, None, None),
        (BIND, 'ys', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, ')', None),
        (ACTION, lambda self: concat([splice(0, 'Call'), splice(0, self.lookup('x')), splice(1, self.lookup('ys'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 57, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'var', None),
        (BIND, 'x', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (LIST_START, None, None),
        (BACKTRACK, 61, None),
        (MATCH_OBJECT, '~', None),
        (LIST_APPEND, None, None),
        (COMMIT, 60, None),
        (LIST_END, None, None),
        (BIND, 'ys', None),
        (CALL, 'hostExpr', None),
        (BIND, 'x', None),
        (ACTION, lambda self: concat([splice(0, 'ListItem'), splice(0, len(self.lookup('ys'))), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 62, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '>', None),
        (LIST_START, None, None),
        (BACKTRACK, 65, None),
        (CALL, 'formatExpr', None),
        (LIST_APPEND, None, None),
        (COMMIT, 64, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '<', None),
        (ACTION, lambda self: concat([splice(0, 'Indent'), splice(0, concat([splice(0, 'Format'), splice(1, self.lookup('xs'))]))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 63, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'hostExpr', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 66, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '#', None),
        (CALL, 'name', None),
        (BIND, 'x', None),
        (ACTION, lambda self: concat([splice(0, 'Native'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (COMMIT, 67, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'name', None),
        (BIND, 'x', None),
        (BACKTRACK, 69, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (MATCH_OBJECT, '=', None),
        (POP_SCOPE, None, None),
        (COMMIT, 68, None),
        (FAIL, 'no match expected', None),
        (ACTION, lambda self: concat([splice(0, 'Lookup'), splice(0, self.lookup('x'))]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, '"', None),
        (LIST_START, None, None),
        (BACKTRACK, 71, None),
        (PUSH_SCOPE, None, None),
        (BACKTRACK, 73, None),
        (MATCH_OBJECT, '"', None),
        (COMMIT, 72, None),
        (FAIL, 'no match expected', None),
        (CALL, 'innerChar', None),
        (POP_SCOPE, None, None),
        (LIST_APPEND, None, None),
        (COMMIT, 70, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (MATCH_OBJECT, '"', None),
        (ACTION, lambda self: join([self.lookup('xs')]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, "'", None),
        (BACKTRACK, 75, None),
        (MATCH_OBJECT, "'", None),
        (COMMIT, 74, None),
        (FAIL, 'no match expected', None),
        (CALL, 'innerChar', None),
        (BIND, 'x', None),
        (MATCH_OBJECT, "'", None),
        (ACTION, lambda self: self.lookup('x'), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 76, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, '\\', None),
        (CALL, 'escape', None),
        (POP_SCOPE, None, None),
        (COMMIT, 77, None),
        (PUSH_SCOPE, None, None),
        (MATCH_ANY, None, None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 78, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, '\\', None),
        (ACTION, lambda self: '\\', None),
        (POP_SCOPE, None, None),
        (COMMIT, 79, None),
        (BACKTRACK, 80, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, "'", None),
        (ACTION, lambda self: "'", None),
        (POP_SCOPE, None, None),
        (COMMIT, 81, None),
        (BACKTRACK, 82, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, '"', None),
        (ACTION, lambda self: '"', None),
        (POP_SCOPE, None, None),
        (COMMIT, 83, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, 'n', None),
        (ACTION, lambda self: '\n', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'space', None),
        (CALL, 'nameStart', None),
        (BIND, 'x', None),
        (LIST_START, None, None),
        (BACKTRACK, 85, None),
        (CALL, 'nameChar', None),
        (LIST_APPEND, None, None),
        (COMMIT, 84, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (ACTION, lambda self: join([self.lookup('x'), self.lookup('xs')]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 86, None),
        (PUSH_SCOPE, None, None),
        (MATCH_RANGE, 'a', 'z'),
        (POP_SCOPE, None, None),
        (COMMIT, 87, None),
        (PUSH_SCOPE, None, None),
        (MATCH_RANGE, 'A', 'Z'),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 88, None),
        (PUSH_SCOPE, None, None),
        (MATCH_RANGE, 'a', 'z'),
        (POP_SCOPE, None, None),
        (COMMIT, 89, None),
        (BACKTRACK, 90, None),
        (PUSH_SCOPE, None, None),
        (MATCH_RANGE, 'A', 'Z'),
        (POP_SCOPE, None, None),
        (COMMIT, 91, None),
        (PUSH_SCOPE, None, None),
        (MATCH_RANGE, '0', '9'),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (LIST_START, None, None),
        (BACKTRACK, 93, None),
        (BACKTRACK, 94, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, ' ', None),
        (POP_SCOPE, None, None),
        (COMMIT, 95, None),
        (PUSH_SCOPE, None, None),
        (MATCH_OBJECT, '\n', None),
        (POP_SCOPE, None, None),
        (LIST_APPEND, None, None),
        (COMMIT, 92, None),
        (LIST_END, None, None),
        (POP_SCOPE, None, None),
        (RETURN, None, None)
    ]
class CodeGenerator(Grammar):

    labels = {'Grammar': 0, 0: 4, 1: 8, 'Rule': 13, 'Or': 21, 2: 30, 3: 33, 'Scope': 34, 'And': 40, 4: 42, 5: 46, 'Bind': 49, 'Star': 57, 'Not': 63, 'MatchCallRule': 69, 'MatchRule': 73, 'MatchRange': 79, 'MatchAny': 87, 'MatchList': 91, 'MatchObject': 97, 'Action': 103, 'actionExpr': 109, 6: 120, 7: 124, 'String': 125, 'List': 129, 'ListItem': 135, 'Format': 143, 'Indent': 149, 'Call': 155, 'Native': 163, 'Lookup': 167, 'asts': 173, 8: 175, 9: 179, 'ast': 184, 'astList': 192, 10: 194, 11: 198, 'py': 203}
    instructions = [
        (PUSH_SCOPE, None, None),
        (MATCH_ANY, None, None),
        (BIND, 'x', None),
        (LIST_START, None, None),
        (BACKTRACK, 1, None),
        (CALL, 'ast', None),
        (LIST_APPEND, None, None),
        (COMMIT, 0, None),
        (LIST_END, None, None),
        (BIND, 'ys', None),
        (ACTION, lambda self: self.bind('I', List(), lambda: self.bind('LABEL', Dict(), lambda: self.bind('', self.lookup('ys'), lambda: join(['class ', self.lookup('x'), '(Grammar):\n\n', indent(join(['labels = ', repr(self.lookup('LABEL')), '\n', 'instructions = [\n', indent(join([join(self.lookup('I'), ',\n')]), self.lookup('indentprefix')), '\n]\n']), self.lookup('indentprefix'))])))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (MATCH_ANY, None, None),
        (BIND, 'x', None),
        (CALL, 'ast', None),
        (BIND, 'y', None),
        (ACTION, lambda self: self.bind('', self.lookup('LABEL')(self.lookup('x'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('y'), lambda: self.lookup('I')('(RETURN, None, None)'))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 2, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (CALL, 'Or', None),
        (BIND, 'y', None),
        (ACTION, lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: self.bind('', self.lookup('I')(join(['(BACKTRACK, ', self.lookup('a'), ', None)'])), lambda: self.bind('', self.lookup('x'), lambda: self.bind('', self.lookup('I')(join(['(COMMIT, ', self.lookup('b'), ', None)'])), lambda: self.bind('', self.lookup('LABEL')(self.lookup('a'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('y'), lambda: self.lookup('LABEL')(self.lookup('b'), len(self.lookup('I')))))))))), None),
        (POP_SCOPE, None, None),
        (COMMIT, 3, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.bind('', self.lookup('I')('(PUSH_SCOPE, None, None)'), lambda: self.bind('', self.lookup('x'), lambda: self.lookup('I')('(POP_SCOPE, None, None)'))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (LIST_START, None, None),
        (BACKTRACK, 5, None),
        (CALL, 'ast', None),
        (LIST_APPEND, None, None),
        (COMMIT, 4, None),
        (LIST_END, None, None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (CALL, 'ast', None),
        (BIND, 'y', None),
        (ACTION, lambda self: self.bind('', self.lookup('y'), lambda: self.lookup('I')(join(['(BIND, ', self.lookup('x'), ', None)']))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: self.bind('', self.lookup('I')('(LIST_START, None, None)'), lambda: self.bind('', self.lookup('LABEL')(self.lookup('a'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('I')(join(['(BACKTRACK, ', self.lookup('b'), ', None)'])), lambda: self.bind('', self.lookup('x'), lambda: self.bind('', self.lookup('I')('(LIST_APPEND, None, None)'), lambda: self.bind('', self.lookup('I')(join(['(COMMIT, ', self.lookup('a'), ', None)'])), lambda: self.bind('', self.lookup('LABEL')(self.lookup('b'), len(self.lookup('I'))), lambda: self.lookup('I')('(LIST_END, None, None)')))))))))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.bind('a', self.lookup('label')(), lambda: self.bind('b', self.lookup('label')(), lambda: self.bind('', self.lookup('I')(join(['(BACKTRACK, ', self.lookup('b'), ', None)'])), lambda: self.bind('', self.lookup('x'), lambda: self.bind('', self.lookup('I')(join(['(COMMIT, ', self.lookup('a'), ', None)'])), lambda: self.bind('', self.lookup('LABEL')(self.lookup('a'), len(self.lookup('I'))), lambda: self.bind('', self.lookup('I')("(FAIL, 'no match expected', None)"), lambda: self.lookup('LABEL')(self.lookup('b'), len(self.lookup('I')))))))))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (ACTION, lambda self: self.lookup('I')('(MATCH_CALL_RULE, None, None)'), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.lookup('I')(join(['(CALL, ', self.lookup('x'), ', None)'])), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (CALL, 'py', None),
        (BIND, 'y', None),
        (ACTION, lambda self: self.lookup('I')(join(['(MATCH_RANGE, ', self.lookup('x'), ', ', self.lookup('y'), ')'])), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (ACTION, lambda self: self.lookup('I')('(MATCH_ANY, None, None)'), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.bind('', self.lookup('I')('(PUSH_STREAM, None, None)'), lambda: self.bind('', self.lookup('x'), lambda: self.lookup('I')('(POP_STREAM, None, None)'))), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.lookup('I')(join(['(MATCH_OBJECT, ', self.lookup('x'), ', None)'])), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'actionExpr', None),
        (BIND, 'x', None),
        (ACTION, lambda self: self.lookup('I')(join(['(ACTION, lambda self: ', self.lookup('x'), ', None)'])), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (BACKTRACK, 6, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (CALL, 'ast', None),
        (BIND, 'y', None),
        (CALL, 'actionExpr', None),
        (BIND, 'z', None),
        (ACTION, lambda self: join(['self.bind(', self.lookup('x'), ', ', self.lookup('y'), ', lambda: ', self.lookup('z'), ')']), None),
        (POP_SCOPE, None, None),
        (COMMIT, 7, None),
        (PUSH_SCOPE, None, None),
        (MATCH_ANY, None, None),
        (CALL, 'ast', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'astList', None),
        (BIND, 'x', None),
        (ACTION, lambda self: join(['concat([', self.lookup('x'), '])']), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (CALL, 'ast', None),
        (BIND, 'y', None),
        (ACTION, lambda self: join(['splice(', self.lookup('x'), ', ', self.lookup('y'), ')']), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'astList', None),
        (BIND, 'x', None),
        (ACTION, lambda self: join(['join([', self.lookup('x'), '])']), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (ACTION, lambda self: join(['indent(', self.lookup('x'), ', ', "self.lookup('indentprefix'))"]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'ast', None),
        (BIND, 'x', None),
        (CALL, 'astList', None),
        (BIND, 'y', None),
        (ACTION, lambda self: join([self.lookup('x'), '(', self.lookup('y'), ')']), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (MATCH_ANY, None, None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (CALL, 'py', None),
        (BIND, 'x', None),
        (ACTION, lambda self: join(['self.lookup(', self.lookup('x'), ')']), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (LIST_START, None, None),
        (BACKTRACK, 9, None),
        (CALL, 'ast', None),
        (LIST_APPEND, None, None),
        (COMMIT, 8, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (ACTION, lambda self: join([self.lookup('xs')]), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (PUSH_STREAM, None, None),
        (MATCH_CALL_RULE, None, None),
        (BIND, 'x', None),
        (POP_STREAM, None, None),
        (ACTION, lambda self: self.lookup('x'), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (LIST_START, None, None),
        (BACKTRACK, 11, None),
        (CALL, 'ast', None),
        (LIST_APPEND, None, None),
        (COMMIT, 10, None),
        (LIST_END, None, None),
        (BIND, 'xs', None),
        (ACTION, lambda self: join(self.lookup('xs'), ', '), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None),
        (PUSH_SCOPE, None, None),
        (MATCH_ANY, None, None),
        (BIND, 'x', None),
        (ACTION, lambda self: repr(self.lookup('x')), None),
        (POP_SCOPE, None, None),
        (RETURN, None, None)
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
