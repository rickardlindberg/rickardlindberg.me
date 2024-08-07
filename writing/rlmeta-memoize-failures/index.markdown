---
title: 'Memoizing failures in RLMeta'
date: 2020-01-11
tags: rlmeta
---
After reading [Generating a PEG
Parser](https://medium.com/@gvanrossum_83706/generating-a-peg-parser-520057d642a9)
I realized that RLMeta has a flaw in its its memoization logic: It only
memoizes results when a rule matches. If a rule fails, it is not
recorded in the memoization table. Therefore, if that rule is tried
again at the same position, matching has to be tried again, even though
it will fail. In the article, Guido writes

> It is important to cache negative results too - in fact most calls to
> parsing methods will be negative results.

Besides fixing the flaw in the memoization logic, I am also interested
in seeing how it affects performance.

In this article I show how to fix the memoization logic in both the
[optimized version](/writing/optimizing-rlmeta/index.html) and the [vm
based version](/writing/rlmeta-vm/index.html) and show how performance
is affected.

-   [Optimized version](#a9a4166e943a4d2c974c443cef1da5a9)
-   [VM based version](#fe17638693e344ad9e521e55bf0676f3)
-   [Performance measurements](#42ae6babf74d427880f831bd14109002)
-   [Appendix](#042d4832e92c48349da9e4fe0a73f955)
    -   [Support library (optimized)](#ea9e60e51c034b99b32520af593fa2c0)
    -   [Support library (VM)](#46ac5a5f243c4bd69cec58758e92eadd)

[]{#a9a4166e943a4d2c974c443cef1da5a9}Optimized version
------------------------------------------------------

In the optimized version, the memoization table stores tuples with
result, start position, and end position. If a rule fails, a new kind of
value that indicates a failure must be stored. We reuse the same tuple,
but have the result be `None` in case of a failure and have the start
position be the lazy fail message.

The memoization table is both queried and populated in the support
library method `_match_rule`. The fixed version looks like this:

```
1.  optimized
2.  support.py
3.  [match rule]{.cp}
```

```
def _match_rule(self, rule_name):
    key = (rule_name, self._stream.position())
    if key in self._memo:
        if self._memo[key][0] is None:
            self._stream.fail(self._memo[key][1])
        else:
            result, _, self._stream = self._memo[key]
    else:
        try:
            start = self._stream
            result = getattr(self, "_rule_{}".format(rule_name))()
            end = self._stream
            self._memo[key] = (result, start, end)
        except _MatchError as e:
            self._memo[key] = (None, e.lazy_message, None)
            raise
    return result
```

If a memoization entry exists and the result part is `None`, the match
fails immediately with the fail message stored in the entry. If the
result part is not `None` the result is used as before.

If a memoization entry does not exist and the match fails, the failure
is recorded in the memoization table before propagating the exception.
If the match succeeds the result is recorded as before.

The `_MatchError` class is also modified to save the lazy message so
that it can be accessed for storage in the memoization table:

```
1.  optimized
2.  support.py
3.  [memo fail]{.cp}
```

```
def fail(self, stream, lazy_message):
    if stream.position() >= self._latest_stream.position():
        self._latest_stream = stream
        self._latest_lazy_message = lazy_message
    raise _MatchError(self, lazy_message)
```

```
1.  optimized
2.  support.py
3.  [match error]{.cp}
```

```
class _MatchError(Exception):

    def __init__(self, memo, lazy_message):
        Exception.__init__(self)
        self._memo = memo
        self.lazy_message = lazy_message

    def describe(self):
        return self._memo.describe()
```

[]{#fe17638693e344ad9e521e55bf0676f3}VM based version
-----------------------------------------------------

In the VM based version, the memoization table stores tuples with last
action, and stream position stack. If a rule fails, a new kind of value
that indicates a failure must be stored. We reuse the same tuple, but
have the last action be `None` in case of a failure and have the stream
position stack be the failure message.

The memoization table is queried and populated in different places. The
first place it\'s populated is in the `RETURN` instruction:

```
1.  vm
2.  support.py
3.  [RETURN]{.cp}
```

```
if len(call_backtrack_stack) == 0:
    return last_action.eval()
pc, key = call_backtrack_stack.pop()
memo[key] = (last_action, stream_pos_stack+[(stream, pos)])
continue
```

This code is unchanged and I include it here only for context. It
populates the memoization table when a rule has matched. To populate the
memoization table with a failure, the code that pops a backtrack entry
must be modified like this:

```
1.  vm
2.  support.py
3.  [pop backtrack entry]{.cp}
```

```
call_backtrack_entry = tuple()
while call_backtrack_stack:
    call_backtrack_entry = call_backtrack_stack.pop()
    if len(call_backtrack_entry) == 4:
        break
    else:
        _, key = call_backtrack_entry
        memo[key] = (None, fail_message)
```

If the popped item is not a backtrack entry (length of tuple is 4), it
must be a call entry. It means that the given rule has been called, and
it has failed. In that case, `None` is stored in the memoization table
to indicate a failure, along with the fail message as second element.

Next code that queries the memoization table need to be updated. It
happens in the two call instructions `CALL` and `MATCH_CALL_RULE`:

```
1.  vm
2.  support.py
3.  [CALL]{.cp}
```

```
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
```

```
1.  vm
2.  support.py
3.  [MATCH\_CALL\_RULE]{.cp}
```

```
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
```

They are changed to look at the first argument in the memoized entry. If
it is `None`, it indicates a failure. The `fail_message` is then set and
the `continue` statement is skipped:

[]{#42ae6babf74d427880f831bd14109002}Performance measurements
-------------------------------------------------------------

Are the versions that also memoize failures faster? Here are the
performance measurements:

![](image1.png)

<!-- image text -->
<center>
Performance measurements.
</center>

To measure the performance without memoization I commented out the code
that populates the memoization table.

Memoizing failures seems to be much less important than memoizing
matches. But it still improves performance.

[]{#042d4832e92c48349da9e4fe0a73f955}Appendix
---------------------------------------------

### []{#ea9e60e51c034b99b32520af593fa2c0}Support library (optimized)

The rest of the support library for the optimized version:

```
1.  optimized
2.  support.py
```

```
try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

class _Grammar(object):

    def _or(self, matchers):
        original_stream = self._stream
        for matcher in matchers[:-1]:
            try:
                return matcher()
            except _MatchError:
                self._stream = original_stream
        return matchers[-1]()

    def _and(self, matchers):
        result = None
        for matcher in matchers:
            result = matcher()
        return result

    def _star(self, matcher):
        result = []
        while True:
            original_stream = self._stream
            try:
                result.append(matcher())
            except _MatchError:
                self._stream = original_stream
                return _SemanticAction(lambda: [x.eval() for x in result])

    def _not(self, matcher):
        original_stream = self._stream
        try:
            matcher()
        except _MatchError:
            return _SemanticAction(lambda: None)
        else:
            original_stream.fail(lambda: "match found")
        finally:
            self._stream = original_stream

    <<match rule>>

    def _match_range(self, start, end):
        next_objext = self._stream.peek()
        if next_objext >= start and next_objext <= end:
            self._stream = self._stream.advance()
            return _SemanticAction(lambda: next_objext)
        else:
            self._stream.fail(
                lambda: "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)
            )

    def _match_string(self, string):
        next_object = self._stream.peek()
        if next_object == string:
            self._stream = self._stream.advance()
            return _SemanticAction(lambda: string)
        else:
            self._stream.fail(
                lambda: "expected {!r} but found {!r}".format(string, next_object)
            )

    def _match_charseq(self, charseq):
        for char in charseq:
            next_object = self._stream.peek()
            if next_object != char:
                self._stream.fail(
                    lambda: "expected {!r} but found {!r}".format(char, next_object)
                )
            self._stream = self._stream.advance()
        return _SemanticAction(lambda: charseq)

    def _match_any(self):
        next_object = self._stream.peek()
        self._stream = self._stream.advance()
        return _SemanticAction(lambda: next_object)

    def _match_call_rule(self):
        next_object = self._stream.peek()
        self._stream = self._stream.advance()
        return self._match_rule(str(next_object))

    def _match_list(self, matcher):
        original_stream = self._stream
        next_object = self._stream.peek()
        if isinstance(next_object, list):
            self._stream = self._stream.nested(next_object)
            matcher()
            if self._stream.is_at_end():
                self._stream = original_stream.advance()
                return _SemanticAction(lambda: next_object)
        original_stream.fail(lambda: "list match failed")

    def run(self, rule_name, input_object):
        self._memo = _Memo()
        self._stream = _Stream.from_object(self._memo, input_object)
        result = self._match_rule(rule_name).eval()
        if isinstance(result, _Builder):
            return result.build_string()
        else:
            return result

class _Vars(dict):

    def bind(self, name, value):
        self[name] = value
        return value

    def lookup(self, name):
        return self[name]

class _SemanticAction(object):

    def __init__(self, fn):
        self.fn = fn

    def eval(self):
        return self.fn()

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

class _Memo(dict):

    def __init__(self):
        dict.__init__(self)
        self._latest_stream = _ObjectStream(self, [], -1)
        self._latest_lazy_message = lambda: ""

    def describe(self):
        items = []
        for (rule_name, _), (_, start, end) in self.items():
            if end > start:
                items.append((rule_name, start, end))
        items.sort(key=lambda item: (item[2].position(), item[1].position()))
        message = []
        for item in items:
            message.append("matched {: <20} {} -> {}\n".format(*item))
        message.append("\n")
        message.append("ERROR: {}: {}\n".format(
            self._latest_stream,
            self._latest_lazy_message()
        ))
        return "".join(message)

    <<memo fail>>

<<match error>>

class _Stream(object):

    @classmethod
    def from_object(cls, memo, input_object):
        if isinstance(input_object, basestring):
            return _CharStream(memo, input_object, 0)
        else:
            return _ObjectStream(memo, [input_object], 0)

    def __init__(self, memo, objects, index):
        self._memo = memo
        self._objects = objects
        self._index = index

    def fail(self, lazy_message):
        self._memo.fail(self, lazy_message)

    def peek(self):
        if self.is_at_end():
            self.fail(lambda: "not eof")
        return self._objects[self._index]

    def is_at_end(self):
        return self._index >= len(self._objects)

class _CharStream(_Stream):

    def __init__(self, memo, objects, index, line=1, column=1):
        _Stream.__init__(self, memo, objects, index)
        self._line = line
        self._column = column

    def position(self):
        return self._index

    def advance(self):
        if self._objects[self._index] == "\n":
            line = self._line + 1
            column = 1
        else:
            line = self._line
            column = self._column + 1
        return _CharStream(self._memo, self._objects, self._index+1, line, column)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)

class _ObjectStream(_Stream):

    def __init__(self, memo, objects, index, parent=()):
        _Stream.__init__(self, memo, objects, index)
        self._parent_position = parent
        self._position = self._parent_position + (self._index,)

    def position(self):
        return self._position

    def nested(self, input_object):
        return _ObjectStream(self._memo, input_object, 0, self._position)

    def advance(self):
        return _ObjectStream(self._memo, self._objects, self._index+1, self._parent_position)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.position()))
```

### []{#46ac5a5f243c4bd69cec58758e92eadd}Support library (VM)

The rest of the support library for the VM based version:

```
1.  vm
2.  support.py
```

```
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
            <<CALL>>
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
            <<RETURN>>
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
            <<MATCH_CALL_RULE>>
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
        <<pop backtrack entry>>
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
```
