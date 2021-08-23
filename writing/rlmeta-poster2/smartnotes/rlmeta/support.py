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
            "List": List,
            "Dict": Dict,
            "join": join,
            "len": len,
            "repr": repr,
        })).run(rule, stream)

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
