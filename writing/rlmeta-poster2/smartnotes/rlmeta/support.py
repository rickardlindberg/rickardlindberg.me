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

    def __init__(self):
        self.instructions = instructions = []
        self.labels = labels = {}
        def I(name, arg1=None, arg2=None):
            instructions.append((name, arg1, arg2))
        def LABEL(name):
            labels[name] = len(instructions)
        self.assemble(I, LABEL)

    def run(self, rule_name, stream):
        self.label_counter = 0
        return vm(self.instructions, self.labels, rule_name, stream, {
            "label": self.next_label,
            "indentprefix": "    ",
        })

    def next_label(self):
        result = self.label_counter
        self.label_counter += 1
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
