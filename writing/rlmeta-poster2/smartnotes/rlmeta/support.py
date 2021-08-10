class Scope(object):

    def __init__(self, matches, runtime):
        self.matches = matches
        self.runtime = runtime

    def bind(self, name, value, continuation):
        old = self.runtime.get(name, None)
        self.runtime[name] = value
        try:
            return continuation()
        finally:
            self.runtime[name] = old

    def lookup(self, name):
        if name in self.matches:
            return self.matches[name].eval()
        else:
            return self.runtime.get(name, None)

class SemanticAction(object):

    def __init__(self, value, fn=lambda value: value):
        self.value = value
        self.fn = fn

    def eval(self):
        return self.fn(self.value)

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
