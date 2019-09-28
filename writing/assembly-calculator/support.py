def makeNode(name):
    def op(left, right):
        return [name, left, right]
    return op
class Op(object):

    def __init__(self, fn, prec, assoc):
        self.fn = fn
        self.prec = int(prec)
        self.assoc = assoc
def parseOps(expr, items, min_level=0):
    while items and items[0][0].prec >= min_level:
        op, rhs = items.pop(0)
        if op.assoc == "left":
            next_level = op.prec + 1
        else:
            next_level = op.prec
        expr = op.fn(expr, parseOps(rhs, items, next_level))
    return expr
def main():
    grammars = [
        (Parser(), "expr"),
    ]
    try:
        for expr in sys.stdin.read().splitlines():
            for (grammar, rule) in grammars:
                pprint.pprint(expr, width=20)
                box(grammar.__class__.__name__)
                expr = grammar.run(rule, expr)
            pprint.pprint(expr, width=20)
    except _MatchError as e:
        sys.stderr.write(e.describe())

def box(name):
    print("  {}|".format(" "*(len(name)/2)))
    print("=={}==".format("="*len(name)))
    print("  {}".format(name))
    print("=={}==".format("="*len(name)))
    print("  {}|".format(" "*(len(name)/2)))
