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
def flatten(xs):
    result = []
    for x in xs:
        result.extend(x)
    return result
def pad(text):
    return text.ljust(7)
def main():
    grammars = {
        "parser": Parser(),
        "stackmachine": StackMachine(),
        "assembly": Assembly(),
        "gnu": GNU(),
    }
    try:
        for index, expr in enumerate(sys.stdin.read().splitlines()):
            if index > 0:
                print("")
                print("-"*30)
                print("")
            for grammar_name in sys.argv[1:]:
                if grammar_name.startswith("@"):
                    with open(grammar_name[1:], "w") as f:
                        f.write(str(expr))
                    continue
                grammar = grammars[grammar_name]
                print_expr(expr)
                print_box(grammar.__class__.__name__)
                expr = grammar.run("expr", expr)
            print_expr(expr)
    except _MatchError as e:
        sys.stderr.write(e.describe())

def print_expr(expr):
    if isinstance(expr, str):
        print(expr.strip())
    else:
        pprint.pprint(expr, width=20)

def print_box(name):
    HALF = 10
    WIDTH = HALF*2+1
    print("")
    print("{}V{}".format("="*HALF, "="*HALF))
    print(name.center(WIDTH))
    print("{}V{}".format("="*HALF, "="*HALF))
    print("")