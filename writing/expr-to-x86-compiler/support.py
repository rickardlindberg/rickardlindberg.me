class Op(object):

    def __init__(self, fn, prec, assoc):
        self.fn = fn
        self.prec = prec
        self.assoc = assoc

def makeAstNode(name):
    def op(left, right):
        return [name, left, right]
    return op

def parseOps(expr, items, min_level=0):
    while items and items[0][0].prec >= min_level:
        op, rhs = items.pop(0)
        if op.assoc == "left":
            next_level = op.prec + 1
        else:
            next_level = op.prec
        expr = op.fn(expr, parseOps(rhs, items, next_level))
    return expr

def pad(text, suffix):
    return (text+suffix).ljust(7)

def modRmDirect(register):
    return 0xc0 | register

def modRmAddr(addrRegister, desinationRegister):
    return 0x00 | (desinationRegister << 3) | addrRegister

def add(x, y):
    return x + y

def littleEndian(number, numBytes):
    if number < 0 or number >= 2**(8*numBytes):
        raise ValueError("{} is not in range [0, max]".format(number))
    values = []
    for i in range(numBytes):
        values.append(number & 0xff)
        number >>= 8
    return values
def main():
    grammars = {
        "parser": Parser(),
        "acodegen": AbstractCodeGen(),
        "xcodegen": X86CodeGen(),
        "gas": GasGen(),
        "assembler": Assembler(),
    }
    try:
        expr = sys.stdin.read().strip()
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
    print("")
    print("=V{}==".format(" {} ".format(name).center(28, "=")))
    print("")
