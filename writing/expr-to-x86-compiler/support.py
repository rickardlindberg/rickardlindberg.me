def pad(text, suffix):
    return (text+suffix).ljust(7)
def main():
    grammars = {
        "parser": Parser(),
        "acodegen": AbstractCodeGen(),
        "xcodegen": X86CodeGen(),
        "gas": GasGen(),
        "assembler": Assembler(),
        "xrunner": X86Runner(),
    }
    try:
        expr = sys.stdin.read().strip()
        first = True
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
def print_box(name):
    print("")
    print("=V{}==".format(" {} ".format(name).center(28, "=")))
    print("")
def print_expr(expr):
    if isinstance(expr, str):
        print(expr.strip())
    else:
        pprint.pprint(expr, width=20)
class X86Runner(object):

    def run(self, name, machine_code):
        import ctypes
        import mmap
        libc = ctypes.cdll.LoadLibrary(None)
        fn_mmap = libc.mmap
        fn_mmap.restype = ctypes.c_void_p
        fn_mmap.argtypes = (
            ctypes.c_void_p,
            ctypes.c_size_t,
            ctypes.c_int,
            ctypes.c_int,
            ctypes.c_int,
            ctypes.c_size_t,
        )
        code = "".join([chr(x) for x in machine_code])
        code_address = fn_mmap(
            None,
            len(code),
            mmap.PROT_READ|mmap.PROT_WRITE|mmap.PROT_EXEC,
            mmap.MAP_PRIVATE|mmap.MAP_ANONYMOUS,
            -1,
            0
        )
        ctypes.memmove(code_address, code, len(code))
        fn_malloc = libc.malloc
        fn_malloc.restype = ctypes.c_void_p
        fn_malloc.argtypes = (ctypes.c_size_t,)
        expr_fn_type = ctypes.CFUNCTYPE(ctypes.c_int, ctypes.c_void_p)
        expr_fn = ctypes.cast(code_address, expr_fn_type)
        result = expr_fn(fn_malloc(1024))
        return result
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
