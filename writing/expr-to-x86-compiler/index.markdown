---
title: 'DRAFT: Compiling expressions to x86 machine code'
date: 2020-05-20
tags: rlmeta,draft
---
**This is a work in progress that will change. Like to see it finished?
Let me know by sending me an email.**

In [Parsing left associative operators using
RLMeta](/writing/rlmeta-left-associativity/index.html) we looked at how
to parse a subset of mathematical expressions. In this article we will
take it further and compile such expressions to x86 machine code. Thus
creating a real compiler for expressions.

-   [Overview](#969c195716e94c05ad23bd50f7dc061a)
-   [Parser](#a83adf407dbd43929bfbb4ab7f7311e5)
-   [Abstract code generator](#0488143f851349bf876169b04770e6bf)
-   [X86 code generator](#bb76bbbba48d4c23b982e5c90af36d86)
-   [GAS generator](#12e4e39c6b494b30a425f4570e8b8d53)
-   [Assembler](#d3ece5a4ba484033ac47d9190e513acd)
-   [Appendix: All source code](#a047c1b417bc42799170c0e3632cc3f9)
-   [Appendix: RLMeta](#ff74bacb76bf40f082f88738ebc5d8b0)
-   [Appendix: Test script](#7790eca23f484ea7b7430ef607b491c2)

[]{#969c195716e94c05ad23bd50f7dc061a}Overview
---------------------------------------------

The compiler is implemented by transforming an expression in different
stages.

```text
Parser -> Abstract codegen -> X86 codegen -> GAS
                                          -> Assembler
```

The parser turns an expression into an AST, which the abstract code
generator turns it into instructions for an imaginary stack machine,
which the X86 code generator turns into assembly instructions for the
x86 architecture. The final stage either turns those instructions into
textual assembly instructions suitable for compilation with the GNU
assembler (GAS) or assembles them straight into machine code.

The style of this article is inspired by [Chains of meaning in the STEPS
system](http://www.vpri.org/pdf/m2009011_chns_mng.pdf)
([mirror](http://rickardlindberg.me/writing/alan-kay-notes/m2009011_chns_mng.pdf))
where a compiler is explained by explaining all stages in it. Examples
are shown throughout what the input and output of the different stages
are.

[]{#a83adf407dbd43929bfbb4ab7f7311e5}Parser
-------------------------------------------

The parser is the same as in the previous article. It turns an
expression into an AST:

```
1.  parser.rlmeta
```

```rlmeta
Parser {
  expr  = digit:x (op:y digit:z -> [y z])*:xs -> parseOps(x xs)
  digit = '0'-'9':x                           -> int(x)
  op    =
    | '+' -> Op(makeAstNode("ADD") 1 "left")
    | '-' -> Op(makeAstNode("SUB") 1 "left")
    | '*' -> Op(makeAstNode("MUL") 2 "left")
    | '/' -> Op(makeAstNode("DIV") 2 "left")
    | '^' -> Op(makeAstNode("POW") 3 "right")
}
```

The support functions are also defined similarly:

```
1.  support.py
2.  [compiler]{.cp}
```

```py
class Op(object):

    def __init__(self, fn, prec, assoc):
        self.fn = fn
        self.prec = prec
        self.assoc = assoc
```

```
1.  support.py
2.  [compiler]{.cp}
```

```py
def makeAstNode(name):
    def op(left, right):
        return [name, left, right]
    return op
```

```
1.  support.py
2.  [compiler]{.cp}
```

```py
def parseOps(expr, items, min_level=0):
    while items and items[0][0].prec >= min_level:
        op, rhs = items.pop(0)
        if op.assoc == "left":
            next_level = op.prec + 1
        else:
            next_level = op.prec
        expr = op.fn(expr, parseOps(rhs, items, next_level))
    return expr
```

Parsing the simple expression

```text
3
```

looks like this:

```text
3

=V========== Parser ============

3
```

Parsing the slightly more complicated expression

```text
1+2*3
```

looks like this:

```text
1+2*3

=V========== Parser ============

['ADD',
 1,
 ['MUL', 2, 3]]
```

[]{#0488143f851349bf876169b04770e6bf}Abstract code generator
------------------------------------------------------------

The abstract code generator turns an AST into instructions for an
imaginary stack machine:

```
1.  abstractcodegen.rlmeta
```

```rlmeta
AbstractCodeGen {
  expr =
    | [.:name expr:left expr:right] -> [~right ~left name]
    | .:leaf                        -> ["PUSH" leaf]
}
```

The right hand side of an expression is generated before the left hand
side. This order does not matter, but it turns out that it\'s a bit
easier to generate x86 instructions if the right hand side comes first.

Generating stack machine instructions for the simple expression

```text
3
```

looks like this:

```text
3

=V========== Parser ============

3

=V===== AbstractCodeGen ========

['PUSH', 3]
```

Generating stack machine instructions for the slightly more complicated
expression

```text
1+2*3
```

looks like this:

```text
1+2*3

=V========== Parser ============

['ADD',
 1,
 ['MUL', 2, 3]]

=V===== AbstractCodeGen ========

['PUSH',
 3,
 'PUSH',
 2,
 'MUL',
 'PUSH',
 1,
 'ADD']
```

[]{#bb76bbbba48d4c23b982e5c90af36d86}X86 code generator
-------------------------------------------------------

The X86 code generator turns instructions for the imaginary stack
machine into assembly instructions for the x86 architecture.

An assembly instruction is represented as a list with its mnemonic and
operands:

```text
["mnemonic" op1 op2 ..]
```

For an instruction performing a binary operation, `op1` is the
destination and `op2` is the source. The binary operation performed is
therefore the following:

```text
op1 = op1 `operation` op2
```

An operand can be either a constant, a register, or a memory location.

-   `rdi` is the stack pointer where the stack grows downwards (64-bit
    integer)
-   `eax` is a TOS (top of stack) register (32-bit signed integer)
-   We are generating a function according to x64 calling conventions.

**TODO: explain what registers and so on that we use**

```
1.  x86codegen.rlmeta
```

```rlmeta
X86CodeGen {
  expr = [%*:xs] -> [~~xs ["ret"]]
  <<instructions>>
}
```

```
1.  x86codegen.rlmeta
2.  [instructions]{.cp}
```

```rlmeta
ADD = -> [
  ["add" ["reg" "eax"] ["addr" "rdi"]]
  ["sub" ["reg" "rdi"] ["const" 4   ]]
]
```

```
1.  x86codegen.rlmeta
2.  [instructions]{.cp}
```

```rlmeta
SUB = -> [
  ["sub" ["reg" "eax"] ["addr" "rdi"]]
  ["sub" ["reg" "rdi"] ["const" 4   ]]
]
```

```
1.  x86codegen.rlmeta
2.  [instructions]{.cp}
```

```rlmeta
MUL = -> [
  ["imul" ["reg" "eax"] ["addr" "rdi"]]
  ["sub"  ["reg" "rdi"] ["const" 4   ]]
]
```

```
1.  x86codegen.rlmeta
2.  [instructions]{.cp}
```

```rlmeta
DIV = -> [
  ["cdq"                                   ]
  ["idiv" "long" ["addr" "rdi"]            ]
  ["sub"         ["reg"  "rdi"] ["const" 4]]
]
```

```
1.  x86codegen.rlmeta
2.  [instructions]{.cp}
```

```rlmeta
POW = -> [
]
```

```
1.  x86codegen.rlmeta
2.  [instructions]{.cp}
```

```rlmeta
PUSH = .:x -> [
  ["add" ["reg"  "rdi"] ["const" 4    ]]
  ["mov" ["addr" "rdi"] ["reg"   "eax"]]
  ["mov" ["reg"  "eax"] ["const" x    ]]
]
```

Generating assembly instructions for the simple expression

```text
3
```

looks like this:

```text
3

=V========== Parser ============

3

=V===== AbstractCodeGen ========

['PUSH', 3]

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['ret']]
```

Generating assembly instructions for the slightly more complicated
expression

```text
1+2*3
```

looks like this:

```text
1+2*3

=V========== Parser ============

['ADD',
 1,
 ['MUL', 2, 3]]

=V===== AbstractCodeGen ========

['PUSH',
 3,
 'PUSH',
 2,
 'MUL',
 'PUSH',
 1,
 'ADD']

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['imul',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 1]],
 ['add',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['ret']]
```

[]{#12e4e39c6b494b30a425f4570e8b8d53}GAS generator
--------------------------------------------------

The GAS generator turns assembly instructions into textual assembly
instructions suitable for compilation with the GNU assembler (GAS):
`gcc -nostdlib file.s`:

```
1.  gas.rlmeta
```

```rlmeta
GasGen {
  expr = [instr*:xs] -> {
    ".global expr\n"
    "expr:\n"
    xs
  }
  instr =
    | [mnemonic:x op:target op:source] -> { x "  " source "," target "\n" }
    | [mnemonic:x op:arg]              -> { x "  " arg "\n" }
    | [mnemonic:x]                     -> { x "\n" }
  op =
    | ["reg"    .:name] -> { "%" name      }
    | ["addr"   .:name] -> { "(%" name ")" }
    | ["const" .:value] -> { "$" value     }
  mnemonic = .:x size:y -> pad(x y)
  size = "long" -> "l" | -> ""
}
```

```
1.  support.py
```

```py
def pad(text, suffix):
    return (text+suffix).ljust(7)
```

```
1.  driver.c
```

```c
#include <stdlib.h>
#include <stdio.h>

int expr(void* mem);

int main() {
    void* mem = (void*)malloc(32*sizeof(int));
    int result = expr(mem);
    printf("%d\n", result);
}
```

Generating GAS instructions (and compiling and running them) for the
simple expression

```text
3
```

looks like this:

```text
3

=V========== Parser ============

3

=V===== AbstractCodeGen ========

['PUSH', 3]

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['ret']]

=V========== GasGen ============

.global expr
expr:
add      $4,%rdi
mov      %eax,(%rdi)
mov      $3,%eax
ret

=V===== GccCompileAndRun =======

3
```

Generating GAS instructions (and compiling and running them) for the
slightly more complicated expression

```text
1+2*3
```

looks like this:

```text
1+2*3

=V========== Parser ============

['ADD',
 1,
 ['MUL', 2, 3]]

=V===== AbstractCodeGen ========

['PUSH',
 3,
 'PUSH',
 2,
 'MUL',
 'PUSH',
 1,
 'ADD']

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['imul',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 1]],
 ['add',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['ret']]

=V========== GasGen ============

.global expr
expr:
add      $4,%rdi
mov      %eax,(%rdi)
mov      $3,%eax
add      $4,%rdi
mov      %eax,(%rdi)
mov      $2,%eax
imul     (%rdi),%eax
sub      $4,%rdi
add      $4,%rdi
mov      %eax,(%rdi)
mov      $1,%eax
add      (%rdi),%eax
sub      $4,%rdi
ret

=V===== GccCompileAndRun =======

7
```

Generating GAS instructions (and compiling and running them) for the
expression that uses all operators

```text
2^4/2*3-4+5
```

looks like this:

```text
2^4/2*3-4+5

=V========== Parser ============

['ADD',
 ['SUB',
  ['MUL',
   ['DIV',
    ['POW',
     2,
     4],
    2],
   3],
  4],
 5]

=V===== AbstractCodeGen ========

['PUSH',
 5,
 'PUSH',
 4,
 'PUSH',
 3,
 'PUSH',
 2,
 'PUSH',
 4,
 'PUSH',
 2,
 'POW',
 'DIV',
 'MUL',
 'SUB',
 'ADD']

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 5]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['cdq'],
 ['idiv',
  'long',
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['imul',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['sub',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['add',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['ret']]

=V========== GasGen ============

.global expr
expr:
add      $4,%rdi
mov      %eax,(%rdi)
mov      $5,%eax
add      $4,%rdi
mov      %eax,(%rdi)
mov      $4,%eax
add      $4,%rdi
mov      %eax,(%rdi)
mov      $3,%eax
add      $4,%rdi
mov      %eax,(%rdi)
mov      $2,%eax
add      $4,%rdi
mov      %eax,(%rdi)
mov      $4,%eax
add      $4,%rdi
mov      %eax,(%rdi)
mov      $2,%eax
cdq    
idivl    (%rdi)
sub      $4,%rdi
imul     (%rdi),%eax
sub      $4,%rdi
sub      (%rdi),%eax
sub      $4,%rdi
add      (%rdi),%eax
sub      $4,%rdi
ret

=V===== GccCompileAndRun =======

1
```

[]{#d3ece5a4ba484033ac47d9190e513acd}Assembler
----------------------------------------------

The assembler turns assembly instructions into machine code:

```
1.  assembler.rlmeta
```

```rlmeta
Assembler {
  expr   = [instr*:xs]       -> [~~xs]
  instr  = [%:x]             -> x
  add    =
    | reg64:r const:i        -> [0x48 0x83 modRmDirect(r) ~littleEndian(i 1)]
    | reg32:r addr:m         -> []
  sub    =
    | reg64:r const:i        -> []
    | reg32:r addr:m         -> []
  imul   =
    | reg32:r addr:m         -> []
  cdq    =                   -> []
  idiv   =
    | "long" addr:m          -> []
  mov    =
    | addr:m  reg32:r        -> [0x89 modRmAddr(m r)]
    | reg32:r const:i        -> [add(0xb8 r) ~littleEndian(i 4)]
  ret    =                   -> [0xc3]
  addr   = ["addr" reg64n:n] -> n
  reg64  = ["reg"  reg64n:n] -> n
  reg64n =
    | "rdi" -> 7
  reg32  = ["reg"  reg32n:n] -> n
  reg32n =
    | "eax" -> 0
  const  = ["const" .:i]     -> i
}
```

```
1.  support.py
2.  [compiler]{.cp}
```

```py
def modRmDirect(register):
    return 0xc0 | register
```

```
1.  support.py
2.  [compiler]{.cp}
```

```py
def modRmAddr(addrRegister, desinationRegister):
    return 0x00 | (desinationRegister << 3) | addrRegister
```

```
1.  support.py
2.  [compiler]{.cp}
```

```py
def add(x, y):
    return x + y
```

```
1.  support.py
2.  [compiler]{.cp}
```

```py
def littleEndian(number, numBytes):
    if number < 0 or number >= 2**(8*numBytes):
        raise ValueError("{} is not in range [0, max]".format(number))
    values = []
    for i in range(numBytes):
        values.append(number & 0xff)
        number >>= 8
    return values
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
import ctypes
import mmap
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
libc = ctypes.cdll.LoadLibrary(None)
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
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
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
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
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
fn_malloc = libc.malloc
fn_malloc.restype = ctypes.c_void_p
fn_malloc.argtypes = (ctypes.c_size_t,)
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
expr_fn_type = ctypes.CFUNCTYPE(ctypes.c_int, ctypes.c_void_p)
expr_fn = ctypes.cast(code_address, expr_fn_type)
```

```
1.  support.py
2.  [run machine code]{.cp}
```

```py
result = expr_fn(fn_malloc(1024))
```

Generating machine code (and running it) for the simple expression

```text
3
```

looks like this:

```text
3

=V========== Parser ============

3

=V===== AbstractCodeGen ========

['PUSH', 3]

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['ret']]

=V======== Assembler ===========

[72,
 131,
 199,
 4,
 137,
 7,
 184,
 3,
 0,
 0,
 0,
 195]

=V======== X86Runner ===========

3
```

Generating machine code (and running it) for the slightly more
complicated expression

```text
1+2*3
```

looks like this:

```text
1+2*3

=V========== Parser ============

['ADD',
 1,
 ['MUL', 2, 3]]

=V===== AbstractCodeGen ========

['PUSH',
 3,
 'PUSH',
 2,
 'MUL',
 'PUSH',
 1,
 'ADD']

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['imul',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 1]],
 ['add',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['ret']]

=V======== Assembler ===========

[72,
 131,
 199,
 4,
 137,
 7,
 184,
 3,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 2,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 1,
 0,
 0,
 0,
 195]

=V======== X86Runner ===========

1
```

Generating machine code (and running it) for the expression that uses
all operators

```text
2^4/2*3-4+5
```

looks like this:

```text
2^4/2*3-4+5

=V========== Parser ============

['ADD',
 ['SUB',
  ['MUL',
   ['DIV',
    ['POW',
     2,
     4],
    2],
   3],
  4],
 5]

=V===== AbstractCodeGen ========

['PUSH',
 5,
 'PUSH',
 4,
 'PUSH',
 3,
 'PUSH',
 2,
 'PUSH',
 4,
 'PUSH',
 2,
 'POW',
 'DIV',
 'MUL',
 'SUB',
 'ADD']

=V======== X86CodeGen ==========

[['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 5]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 3]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 4]],
 ['add',
  ['reg', 'rdi'],
  ['const', 4]],
 ['mov',
  ['addr', 'rdi'],
  ['reg', 'eax']],
 ['mov',
  ['reg', 'eax'],
  ['const', 2]],
 ['cdq'],
 ['idiv',
  'long',
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['imul',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['sub',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['add',
  ['reg', 'eax'],
  ['addr', 'rdi']],
 ['sub',
  ['reg', 'rdi'],
  ['const', 4]],
 ['ret']]

=V======== Assembler ===========

[72,
 131,
 199,
 4,
 137,
 7,
 184,
 5,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 4,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 3,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 2,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 4,
 0,
 0,
 0,
 72,
 131,
 199,
 4,
 137,
 7,
 184,
 2,
 0,
 0,
 0,
 195]

=V======== X86Runner ===========

2
```

[]{#a047c1b417bc42799170c0e3632cc3f9}Appendix: All source code
--------------------------------------------------------------

Here is all source code needed to turn expressions into x86 machine
code. Code to run machine code is dependent on situation, so it is not
included here.

Parser:

```text
Parser {
  expr  = digit:x (op:y digit:z -> [y z])*:xs -> parseOps(x xs)
  digit = '0'-'9':x                           -> int(x)
  op    =
    | '+' -> Op(makeAstNode("ADD") 1 "left")
    | '-' -> Op(makeAstNode("SUB") 1 "left")
    | '*' -> Op(makeAstNode("MUL") 2 "left")
    | '/' -> Op(makeAstNode("DIV") 2 "left")
    | '^' -> Op(makeAstNode("POW") 3 "right")
}
```

Abstract code generator:

```text
AbstractCodeGen {
  expr =
    | [.:name expr:left expr:right] -> [~right ~left name]
    | .:leaf                        -> ["PUSH" leaf]
}
```

X86 code generator:

```text
X86CodeGen {
  expr = [%*:xs] -> [~~xs ["ret"]]
  ADD = -> [
    ["add" ["reg" "eax"] ["addr" "rdi"]]
    ["sub" ["reg" "rdi"] ["const" 4   ]]
  ]
  SUB = -> [
    ["sub" ["reg" "eax"] ["addr" "rdi"]]
    ["sub" ["reg" "rdi"] ["const" 4   ]]
  ]
  MUL = -> [
    ["imul" ["reg" "eax"] ["addr" "rdi"]]
    ["sub"  ["reg" "rdi"] ["const" 4   ]]
  ]
  DIV = -> [
    ["cdq"                                   ]
    ["idiv" "long" ["addr" "rdi"]            ]
    ["sub"         ["reg"  "rdi"] ["const" 4]]
  ]
  POW = -> [
  ]
  PUSH = .:x -> [
    ["add" ["reg"  "rdi"] ["const" 4    ]]
    ["mov" ["addr" "rdi"] ["reg"   "eax"]]
    ["mov" ["reg"  "eax"] ["const" x    ]]
  ]
}
```

Assembler:

```text
Assembler {
  expr   = [instr*:xs]       -> [~~xs]
  instr  = [%:x]             -> x
  add    =
    | reg64:r const:i        -> [0x48 0x83 modRmDirect(r) ~littleEndian(i 1)]
    | reg32:r addr:m         -> []
  sub    =
    | reg64:r const:i        -> []
    | reg32:r addr:m         -> []
  imul   =
    | reg32:r addr:m         -> []
  cdq    =                   -> []
  idiv   =
    | "long" addr:m          -> []
  mov    =
    | addr:m  reg32:r        -> [0x89 modRmAddr(m r)]
    | reg32:r const:i        -> [add(0xb8 r) ~littleEndian(i 4)]
  ret    =                   -> [0xc3]
  addr   = ["addr" reg64n:n] -> n
  reg64  = ["reg"  reg64n:n] -> n
  reg64n =
    | "rdi" -> 7
  reg32  = ["reg"  reg32n:n] -> n
  reg32n =
    | "eax" -> 0
  const  = ["const" .:i]     -> i
}
```

Support functions:

```text
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
```

[]{#ff74bacb76bf40f082f88738ebc5d8b0}Appendix: RLMeta
-----------------------------------------------------

In this article I use a version of RLMeta that builds upon the VM based
version from [memoizing
failures](/writing/rlmeta-memoize-failures/index.html), but has the
following changes:

-   It supports integers in semantic actions.
-   It support arbitrary many `~` operators.
-   It supports repeating the `%` operator.

I will not explain how I made those changes here. The full source code
is available on
[GitHub](https://github.com/rickardlindberg/rickardlindberg.me/tree/master/writing/expr-to-x86-compiler/rlmeta).

[]{#7790eca23f484ea7b7430ef607b491c2}Appendix: Test script
----------------------------------------------------------

I used the following script to run the examples:

```
1.  compile.sh
```

```sh
set -e

compile() {
    echo "import sys"
    echo "import pprint"
    python rlmeta/rlmeta.py --support
    cat "support.py"
    python rlmeta/rlmeta.py < parser.rlmeta
    python rlmeta/rlmeta.py < abstractcodegen.rlmeta
    python rlmeta/rlmeta.py < x86codegen.rlmeta
    python rlmeta/rlmeta.py < gas.rlmeta
    python rlmeta/rlmeta.py < assembler.rlmeta
    echo "main()"
}

python <(compile) "$@"
```

```
1.  support.py
```

```py
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
```

```
1.  support.py
```

```py
def print_box(name):
    print("")
    print("=V{}==".format(" {} ".format(name).center(28, "=")))
    print("")
```

```
1.  support.py
```

```py
def print_expr(expr):
    if isinstance(expr, str):
        print(expr.strip())
    else:
        pprint.pprint(expr, width=20)
```

```
1.  gas.sh
```

```sh
set -e

cat | bash compile.sh parser acodegen xcodegen gas @expr.s

gcc driver.c expr.s -o expr

echo ""
echo "=V===== GccCompileAndRun ======="
echo ""

./expr
```

```
1.  native.sh
```

```sh
set -e

bash compile.sh parser acodegen xcodegen assembler xrunner
```

```
1.  support.py
```

```py
class X86Runner(object):

    def run(self, name, machine_code):
        <<run machine code>>
        return result
```

```
1.  support.py
```

```py
<<compiler>>
```
