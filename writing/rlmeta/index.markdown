---
title: 'A meta approach to implementing programming languages'
date: 2018-12-02
tags: rlmeta,favourite
---
*28 May 2019: Added link to next article in* [*Putting it
together*](#2c78e9d9104c4bddbdd1dfe6314506c9).

How does the computer know what to do with the following expression?

```text
1+2*3
```

How does it know how to recognize the sequence of characters as an
arithmetic expression? How does it know that `2*3` should be computed
first? How does it translate it to instructions that execute on the CPU?

In this article I present a metalanguage that I\'ve developed that can
help answer those questions.

-   [Metalanguages](#3f00cf52d91a411d941312539a15cc32)
-   [Interpreting expressions](#ee16dcaf86a7402e9ca78edc620caacb)
-   [Compiling expressions](#61e10d1de6624255a4406572fef4b413)
-   [RLMeta implementation](#f5f122c94d3d4fa0b3a7e64a7fa0a724)
    -   [Parser](#1e01a8bdd22d48f2a7e8533d552bc264)
        -   [Grammar](#a757df1ad1764cc996d9d3e0a0ce4d25)
        -   [Rule](#e1799ddc14ce4a7f83c8f390f9bf8720)
        -   [Choice](#c2a43ebb9a7d477f8dd7126f94bd33de)
        -   [Sequence](#08818eb200d0482f8feb9104e1ae2ea7)
        -   [Expression](#5fca8ddb3f88457bbe5217fa1ebf4383)
        -   [Expression of level 1](#cdeac4c7bcb84261a3d194c1183c9dd7)
        -   [Expression of level 2](#4e76262e4c9841db8c0b79401ac7a8ff)
        -   [Host expression](#768d143c206146ca8efba292c6e9169b)
        -   [Character related](#2057abb618bf4209950acd87fa49da5d)
        -   [Name](#19acecc9fbc44023a69cea9eadbe734d)
        -   [Space](#d729731e037a4a5aafa6da77d2b8bdb1)
    -   [Code generator](#c3f29fa523f341a487ebc07209c471f9)
        -   [Note on target language](#b225bd1638614a808495e33a63797beb)
        -   [Parsing algorithm](#69841ad3d1044db18591fe09cdf1caee)
        -   [Structure of code
            generator](#0d8a472a5c844a729b866f2c2c64794b)
        -   [Grammar](#ed6931f4eeca4d43aaee1e9f5485a295)
        -   [Rule](#3c542ba5b0104273805b227aeab84c04)
        -   [Or](#696631bc029c47949b2ad97ce78ea32c)
        -   [Scope](#86438bda56d342bc9d1f2c62e632a72d)
        -   [And](#487e675a013c45e3aee8b6c068e226df)
        -   [Bind](#4309ca572367401bb6c4561f273b8c85)
        -   [Star](#244f0754be514ae88e0f581f3ab58c59)
        -   [Not](#44d3b166c62d43e38d2a781fb9f06b6d)
        -   [Semantic action](#07530ffd21784561bd8594c98fcd050a)
        -   [Match rule](#c10aabed8057404e97ef8cd6ac1b113d)
        -   [Match range](#3ba5893bb7094ece96e853f869df9456)
        -   [Match string](#82022cc15d944579825b52559b1ee469)
        -   [Match character
            sequence](#3eff70b8911e4b4e9bd520a7d25624e7)
        -   [Match any](#5edd25f746a94b8392834343dce57370)
        -   [Match list](#c3b58d2d7b024fa4a0e5ff0bcf06d154)
        -   [String](#74804e4e0ee643ac95210d7aa17ae7c6)
        -   [List](#03d7a70d44fe4470967acd15d51ad56a)
        -   [Builder](#166dffacb2ca4911908584d77f30b21f)
        -   [Function call](#f5372dab8e084ca386297eb9575052e8)
        -   [Variable lookup](#d9f73bdeb5c447dbaf5b8fe0bf3b67ba)
        -   [Final support methods](#4397d20868fa4c08ac289e463725f18e)
    -   [Putting it together](#2c78e9d9104c4bddbdd1dfe6314506c9)
    -   [Bootstrapping](#313b147a2f574ea09d76d9c7371bdf18)
-   [Implementing programming
    languages](#5f558623cfce441fb3e61033299d1419)
-   [Resources](#4c6af1a11ed440d2bf31a497032c9c0b)
-   [Code listings for RLMeta](#388bb1e8ccbd4d55b89b391c08452c33)
    -   [parser.rlmeta](#a56c54f42c00473091d7c8295ff4e0f1)
    -   [codegenerator.rlmeta](#d9ea64bbdad3465897667ebec9d5ace1)
    -   [support.py](#983e606587034a0a9c8e8b5c714e7ef8)
    -   [compile.sh](#193d8f0ff47f4edcb201139df8cd9520)

[]{#3f00cf52d91a411d941312539a15cc32}Metalanguages
--------------------------------------------------

Metalanguages are used to reason about languages. In metalanguages you
can make statements about statements in a different language.

The metalanguage I\'ve developed is called RLMeta. It is inspired by a
metalanguage from the sixties called [META
II](https://en.wikipedia.org/wiki/META_II). I wanted to develop my own
version of META II to understand it deeply. RLMeta is also inspired by
[OMeta](https://en.wikipedia.org/wiki/OMeta) (another META II
derivative).

RLMeta is a programming language in which you write grammars. Grammars
have rules that specify how to match objects from an input stream and
specify what should happen when objects are matched. The RLMeta compiler
translates grammars into programs that recognize the objects specified
in the grammar and evaluates the semantic actions when the objects are
matched.

![](image1.png)

<!-- image text -->
<center>
Overview of RLMeta compiler.
</center>

[]{#ee16dcaf86a7402e9ca78edc620caacb}Interpreting expressions
-------------------------------------------------------------

How can RLMeta be used to give meaning to arithmetic expressions of the
kind presented in the introductory example? Here is a grammar:

```
1.  calculator
2.  calculator.rlmeta
```

```
Calculator {
  expression =
    | additive
  additive =
    | multitive:x '+' additive:y -> add(x y)
    | multitive
  multitive =
    | digit:x '*' multitive:y    -> mul(x y)
    | digit
  digit =
    | '0'-'9':x                  -> int(x)
}
```

This grammar is called `Calculator`. It has four rules. The first rule
says that an expression is an additive. The second rule says that an
additive is either a multitive followed by the character \'+\' followed
by another additive or just a multitive. The second case is only tried
if the first does not match. The third rule says that a multitive is
either a digit followed by the character \'\*\' followed by another
multitive or just a digit. The fourth rule says that a digit is a
character in the range 0-9. The `:` followed by a name binds the result
of a match to a variable. The expressions to the right of `->` are
semantic actions. They specify what should happen on a match. They can
refer to variables. In this grammar they say that whenever an additive
is matched, call the host language function `add` with the left and
right side, and whenever a multitive is matched, call the host language
function `mul` with the left and right side, and whenever a digit is
matched, call the host language function `int` with the digit character.
The host language function `int` converts a digit character to an
integer and the `add` and `mul` functions perform addition and
multiplication.

This grammar describes how to recognize an arithmetic expression in a
sequence of characters. Precedence is encoded by the order of the rules.
An additive is `x1 + x2 + x3 + ..` where the xes are multitives.
Therefore multiplication is performed before addition. It gives meaning
to the expression by calling host language functions when parts are
matched.

When the calculator grammar is fed to the RLMeta compiler, a program is
output that is an interpreter for arithmetic expressions.

![](image2.png)

<!-- image text -->
<center>
Overview of calculator compilation.
</center>

More specifically, this program is a Python class that implements
interpretation of arithmetic expressions. The class depends on a support
library and also on the host language functions that were called from
the grammar (`add`, `mul`, and `int`). Host language refers to the
language that grammars are compiled to. In this case Python. The pieces
must be assembled to form an executable program. Here is a template for
the final Python file that implements a read-eval-print loop (REPL) for
arithmetic expressions:

```
1.  calculator
2.  compile.sh
3.  [python file template]{.cp}
```

```
from operator import add, mul

$support_py

$calculator_py

if __name__ == "__main__":
    calculator = Calculator()
    while True:
        line = raw_input("> ")
        result = calculator.run("expression", line)
        print(result)
```

First the host language functions are imported (`int` is always
available). Then the support library and the compiled calculator grammar
snippets are inserted. Finally the main method which is the REPL is
implemented. Compiled grammars are used by instantiating them and
calling their `run` method with the name of the rule and the input
object. This template is rendered with a Bash script:

```
1.  calculator
2.  compile.sh
```

```
#!/bin/bash

set -e

cd "$(dirname "$0")"

support_py=$(python ../rlmeta/rlmeta.py --support)
calculator_py=$(python ../rlmeta/rlmeta.py < calculator.rlmeta)

cat <<EOF
<<python file template>>
EOF
```

First the `set -e` directive ensures that the script exits as soon as
there is an error. Then the directory is changed to the one where the
compile script lives. Then the output of two calls to the RLMeta
compiler (`rlmeta.py`) are captured in two variables. The RLMeta
compiler reads a grammar from stdin and writes a Python class to stdout.
If the `--support` flag is given, it writes the support library to
stdout instead. The `command < file` syntax redirects the contents of
the file to the command\'s stdin. Finally the Python file template is
rendered and written to stdout using a [here
document](https://en.wikipedia.org/wiki/Here_document#Unix_shells) which
has access to the previously captured variables.

Example usage on the command line:

```text
$ python <(./calculator/compile.sh)
> 1+2*3
7
```

The compile script writes a Python file to stdout. The `<(command)`
syntax is [process
substitution](https://en.wikipedia.org/wiki/Process_substitution) and
turns the output of the command into a temporary file which can then be
run with Python.

In this example, the input stream to the calculator becomes a list of
characters:

```text
['1', '+', '2', '*', '3']
```

When the calculator matches the expression, the following host language
functions will be called:

```text
add(int('1'), mul(int('2'), int('3')))
```

[]{#61e10d1de6624255a4406572fef4b413}Compiling expressions
----------------------------------------------------------

The previous example relied on host language functions to perform
addition and multiplication. The meaning of an expression was defined in
terms of the meaning of Python functions. To understand what an
expression means, you need to understand how Python implements those
functions. The next example compiles an expression down to a kind of
assembly language that eliminates the need for Python.

For this compilation, two grammars are written: a parser and a code
generator. The parser looks similar to the calculator grammar but
instead of evaluating the expression, it creates an abstract syntax tree
(AST) describing the expression:

```
1.  expression
2.  parser.rlmeta
```

```
Parser {
  expression =
    | additive
  additive =
    | multitive:x '+' additive:y -> ["add" x y]
    | multitive
  multitive =
    | digit:x '*' multitive:y    -> ["mul" x y]
    | digit
  digit =
    | '0'-'9':x                  -> ["digit" x]
}
```

The bracket notation in the semantic actions creates lists. Nodes in the
AST are represented as lists where the first item is a string denoting
the type of node.

The code generator takes as input an AST from the parser and generates
assembly language code for an imaginary stack machine:

```
1.  expression
2.  codegenerator.rlmeta
```

```
CodeGenerator {
  ast =
    | ["add" ast:x ast:y] -> { x y "add"     "\n" }
    | ["mul" ast:x ast:y] -> { x y "mul"     "\n" }
    | ["digit" .:x]       -> {     "push " x "\n" }
}
```

This grammar has only one rule: `ast`. It says that an AST is either a
list that starts with the string \'add\', or a list that starts with the
string \'mul\', or a list that starts with the string \'digit\'. The add
and mul cases recursively match AST nodes as their left and right side
whereas the digit matches anything (`.`) which is the digit stored in
the AST node. The semantic actions in this grammar generate string
output which is denoted by the curly braces. When a digit AST node is
matched, the string \'push \[digit\]\\n\' is generated. It instructs the
stack machine to push the given digit to the stack. For add and mul,
instructions for the operands are first output followed by an \'add\\n\'
or \'mul\\n\' instruction. They instruct the stack machine to pop two
numbers off the stack, add or multiply them, and push the result.

This grammar describes how to recognize and AST in a sequence of
objects. It gives meaning to the AST by generating assembly language
code when AST nodes are matched.

When the expression grammars are fed to the RLMeta compiler, programs
are output whose combination is a compiler for arithmetic expressions.

![](image3.png)

<!-- image text -->
<center>
Overview of expression compilation.
</center>

Here is a template for the final Python file that implements a REPL for
arithmetic expression compilation:

```
1.  expression
2.  compile.sh
3.  [python file template]{.cp}
```

```
import sys

$support_py

$parser_py

$codegenerator_py

if __name__ == "__main__":
    parser = Parser()
    codegenerator = CodeGenerator()
    while True:
        line = raw_input("> ")
        ast = parser.run("expression", line)
        assembly = codegenerator.run("ast", ast)
        sys.stdout.write(assembly)
```

First the `sys` module is imported because the main method needs it.
Then the support library, compiled parser grammar, and compiled code
generator grammar snippets are inserted. Finally the main method which
is the REPL is implemented. The output of the parser is fed to the code
generator and its output is finally written to stdout. This template is
rendered with a Bash script:

```
1.  expression
2.  compile.sh
```

```
#!/bin/bash

set -e

cd "$(dirname "$0")"

support_py=$(python ../rlmeta/rlmeta.py --support)
parser_py=$(python ../rlmeta/rlmeta.py < parser.rlmeta)
codegenerator_py=$(python ../rlmeta/rlmeta.py < codegenerator.rlmeta)

cat <<EOF
<<python file template>>
EOF
```

First the `set -e` directive ensures that the script exits as soon as
there is an error. Then the directory is changed to the one where the
compile script lives. Then the output of three calls to the RLMeta
compiler are captured in three variables. Finally the Python file
template is rendered and written to stdout.

Example usage on the command line:

```text
$ python <(./expression/compile.sh)
> 1+2*3
push 1
push 2
push 3
mul
add
> 1*2+3
push 1
push 2
mul
push 3
add
```

In the first example, the input stream to the parser becomes a list of
characters (same as for the calculator):

```text
['1', '+', '2', '*', '3']
```

The input stream to the code generator becomes a list with a single
object which is a list (the root AST node):

```text
[
    [
        'add',
        ['digit', '1'],
        [
            'mul',
            ['digit', '2'],
            ['digit', '3']
        ]
    ]
]
```

The generated assembly language code is much closer to CPU instructions
than the Python-based interpreter. A grammar could be written to convert
these assembly instructions to assembly instructions of a real CPU. But
I will not do that here. The point is that the meaning of an expression
can be described by a series of transformations that eventually output
machine instructions.

[]{#f5f122c94d3d4fa0b3a7e64a7fa0a724}RLMeta implementation
----------------------------------------------------------

So far I\'ve just given informal descriptions of how RLMeta works. To
fully understand how arithmetic expressions are evaluated and compiled,
you need to understand how RLMeta is implemented.

The RLMeta compiler translates a grammar to a Python class. This
translation is implemented in RLMeta itself. A grammar is, like an
expression, translated in two steps: the parser translates grammar
syntax to an AST and the code generator translates the AST to a Python
class. The generated Python class depends on a support library.

![](image4.png)

<!-- image text -->
<center>
RLMeta compiler internals illustrated.
</center>

The RLMeta compiler thus comprises three pieces: the parser, the code
generator, and the support library.

### []{#1e01a8bdd22d48f2a7e8533d552bc264}Parser

This section defines the parser:

```
1.  rlmeta
2.  parser.rlmeta
```

```
Parser {
  <<rules>>
}
```

#### []{#a757df1ad1764cc996d9d3e0a0ce4d25}Grammar

The top level syntactic element is a grammar. A grammar has a
[*name*](#19acecc9fbc44023a69cea9eadbe734d) followed by
[*rules*](#e1799ddc14ce4a7f83c8f390f9bf8720) enclosed in curly braces.
When this is matched, a `Grammar` AST node is created containing the
name of the grammar and the rule AST nodes:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
grammar =
  | name:x space '{' rule*:ys space '}' -> ["Grammar" x ~ys]
```

Throughout the parser, [*space*](#d729731e037a4a5aafa6da77d2b8bdb1) is
ignored. As a rule of thumb, it is inserted before matching a character
sequence (and not before matching other rules).

The `*` operator after `rule` means match the preceding expression zero
or more times. The result is a list.

The `~` operator in the semantic action means splice the list in-line
into the enclosing list. The `Grammar` AST node thus has the name as
element one, the first rule AST node as element two, the second rule AST
node as element three, and so on.

#### []{#e1799ddc14ce4a7f83c8f390f9bf8720}Rule

A rule has a [*name*](#19acecc9fbc44023a69cea9eadbe734d) followed by an
equal sign followed by a [*choice*](#c2a43ebb9a7d477f8dd7126f94bd33de).
When this is matched, a `Rule` AST node is created containing the name
and the choice AST node:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
rule =
  | name:x space '=' choice:y -> ["Rule" x y]
```

#### []{#c2a43ebb9a7d477f8dd7126f94bd33de}Choice

A choice has [*sequences*](#08818eb200d0482f8feb9104e1ae2ea7) separated
by vertical bars. Optionally the first sequence can start with a
vertical bar to allow all sequence lines to look the same. When this is
matched, an `Or` AST node is created containing the sequence AST nodes:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
choice =
  | (space '|')?
    sequence:x (space '|' sequence)*:xs -> ["Or" x ~xs]
```

The `?` operator means match the preceding expression zero or one time.

The result of `xs` is a list of sequences since the expression inside
parenthesis returns the last match (which is a sequence).

#### []{#08818eb200d0482f8feb9104e1ae2ea7}Sequence

A sequence has one or more
[*expressions*](#5fca8ddb3f88457bbe5217fa1ebf4383). When this is
matched, a `Scope` AST node is created containing an `And` AST node
containing the expression AST nodes:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
sequence =
  | expr:x expr*:xs -> ["Scope" ["And" x ~xs]]
```

#### []{#5fca8ddb3f88457bbe5217fa1ebf4383}Expression

An expression is one of the following sequences:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
expr =
  | expr1:x space ':' name:y -> ["Bind" y x]
  | expr1
```

The first sequence is an [*expression of level
1*](#cdeac4c7bcb84261a3d194c1183c9dd7) followed by a colon followed by a
[*name*](#19acecc9fbc44023a69cea9eadbe734d). When this is matched, a
`Bind` AST node is created containing the name and the expression AST
node.

The second sequence is an [*expression of level
1*](#cdeac4c7bcb84261a3d194c1183c9dd7).

#### []{#cdeac4c7bcb84261a3d194c1183c9dd7}Expression of level 1

An expression of level 1 is one of the following sequences:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
expr1 =
  | expr2:x space '*' -> ["Star" x]
  | expr2:x space '?' -> ["Or" x ["And"]]
  | space '!' expr2:x -> ["Not" x]
  | expr2
```

The first sequence is an [*expression of level
2*](#4e76262e4c9841db8c0b79401ac7a8ff) followed by an asterisk. When
this is matched, a `Star` AST node is created containing the expression
AST node.

The second sequence is an [*expression of level
2*](#4e76262e4c9841db8c0b79401ac7a8ff) followed by a question mark. When
this is matched, an `Or` AST node is created containing the expression
AST node and an empty `And` AST node. There is no dedicated AST node for
the `?` operator, but it is equivalent to matching the expression or
zero expressions and\'ed.

The third sequence is an exclamation mark followed by an [*expression of
level 2*](#4e76262e4c9841db8c0b79401ac7a8ff). When this is matched, a
`Not` AST node is created containing the expression AST node.

The fourth sequence is an [*expression of level
2*](#4e76262e4c9841db8c0b79401ac7a8ff).

#### []{#4e76262e4c9841db8c0b79401ac7a8ff}Expression of level 2

An expression of level 2 is one of the following sequences:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
expr2 =
  | space '->' hostExpr:x        -> ["SemanticAction" x]
  | name:x !(space '=')          -> ["MatchRule" x]
  | space char:x '-' char:y      -> ["MatchRange" x y]
  | space string:x               -> ["MatchString" x]
  | space charseq:x              -> ["MatchCharseq" x]
  | space '.'                    -> ["MatchAny"]
  | space '(' choice:x space ')' -> x
  | space '[' expr*:xs space ']' -> ["MatchList" ["And" ~xs]]
```

The first sequence is the characters \'-\>\' followed by a [*host
expression*](#768d143c206146ca8efba292c6e9169b). When this is matched, a
`SemanticAction` AST node is created containing the expression AST node.

The second sequence is a [*name*](#19acecc9fbc44023a69cea9eadbe734d)
that is not followed by an equal sign (otherwise it would also match the
start of a rule). When this is matched, a `MatchRule` AST node is
created containing the name.

The third sequence is a [*character*](#2057abb618bf4209950acd87fa49da5d)
followed by a dash followed by another character. When this is matched,
a `MatchRange` AST node is created containing the two characters.

The fourth sequence is a [*string*](#2057abb618bf4209950acd87fa49da5d).
When this is matched, a `MatchString` AST node is created containing the
string.

The fifth sequence is a [*character
sequence*](#2057abb618bf4209950acd87fa49da5d). When this is matched, a
`MatchCharseq` AST node is created containing the character sequence.

The sixth sequence is a dot. When this is matched, a `MatchAny` AST node
is created.

The seventh sequence is an open parenthesis followed by a
[*choice*](#c2a43ebb9a7d477f8dd7126f94bd33de) followed by a closing
parenthesis. When this is matched, the choice AST node is returned.

The eighth sequence is an open bracket followed by
[*expressions*](#5fca8ddb3f88457bbe5217fa1ebf4383) followed by a closing
bracket. When this is matched, a `MatchList` AST node is created
containing an `And` AST node containing the expressions.

#### []{#768d143c206146ca8efba292c6e9169b}Host expression

A host expression is one of the following sequences:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
hostExpr =
  | space string:x                           -> ["String" x]
  | space '[' hostExprListItem*:xs space ']' -> ["List" ~xs]
  | space '{' buildExpr*:xs space '}'        -> ["Builder" ~xs]
  | name:x space '(' hostExpr*:ys space ')'  -> ["FnCall" x ~ys]
  | name:x                                   -> ["VarLookup" x]
```

The first sequence is a [*string*](#2057abb618bf4209950acd87fa49da5d).
When this is matched, a `String` AST node is created containing the
string.

The second sequence is an open bracket followed by host expression list
items followed by a closing bracket. When this is matched, a `List` AST
node is created containing the list item AST nodes.

A list item is either a host expression preceded by the `~` operator, in
which case a `ListItemSplice` AST node is created containing the
expression, or a host expression:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
hostExprListItem =
  | space '~' hostExpr:x -> ["ListItemSplice" x]
  | hostExpr
```

The third sequence is an open curly brace followed by build expressions
followed by a closing curly brace. When this is matched, a `Builder` AST
node is created containing the expression AST nodes.

A build expression is either a greater than character, in which case an
`IndentBuilder` AST node is created, or a less than character, in which
case a `DedentBuilder` AST node is created, or a host expression.

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
buildExpr =
  | space '>' -> ["IndentBuilder"]
  | space '<' -> ["DedentBuilder"]
  | hostExpr
```

The fourth sequence is a [*name*](#19acecc9fbc44023a69cea9eadbe734d)
followed by an open parenthesis followed by host expressions followed by
a closing parenthesis. When this is matched, a `FnCall` AST node is
created containing the name and expression AST nodes.

The fifth sequence is a [*name*](#19acecc9fbc44023a69cea9eadbe734d).
When this is matched, a `VarLookup` AST node is created containing the
name.

#### []{#2057abb618bf4209950acd87fa49da5d}Character related

Character related rules capture strings, character sequences, and single
characters. Inside all of them a few escape codes are possible. When
this is matched, the characters inside the delimiters are joined
together to create the string:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
string    = '"'  (!'"'  innerChar)*:xs '"'  -> join(xs)
charseq   = '\'' (!'\'' innerChar)*:xs '\'' -> join(xs)
char      = '\''  !'\'' innerChar  :x  '\'' -> x
innerChar = '\\' escape | .
escape    = '\\' -> "\\" | '\'' -> "'"
          | '"'  -> "\"" | 'n'  -> "\n"
```

#### []{#19acecc9fbc44023a69cea9eadbe734d}Name

A name has at least one alphabetic character followed by any number of
alphanumeric characters. When this is matched, the individual characters
are joined together to create a string:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
name      = space nameStart:x nameChar*:xs -> join([x ~xs])
nameStart = 'a'-'z' | 'A'-'Z'
nameChar  = 'a'-'z' | 'A'-'Z' | '0'-'9'
```

#### []{#d729731e037a4a5aafa6da77d2b8bdb1}Space

A space is any number of space characters or newlines:

```
1.  rlmeta
2.  parser.rlmeta
3.  [rules]{.cp}
```

```
space = (' ' | '\n')*
```

### []{#c3f29fa523f341a487ebc07209c471f9}Code generator

This section defines the code generator and the support library:

```
1.  rlmeta
2.  codegenerator.rlmeta
```

```
CodeGenerator {
  <<rules>>
}
```

```
1.  rlmeta
2.  support.py
```

```
<<classes>>
```

#### []{#b225bd1638614a808495e33a63797beb}Note on target language

The choice of Python as the target language for code generation is an
implementation detail. A different target language could easily be used.
Say for example that you would like to use RLMeta in a web browser. In
that case JavaScript must be used as the target language. RLMeta could
be ported to JavaScript by modifying the code generator and the support
library.

#### []{#69841ad3d1044db18591fe09cdf1caee}Parsing algorithm

The parsing algorithm that RLMeta implements is based on [parsing
expression
grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar)
(PEG), but is extended to match arbitrary objects, not just characters.
Another way to describe the parsing algorithm is that it is a [recursive
descent parser](https://en.wikipedia.org/wiki/Recursive_descent_parser)
with backtracking an
[memoization](https://en.wikipedia.org/wiki/Memoization). Details of the
algorithm is shown in the remainder of this section.

#### []{#0d8a472a5c844a729b866f2c2c64794b}Structure of code generator

The code generator has two main rules: `ast` and `astFnBody`:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
```

```
ast =
  <<ast>>
  | astFnBody:x -> { "(lambda:\n" > x < "\n)" }
astFnBody =
  <<astFnBody>>
```

Sometimes generated code for an AST node should be wrapped in a lambda.
Those AST nodes are added to the `astFnBody` rule. The `astFnBody` rule
is not strictly needed, but without it, many rules would have to wrap
its output in a lambda.

The greater than and less than characters in the string output
expression cause an indent and a dedent like this:

```text
(lambda:
    x
)
```

#### []{#ed6931f4eeca4d43aaee1e9f5485a295}Grammar

When a `Grammar` AST node is matched, a Python class inheriting
`_Grammar` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["Grammar" .:x ast*:ys] -> { "class " x "(_Grammar):\n" > ys < }
```

The name of the class is the same as the name of the grammar and the
child AST nodes are assumed to generate methods in the class.

The `_Grammar` class is defined in the support library:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _Grammar(object):

    <<_Grammar>>
```

Names of support classes start with underscore to not collide with
generated grammar names (which can not contain underscores).

#### []{#3c542ba5b0104273805b227aeab84c04}Rule

When a `Rule` AST node is matched, a Python method with a name prefixed
with `_rule_` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["Rule" .:x ast:y] -> { "\ndef _rule_" x "(self):\n" > "return " y "()\n" < }
```

The method name ends with the same name as the rule. The child AST node
is assumed to generate a matcher. A matcher is a function that tries to
match objects from the input stream and return a semantic action if it
succeeds or raise an exception if it fails. That function is called from
the generated method and the semantic action is returned.

#### []{#696631bc029c47949b2ad97ce78ea32c}Or

When an `Or` AST node is matched, a matcher that calls the built-in
`_or` method is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["Or" astItems:x] -> { "self._or([" x "])" }
```

Rules to generate a list of items:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
```

```
astItems = astItem*:xs -> { "\n" > xs < }
astItem  = ast:x       -> { x ",\n"     }
```

The generated string is wrapped in a lambda because the code is added to
the `astFnBody` rule and will thus look like this:

```text
(lambda:
    self._or([
        matcher1,
        matcher2,
        ...
    ])
)
```

The `_or` method expects a list of matchers which are tried in sequence:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _or(self, matchers):
    original_stream = self._stream
    for matcher in matchers:
        try:
            return matcher()
        except _MatchError:
            self._stream = original_stream
    original_stream.fail("no choice matched")
```

The result of the first succeeding matcher is returned. The input stream
is stored in `_stream`. Streams are immutable, so resetting the stream
upon failure is just a matter of saving and restoring `_stream`.
`_MatchError` is the name of the exception that is raised when a match
fails. Streams have a `fail` method that generates that exception and
adds context to it that is useful for error reporting.

#### []{#86438bda56d342bc9d1f2c62e632a72d}Scope

When a `Scope` AST node is matched, a matcher that creates a new scope
is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["Scope" ast:x] -> { "(lambda _vars:\n" > x < "()\n)(_Vars())" }
```

A scope is a set of variables that do not interfere with variables in
other scopes. The name `_vars` is used to refer to variables in the
current scope. The child AST node is assumed to generate a matcher which
is called to return a semantic action. The generated string will thus
look like this:

```text
(lambda:
    (lambda _vars:
        matcher()
    )(_Vars())
)
```

The `_Vars` class is a subclass of a Python dictionary:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _Vars(dict):

    <<_Vars>>
```

#### []{#487e675a013c45e3aee8b6c068e226df}And

When an `And` AST node is matched, a matcher that calls the built-in
`_and` method is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["And" astItems:x] -> { "self._and([" x "])" }
```

The `_and` method expects a list of matchers which are called in
sequence:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _and(self, matchers):
    result = None
    for matcher in matchers:
        result = matcher()
    return result
```

The result of the last matcher is returned.

#### []{#4309ca572367401bb6c4561f273b8c85}Bind

When a `Bind` AST node is matched, a matcher that binds the name to a
value in the current scope is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["Bind" .:x ast:y] -> { "_vars.bind(" repr(x) ", " y "())" }
```

The child AST node is assumed to generate a matcher which is called to
make the value a semantic action.

The `bind` method stores and returns a value with the given name:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Vars]{.cp}
```

```
def bind(self, name, value):
    self[name] = value
    return value
```

#### []{#244f0754be514ae88e0f581f3ab58c59}Star

When a `Star` AST node is matched, a matcher that calls the built-in
`_star` method is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["Star" ast:x] -> { "self._star(" x ")" }
```

The `_star` method expects a matcher and calls it for as long as it
succeeds:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _star(self, matcher):
    result = []
    while True:
        original_stream = self._stream
        try:
            result.append(matcher())
        except _MatchError:
            self._stream = original_stream
            return _SemanticAction(lambda: [x.eval() for x in result])
```

The return value is a semantic action. When evaluated, it returns a list
of all match results evaluated.

A semantic action is a wrapper for a function:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _SemanticAction(object):

    def __init__(self, fn):
        self.fn = fn

    def eval(self):
        return self.fn()
```

Its `eval` method calls the function. The reason for wrapping semantic
actions in functions is to prevent them from being evaluated before a
parse is complete. If a parse fails, no semantic actions are evaluated.

#### []{#44d3b166c62d43e38d2a781fb9f06b6d}Not

When a `Not` AST node is matched, a matcher that calls the built-in
`_not` method is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["Not" ast:x] -> { "self._not(" x ")" }
```

The `_not` method expects a matcher and succeeds if that matcher fails:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _not(self, matcher):
    original_stream = self._stream
    try:
        matcher()
    except _MatchError:
        return _SemanticAction(lambda: None)
    else:
        original_stream.fail("match found")
    finally:
        self._stream = original_stream
```

It never consumes any input. The original stream is always reset.

#### []{#07530ffd21784561bd8594c98fcd050a}Semantic action

When a `SemanticAction` AST node is matched, a matcher that creates a
`_SemanticAction` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["SemanticAction" ast:x] -> { "_SemanticAction(lambda: " x ")" }
```

The child AST node is assumed to generate a Python expression that will
be returned when the semantic action is evaluated.

#### []{#c10aabed8057404e97ef8cd6ac1b113d}Match rule

When a `MatchRule` AST node is matched, a matcher that calls the
built-in `_match_rule` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["MatchRule" .:x] -> { "self._match_rule(" repr(x) ")"}
```

The `_match_rule` method expects the name of the rule to call:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _match_rule(self, rule_name):
    key = (rule_name, self._stream.position())
    if key in self._memo:
        result, _, self._stream = self._memo[key]
    else:
        start = self._stream
        result = getattr(self, "_rule_{}".format(rule_name))()
        end = self._stream
        self._memo[key] = (result, start, end)
    return result
```

If the given rule has been matched at the current position before, the
memoized result is returned and the input stream is changed to where the
previous match ended. If there has been no previous match, the rule is
matched by calling the method. The result of the match is stored in the
memoization table for later retrieval.

#### []{#3ba5893bb7094ece96e853f869df9456}Match range

When a `MatchRange` AST node is matched, a matcher that calls the
built-in `_match_range` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["MatchRange" .:x .:y] -> { "self._match_range(" repr(x) ", " repr(y) ")" }
```

The `_match_range` method expects two objects defining a range to match:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _match_range(self, start, end):
    original_stream = self._stream
    next_objext, self._stream = self._stream.next()
    if next_objext >= start and next_objext <= end:
        return _SemanticAction(lambda: next_objext)
    else:
        original_stream.fail(
            "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)
        )
```

If the next object from the input stream is in that range, it succeeds,
otherwise it fails.

#### []{#82022cc15d944579825b52559b1ee469}Match string

When a `MatchString` AST node is matched, a matcher that calls the
built-in `_match_string` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["MatchString" .:x] -> { "self._match_string(" repr(x) ")" }
```

The `_match_string` method expects the string to match:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _match_string(self, string):
    original_stream = self._stream
    next_object, self._stream = self._stream.next()
    if next_object == string:
        return _SemanticAction(lambda: string)
    else:
        original_stream.fail(
            "expected {!r} but found {!r}".format(string, next_object)
        )
```

If the next object from the input stream is that string, it succeeds,
otherwise it fails.

#### []{#3eff70b8911e4b4e9bd520a7d25624e7}Match character sequence

When a `MatchCharseq` AST node is matched, a matcher that calls the
built-in `_match_charseq` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["MatchCharseq" .:x] -> { "self._match_charseq(" repr(x) ")" }
```

The `_match_charseq` method expects a string with characters to match:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _match_charseq(self, charseq):
    for char in charseq:
        original_stream = self._stream
        next_object, self._stream = self._stream.next()
        if next_object != char:
            original_stream.fail(
                "expected {!r} but found {!r}".format(char, next_object)
            )
    return _SemanticAction(lambda: charseq)
```

If the next objects from the input stream are those characters, it
succeeds, otherwise it fails.

#### []{#5edd25f746a94b8392834343dce57370}Match any

When a `MatchAny` AST node is matched, the built-in `_match_any` is
generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["MatchAny"] -> { "self._match_any" }
```

The `_match_any` method expects no arguments and always matches the next
object from the input stream:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _match_any(self):
    next_object, self._stream = self._stream.next()
    return _SemanticAction(lambda: next_object)
```

It only fails if there are no more objects.

#### []{#c3b58d2d7b024fa4a0e5ff0bcf06d154}Match list

When a `MatchList` AST node is matched, a matcher that calls the
built-in `_match_list` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [astFnBody]{.cp}
```

```
| ["MatchList" ast:x] -> { "self._match_list(" x ")" }
```

The `_match_list` method expects a matcher that should match the
contents of the list:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def _match_list(self, matcher):
    original_stream = self._stream
    next_object, next_stream = self._stream.next()
    if isinstance(next_object, list):
        self._stream = self._stream.nested(next_object)
        matcher()
        if self._stream.is_at_end():
            self._stream = next_stream
            return _SemanticAction(lambda: next_object)
    original_stream.fail("list match failed")
```

If the next object is a list, a new stream is created with the `nested`
call that contains all child objects. It is set to be the input stream,
and the matcher is then called. The matcher must match all child
objects, or the match fails.

#### []{#74804e4e0ee643ac95210d7aa17ae7c6}String

When a `String` AST node is matched, a Python string is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["String" .:x] -> { repr(x) }
```

#### []{#03d7a70d44fe4470967acd15d51ad56a}List

When a `List` AST node is matched, a Python list is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["List" astList:x] -> { x }
```

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
```

```
astList = astListItem*:xs    -> { "(" xs "[])" }
astListItem =
  | ["ListItemSplice" ast:x] -> {     x  "+"   }
  | ast:x                    -> { "[" x "]+"   }
```

The Python list is generated by concatenating sub-lists. If an item
should be spliced, it is assumed to be a list already and is not wrapped
in brackets.

#### []{#166dffacb2ca4911908584d77f30b21f}Builder

When a `Builder` AST node is matched, a `_Builder` is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["Builder" astItems:x] -> { "_Builder.create([" x "])" }
```

The child AST nodes are assumed to generate Python expressions.

When an `IndentBuilder` AST node is matched, an `_IndentBuilder` is
generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["IndentBuilder"] -> { "_IndentBuilder()" }
```

When a `DedentBuilder` AST node is matched, a `_DedentBuilder` is
generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["DedentBuilder"] -> { "_DedentBuilder()" }
```

All builders inherit from `_Builder`:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _Builder(object):

    <<_Builder>>
```

A builder can build a string with the `build_string` method:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Builder]{.cp}
```

```
def build_string(self):
    output = _Output()
    self.write(output)
    return output.value
```

All builders must implement the `write` method that is passed an
instance of `_Output`. The `_Output` class has functionality to build a
string with appropriate indentation:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _Output(object):

    def __init__(self):
        self.value = ""
        self.indentation = 0

    def write(self, value):
        for ch in value:
            if self.value and ch != "\n" and self.value[-1] == "\n":
                self.value += "    "*self.indentation
            self.value += ch
```

The `create` method of a builder creates an instance of a builder
depending on the type of object passed in:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Builder]{.cp}
```

```
@classmethod
def create(self, item):
    if isinstance(item, _Builder):
        return item
    elif isinstance(item, list):
        return _ListBuilder([_Builder.create(x) for x in item])
    else:
        return _AtomBuilder(item)
```

A `_ListBuilder` calls the `write` method on all child builders:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _ListBuilder(_Builder):

    def __init__(self, builders):
        self.builders = builders

    def write(self, output):
        for builder in self.builders:
            builder.write(output)
```

An `_AtomBuilder` converts its object to a string and writes it to the
output:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _AtomBuilder(_Builder):

    def __init__(self, atom):
        self.atom = atom

    def write(self, output):
        output.write(str(self.atom))
```

An `_IndentBuilder` changes the indentation of the output:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _IndentBuilder(_Builder):

    def write(self, output):
        output.indentation += 1
```

A `_DedentBuilder` changes the indentation of the output:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _DedentBuilder(_Builder):

    def write(self, output):
        output.indentation -= 1
```

#### []{#f5372dab8e084ca386297eb9575052e8}Function call

When a `FnCall` AST node is matched, a Python function call is
generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["FnCall" .:x astItems:y] -> { x "(" y ")" }
```

The child AST nodes are assumed to generate Python expressions.

#### []{#d9f73bdeb5c447dbaf5b8fe0bf3b67ba}Variable lookup

When a `VarLookup` AST node is matched, a Python expression that looks
up the variable and evaluates it is generated:

```
1.  rlmeta
2.  codegenerator.rlmeta
3.  [rules]{.cp}
4.  [ast]{.cp}
```

```
| ["VarLookup" .:x] -> { "_vars.lookup(" repr(x) ").eval()" }
```

The `lookup` method returns the stored value:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Vars]{.cp}
```

```
def lookup(self, name):
    return self[name]
```

Every time a variable is referenced, the `eval` method is called.
Consider this grammar:

```text
AGrammar {
  foo = bar:x -> { x x }
  bar = .     -> a_side_effect()
}
```

If evaluating `x` has a side effect, it will be executed twice. The
`eval` method of `_SemanticAction` could be modified so that the
function is only called once if needed.

#### []{#4397d20868fa4c08ac289e463725f18e}Final support methods

Grammars in Python have a single entry point, `run`, which expects the
name of the rule to match and the input object:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
4.  [\_Grammar]{.cp}
```

```
def run(self, rule_name, input_object):
    self._memo = _Memo()
    self._stream = _Stream.from_object(self._memo, input_object)
    result = self._match_rule(rule_name).eval()
    if isinstance(result, _Builder):
        return result.build_string()
    else:
        return result
```

It initializes the memoization table and the input stream, matches the
rule, and returns the evaluated result. If the result is a builder, the
string that the builder builds is returned.

The memoization table is a subclass of a Python dictionary:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _Memo(dict):

    def __init__(self):
        dict.__init__(self)
        self._latest_stream = _ObjectStream(self, [], position=-1)
        self._latest_message = ""

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
            self._latest_message
        ))
        return "".join(message)

    def fail(self, stream, message):
        if stream.position() >= self._latest_stream.position():
            self._latest_stream = stream
            self._latest_message = message
        raise _MatchError(self)
```

Its `describe` method returns a string that describes what has been
matched so far and what the latest error message was. It is used for
error reporting. It keeps track of match failures via the `fail` method
which is always called to raise a `_MatchError`.

A `_MatchError` is a Python exception that also has a reference to a
memoization table so that the `describe` method can be exposed:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _MatchError(Exception):

    def __init__(self, memo):
        Exception.__init__(self)
        self._memo = memo

    def describe(self):
        return self._memo.describe()
```

A `_Stream` is an immutable object that has a list of objects:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _Stream(object):

    @classmethod
    def from_object(cls, memo, input_object):
        if isinstance(input_object, basestring):
            return _CharStream(memo, list(input_object))
        else:
            return _ObjectStream(memo, [input_object])

    def __init__(self, memo, objects):
        self._memo = memo
        self._objects = objects

    def fail(self, message):
        self._memo.fail(self, message)

    def next(self):
        if self.is_at_end():
            self.fail("not eof")
        next_object = self._objects[0]
        return (
            next_object,
            self._advance(next_object, self._objects[1:]),
        )

    def is_at_end(self):
        return len(self._objects) == 0
```

Its `next` method returns a tuple with the next object and the next
stream. There are two kinds of streams: character streams and object
streams. They are subclasses of `_Stream` and implement the `position`
and `_advance` methods. The appropriate stream is chosen in the
`from_object` method.

A character steam stores the position as a line + column:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _CharStream(_Stream):

    def __init__(self, memo, objects, line=1, column=1):
        _Stream.__init__(self, memo, objects)
        self._line = line
        self._column = column

    def position(self):
        return (self._line, self._column)

    def _advance(self, next_object, objects):
        if next_object == "\n":
            return _CharStream(self._memo, objects, self._line+1, 1)
        else:
            return _CharStream(self._memo, objects, self._line, self._column+1)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)
```

An object stream stores the position as a tuple of indices:

```
1.  rlmeta
2.  support.py
3.  [classes]{.cp}
```

```
class _ObjectStream(_Stream):

    def __init__(self, memo, objects, parent=(), position=0):
        _Stream.__init__(self, memo, objects)
        self._parent = parent
        self._position = position

    def position(self):
        return self._parent + (self._position,)

    def nested(self, input_object):
        return _ObjectStream(self._memo, input_object, self._parent+(self._position,))

    def _advance(self, next_object, objects):
        return _ObjectStream(self._memo, objects, self._parent, self._position+1)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.position()))
```

The `nested` method will only be called on object streams since
character streams do not nest.

### []{#2c78e9d9104c4bddbdd1dfe6314506c9}Putting it together

Here is a template for the final Python file that implements the RLMeta
compiler:

```
1.  rlmeta
2.  compile.sh
3.  [python file template]{.cp}
```

```
import sys

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(compile_grammar(sys.stdin.read()))
        except _MatchError as e:
            sys.stderr.write(e.describe())
            sys.exit(1)
```

First the `sys` module is imported because the main method needs it.
Then the support library snippet is stored in a variable so that it can
be output when the `--support` flag is given. Then the support library,
compiled parser grammar, and compiled code generator grammar snippets
are inserted. Then the host language function `join` is defined. Then a
function to compile a grammar is defined. Finally the main method that
reads a grammar from stdin and writes a Python class to stdout is
implemented. If an error occurs, it is written to stderr using the
exception\'s `describe` method. This template is rendered with a Bash
script:

```
1.  rlmeta
2.  compile.sh
```

```
#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py=$(cat support.py)
support_py_string=$(to_python_string < support.py)
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)

cat <<EOF
<<python file template>>
EOF
```

First the `set -e` directive ensures that the script exits as soon as
there is an error. Then a variable containing the path to the RLMeta
compiler is defined. The path must be given to the compile script as the
first argument. Then the directory is changed to the one where the
compile script lives. Then a function is defined that turns its stdin
into a Python string with correct quoting. Then the support library is
captured in a variable. Then the output of passing the support library
to `to_python_string` is captured in a variable. Then the output of two
calls to the RLMeta compiler are captured in two variables. Finally the
Python file template is rendered and written to stdout.

If the compile script is run with the current RLMeta compiler
(`rlmeta/rlmeta.py`), a new Python file is output that is exactly the
same as the `rlmeta/rlmeta.py` file. It can be seen by diffing the two
files:

```text
$ diff <(./rlmeta/compile.sh rlmeta/rlmeta.py) rlmeta/rlmeta.py && echo EQUAL
EQUAL
```

The `rlmeta.py` file never needs to be changed manually. The next
version can always be produced using the previous version. Sometimes the
next version must be produced in steps and intermediate compilers must
be created. That is why the path to the compiler must be given to the
compile script. (More on modifying RLMeta in the [next
article](/writing/modifying-rlmeta/index.html).) If you want to see what
the `rlmeta.py` file looks like, it is
[here](https://github.com/rickardlindberg/rickardlindberg.me/blob/master/writing/rlmeta/rlmeta/rlmeta.py).

### []{#313b147a2f574ea09d76d9c7371bdf18}Bootstrapping

In the previous section, a version of the RLMeta compiler was needed to
compile the RLMeta compiler:

```text
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)
```

But how can the RLMeta compiler be run before it exists? How was the
first version of `rlmeta.py` created? This is a bootstrapping problem.
In this case I solved it by translating the parser and the code
generator to Python code manually according the rules specified in the
grammars. I did manually what the compiler would do.

I started translating the parser that looks like this:

```text
Parser {
  ...
}
```

This is matched by the `grammar` rule in the parser and turned into a
`Grammar` AST node:

```text
grammar =
  | name:x space '{' rule*:ys space '}' -> ["Grammar" x ~ys]
```

The `Grammar` AST node is then turned into a Python class definition by
the code generator:

```text
| ["Grammar" .:x ast*:ys] -> { "class " x "(_Grammar):\n" > ys < }
```

The parser is thus turned into the following Python code:

```text
class Parser(_Grammar):
    ...
```

I then went on to translate the rules in the parser. The first rule is
`grammar`:

```text
grammar =
  | name:x space '{' rule*:ys space '}' -> ["Grammar" x ~ys]
```

It is matched by the `rule` rule in the parser and turned into a `Rule`
AST node:

```text
rule =
  | name:x space '=' choice:y -> ["Rule" x y]
```

The `Rule` AST node is then turned into a Python method definition by
the code generator:

```text
| ["Rule" .:x ast:y] -> { "\ndef _rule_" x "(self):\n" > "return " y "()\n" < }
```

The `grammar` rule is thus turned into the following Python code:

```text
def _rule_grammar(self):
    return ...()
```

The body of the `grammar` rule is matched by the `choice` rule in the
parser and turned into an `Or` AST node:

```text
choice =
  | (space '|')?
    sequence:x (space '|' sequence)*:xs -> ["Or" x ~xs]
```

The `Or` AST node is then turned into a Python lambda expression by the
code generator:

```text
| ["Or" astItems:x] -> { "self._or([" x "])" }
```

The body of the `grammar` rule is thus turned into the following Python
code:

```text
(lambda:
    self._or([...])
)
```

I continued this process until all dots had been expanded. Then I did
the same for all remaining rules. Finally I repeated the process for the
code generator. Once I had the manually translated versions of
`parser.py` and `codegenerator.py`, I could temporarily replace

```text
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)
```

with

```text
parser_py=$(cat parser.py)
codegenerator_py=$(cat codegenerator.py)
```

to create the initial version of `rlmeta.py`.

With the initial version of `rlmeta.py` I could run the compile script
to generate the second version of `rlmeta.py`. The second version did
not match the first version exactly. The complete diff can be seen in
the commit [Get rid of bootstrapped
compiler](https://github.com/rickardlindberg/rickardlindberg.me/commit/134c3a360160a2b978cd742a935df1c3a85de546#diff-7687f8856e0607ca8e0247b6cd77cf7b).
Mostly I had used a different character for strings and forgotten some
commas that were not strictly necessary. Once the second version
replaced the first, the compile script reproduced `rlmeta.py` exactly.
At this point the manually translated versions could be discarded. The
RLMeta compiler was bootstrapped. This was a tremendously rewarding
experience.

Did the first version work on the first run? No. In the translation
process I noticed incorrect behavior in the grammars that I had to fix.
And some manual translations were done incorrectly. But all fixes were
relatively minor. Before the translation I had also carefully debugged
the grammars in my head to decrease the risk of them having bugs.

To avoid making mistakes in the manual translation process, I created
snippets for the Vim text editor for each AST node. So when I
encountered a `Rule` AST node, I could type \"rule\", hit tab, and the
following snippet would be inserted placing the cursor where the rule
name should be inserted:

```text
def _rule_${1:name}(self):
    return ${2:matcher}()
```

I could then type the name (\"grammar\" for example), hit tab, write the
name of the next AST node, and hit tab. If the next AST node was `Or`
for example, the following snipped would be inserted, placing the cursor
inside the list:

```text
(lambda:
    self._or([
        ${1:matchers}
    ])
)
```

The Vim snippets saved me a lot of typing and made manual translation
feasible. Getting all commas and parenthesis right would have been
difficult otherwise.

[]{#5f558623cfce441fb3e61033299d1419}Implementing programming languages
-----------------------------------------------------------------------

When I read [Structure and Interpretation of Computer
Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)
I remember thinking that it described what programming was all about.
The following quote from [chapter
4](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-25.html#%_chap_4)
I think summarizes it:

> However, as we confront increasingly complex problems, we will find
> that Lisp, or indeed any fixed programming language, is not sufficient
> for our needs. We must constantly turn to new languages in order to
> express our ideas more effectively. Establishing new languages is a
> powerful strategy for controlling complexity in engineering design; we
> can often enhance our ability to deal with a complex problem by
> adopting a new language that enables us to describe (and hence to
> think about) the problem in a different way, using primitives, means
> of combination, and means of abstraction that are particularly well
> suited to the problem at hand.

In this article I set out to explain how a metalanguage could be used to
give meaning to arithmetic expressions. But RLMeta is more powerful than
that. It can be used to implement itself and also many other programming
languages because matching and transforming is at the heart of
programming language implementation. Its small implementation, just over
400 lines of code, also makes it feasible to understand and modify:

```text
 51 parser.rlmeta
 33 codegenerator.rlmeta
278 support.py
 45 compile.sh
407 total
```

I hope this article inspires you to experiment with implementing
programming languages so that you can solve complex problems elegantly.

[]{#4c6af1a11ed440d2bf31a497032c9c0b}Resources
----------------------------------------------

I was helped by the following resources when implementing RLMeta:

-   [META II paper](http://www.hcs64.com/files/pd1-3-schorre.pdf)
-   [OMeta thesis](http://www.vpri.org/pdf/tr2008003_experimenting.pdf)
-   [Tutorial: Metacompilers Part
    1](http://www.bayfronttechnologies.com/mc_tutorial.html)
-   [META II: A Syntax-Oriented Compiler Writing Language - Papers We
    Love Singapore](https://www.youtube.com/watch?v=L1rwVBLHGiU)

[]{#388bb1e8ccbd4d55b89b391c08452c33}Code listings for RLMeta
-------------------------------------------------------------

### []{#a56c54f42c00473091d7c8295ff4e0f1}parser.rlmeta

```text
Parser {
  grammar =
    | name:x space '{' rule*:ys space '}'      -> ["Grammar" x ~ys]
  rule =
    | name:x space '=' choice:y                -> ["Rule" x y]
  choice =
    | (space '|')?
      sequence:x (space '|' sequence)*:xs      -> ["Or" x ~xs]
  sequence =
    | expr:x expr*:xs                          -> ["Scope" ["And" x ~xs]]
  expr =
    | expr1:x space ':' name:y                 -> ["Bind" y x]
    | expr1
  expr1 =
    | expr2:x space '*'                        -> ["Star" x]
    | expr2:x space '?'                        -> ["Or" x ["And"]]
    | space '!' expr2:x                        -> ["Not" x]
    | expr2
  expr2 =
    | space '->' hostExpr:x                    -> ["SemanticAction" x]
    | name:x !(space '=')                      -> ["MatchRule" x]
    | space char:x '-' char:y                  -> ["MatchRange" x y]
    | space string:x                           -> ["MatchString" x]
    | space charseq:x                          -> ["MatchCharseq" x]
    | space '.'                                -> ["MatchAny"]
    | space '(' choice:x space ')'             -> x
    | space '[' expr*:xs space ']'             -> ["MatchList" ["And" ~xs]]
  hostExpr =
    | space string:x                           -> ["String" x]
    | space '[' hostExprListItem*:xs space ']' -> ["List" ~xs]
    | space '{' buildExpr*:xs space '}'        -> ["Builder" ~xs]
    | name:x space '(' hostExpr*:ys space ')'  -> ["FnCall" x ~ys]
    | name:x                                   -> ["VarLookup" x]
  hostExprListItem =
    | space '~' hostExpr:x                     -> ["ListItemSplice" x]
    | hostExpr
  buildExpr =
    | space '>'                                -> ["IndentBuilder"]
    | space '<'                                -> ["DedentBuilder"]
    | hostExpr
  string    = '"'  (!'"'  innerChar)*:xs '"'   -> join(xs)
  charseq   = '\'' (!'\'' innerChar)*:xs '\''  -> join(xs)
  char      = '\''  !'\'' innerChar  :x  '\''  -> x
  innerChar = '\\' escape | .
  escape    = '\\' -> "\\" | '\'' -> "'"
            | '"'  -> "\"" | 'n'  -> "\n"
  name      = space nameStart:x nameChar*:xs   -> join([x ~xs])
  nameStart = 'a'-'z' | 'A'-'Z'
  nameChar  = 'a'-'z' | 'A'-'Z' | '0'-'9'
  space     = (' ' | '\n')*
}
```

### []{#d9ea64bbdad3465897667ebec9d5ace1}codegenerator.rlmeta

```text
CodeGenerator {
  ast =
    | ["Grammar" .:x ast*:ys]   -> { "class " x "(_Grammar):\n" > ys <                   }
    | ["Rule" .:x ast:y]        -> { "\ndef _rule_" x "(self):\n" > "return " y "()\n" < }
    | ["MatchAny"]              -> { "self._match_any"                                   }
    | ["String" .:x]            -> { repr(x)                                             }
    | ["List" astList:x]        -> { x                                                   }
    | ["Builder" astItems:x]    -> { "_Builder.create([" x "])"                          }
    | ["IndentBuilder"]         -> { "_IndentBuilder()"                                  }
    | ["DedentBuilder"]         -> { "_DedentBuilder()"                                  }
    | ["FnCall" .:x astItems:y] -> { x "(" y ")"                                         }
    | ["VarLookup" .:x]         -> { "_vars.lookup(" repr(x) ").eval()"                  }
    | astFnBody:x               -> { "(lambda:\n" > x < "\n)" }
  astFnBody =
    | ["Or" astItems:x]         -> { "self._or([" x "])"                                 }
    | ["Scope" ast:x]           -> { "(lambda _vars:\n" > x < "()\n)(_Vars())"           }
    | ["And" astItems:x]        -> { "self._and([" x "])"                                }
    | ["Bind" .:x ast:y]        -> { "_vars.bind(" repr(x) ", " y "())"                  }
    | ["Star" ast:x]            -> { "self._star(" x ")"                                 }
    | ["Not" ast:x]             -> { "self._not(" x ")"                                  }
    | ["SemanticAction" ast:x]  -> { "_SemanticAction(lambda: " x ")"                    }
    | ["MatchRule" .:x]         -> { "self._match_rule(" repr(x) ")"                     }
    | ["MatchRange" .:x .:y]    -> { "self._match_range(" repr(x) ", " repr(y) ")"       }
    | ["MatchString" .:x]       -> { "self._match_string(" repr(x) ")"                   }
    | ["MatchCharseq" .:x]      -> { "self._match_charseq(" repr(x) ")"                  }
    | ["MatchList" ast:x]       -> { "self._match_list(" x ")"                           }
  astItems = astItem*:xs        -> { "\n" > xs <                                         }
  astItem  = ast:x              -> { x ",\n"                                             }
  astList  = astListItem*:xs    -> { "(" xs "[])"                                        }
  astListItem =
    | ["ListItemSplice" ast:x]  -> {     x  "+"                                          }
    | ast:x                     -> { "[" x "]+"                                          }
}
```

### []{#983e606587034a0a9c8e8b5c714e7ef8}support.py

```text
class _Grammar(object):

    def _or(self, matchers):
        original_stream = self._stream
        for matcher in matchers:
            try:
                return matcher()
            except _MatchError:
                self._stream = original_stream
        original_stream.fail("no choice matched")

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
            original_stream.fail("match found")
        finally:
            self._stream = original_stream

    def _match_rule(self, rule_name):
        key = (rule_name, self._stream.position())
        if key in self._memo:
            result, _, self._stream = self._memo[key]
        else:
            start = self._stream
            result = getattr(self, "_rule_{}".format(rule_name))()
            end = self._stream
            self._memo[key] = (result, start, end)
        return result

    def _match_range(self, start, end):
        original_stream = self._stream
        next_objext, self._stream = self._stream.next()
        if next_objext >= start and next_objext <= end:
            return _SemanticAction(lambda: next_objext)
        else:
            original_stream.fail(
                "expected range {!r}-{!r} but found {!r}".format(start, end, next_objext)
            )

    def _match_string(self, string):
        original_stream = self._stream
        next_object, self._stream = self._stream.next()
        if next_object == string:
            return _SemanticAction(lambda: string)
        else:
            original_stream.fail(
                "expected {!r} but found {!r}".format(string, next_object)
            )

    def _match_charseq(self, charseq):
        for char in charseq:
            original_stream = self._stream
            next_object, self._stream = self._stream.next()
            if next_object != char:
                original_stream.fail(
                    "expected {!r} but found {!r}".format(char, next_object)
                )
        return _SemanticAction(lambda: charseq)

    def _match_any(self):
        next_object, self._stream = self._stream.next()
        return _SemanticAction(lambda: next_object)

    def _match_list(self, matcher):
        original_stream = self._stream
        next_object, next_stream = self._stream.next()
        if isinstance(next_object, list):
            self._stream = self._stream.nested(next_object)
            matcher()
            if self._stream.is_at_end():
                self._stream = next_stream
                return _SemanticAction(lambda: next_object)
        original_stream.fail("list match failed")

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
        self.value = ""
        self.indentation = 0

    def write(self, value):
        for ch in value:
            if self.value and ch != "\n" and self.value[-1] == "\n":
                self.value += "    "*self.indentation
            self.value += ch

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
        self._latest_stream = _ObjectStream(self, [], position=-1)
        self._latest_message = ""

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
            self._latest_message
        ))
        return "".join(message)

    def fail(self, stream, message):
        if stream.position() >= self._latest_stream.position():
            self._latest_stream = stream
            self._latest_message = message
        raise _MatchError(self)

class _MatchError(Exception):

    def __init__(self, memo):
        Exception.__init__(self)
        self._memo = memo

    def describe(self):
        return self._memo.describe()

class _Stream(object):

    @classmethod
    def from_object(cls, memo, input_object):
        if isinstance(input_object, basestring):
            return _CharStream(memo, list(input_object))
        else:
            return _ObjectStream(memo, [input_object])

    def __init__(self, memo, objects):
        self._memo = memo
        self._objects = objects

    def fail(self, message):
        self._memo.fail(self, message)

    def next(self):
        if self.is_at_end():
            self.fail("not eof")
        next_object = self._objects[0]
        return (
            next_object,
            self._advance(next_object, self._objects[1:]),
        )

    def is_at_end(self):
        return len(self._objects) == 0

class _CharStream(_Stream):

    def __init__(self, memo, objects, line=1, column=1):
        _Stream.__init__(self, memo, objects)
        self._line = line
        self._column = column

    def position(self):
        return (self._line, self._column)

    def _advance(self, next_object, objects):
        if next_object == "\n":
            return _CharStream(self._memo, objects, self._line+1, 1)
        else:
            return _CharStream(self._memo, objects, self._line, self._column+1)

    def __str__(self):
        return "L{:03d}:C{:03d}".format(self._line, self._column)

class _ObjectStream(_Stream):

    def __init__(self, memo, objects, parent=(), position=0):
        _Stream.__init__(self, memo, objects)
        self._parent = parent
        self._position = position

    def position(self):
        return self._parent + (self._position,)

    def nested(self, input_object):
        return _ObjectStream(self._memo, input_object, self._parent+(self._position,))

    def _advance(self, next_object, objects):
        return _ObjectStream(self._memo, objects, self._parent, self._position+1)

    def __str__(self):
        return "[{}]".format(", ".join(str(x) for x in self.position()))
```

### []{#193d8f0ff47f4edcb201139df8cd9520}compile.sh

```text
#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py=$(cat support.py)
support_py_string=$(to_python_string < support.py)
parser_py=$(python "$rlmeta_compiler" < parser.rlmeta)
codegenerator_py=$(python "$rlmeta_compiler" < codegenerator.rlmeta)

cat <<EOF
import sys

SUPPORT = $support_py_string

$support_py

$parser_py

$codegenerator_py

join = "".join

def compile_grammar(grammar):
    parser = Parser()
    code_generator = CodeGenerator()
    return code_generator.run("ast", parser.run("grammar", grammar))

if __name__ == "__main__":
    if "--support" in sys.argv:
        sys.stdout.write(SUPPORT)
    else:
        try:
            sys.stdout.write(compile_grammar(sys.stdin.read()))
        except _MatchError as e:
            sys.stderr.write(e.describe())
            sys.exit(1)
EOF
```
