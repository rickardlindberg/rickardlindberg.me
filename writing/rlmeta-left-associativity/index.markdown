---
title: 'Parsing left associative operators using RLMeta'
date: 2019-09-07
tags: rlmeta
---
The [first article](/writing/rlmeta/index.html) about RLMeta has this
example grammar implementing a simple calculator:

```text
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

It works, but if we were to add a second case to support subtraction, it
would break because subtraction is a left associate operator but the
calculator creates a right associative parse. In this article I show how
it would brake and how it can be solved.

-   [What is operator associativity?](#37144d09790c48a3bbdc3485473b5416)
-   [Right associative calculator](#cb24f5d3a7df423e9081889d4a2d3785)
-   [Left associative calculator](#8366ac579e1e4be9930317242abd5dd6)
-   [Correct left associative
    calculator](#855b50c552e54c0895128e7e86da55e9)
-   [Right associative calculator without
    recursion](#736626ed0c3c4fea922cc4f14c5cda38)
-   [Calculator combining operators](#ae39c406658e4cd287411ab2c76710f8)
-   [Cleaner handling of precedence](#063f23a343574259bf31debb9b8425bf)
-   [Comparison of operator parsers](#24d9bf94e3b64dfba2841df8b3cf00b1)
-   [Resources](#5cca99df2e2c4c78ab4bfe4cb946d89a)
-   [Appendix: Test script](#c1716e6e63034983a86a1c9f621b2a52)

[]{#37144d09790c48a3bbdc3485473b5416}What is operator associativity?
--------------------------------------------------------------------

Operator associativity has to do with the order that operators are
evaluated. The following expression can be evaluated in two ways:

```text
1-2-3
```

Either as this (if the operator is left associative):

```text
(1-2)-3
```

Or as this (if the operator is right associative):

```text
1-(2-3)
```

Let\'s see how the calculator evaluates expressions.

[]{#cb24f5d3a7df423e9081889d4a2d3785}Right associative calculator
-----------------------------------------------------------------

The following grammar works like the calculator but only supports
subtraction:

```
1.  right.rlmeta
```

```rlmeta
Calculator {
  expr  = digit:x '-' expr:y -> sub(x y)
        | digit
  digit = '0'-'9':x          -> int(x)
}
```

The `sub` function is implemented in two ways. The first creates an AST
node representing a subtraction:

```
1.  ast.py
```

```py
def sub(left, right):
    return ["-", left, right]
```

The second evaluates the subtraction expression:

```
1.  eval.py
```

```py
def sub(left, right):
    return left - right
```

Let\'s see how the calculator handles the following two expressions:

```text
1-2
1-2-3
```

Below are the ASTs and the evaluated results:

```text
['-', 1, 2]
=>
-1

['-',
 1,
 ['-', 2, 3]]
=>
2
```

For an expression with only two numbers, the calculator works as
expected. But with more numbers, the parse is incorrect. The calculator
parses `1-2-3` as `1-(2-3)` which is incorrect because subtraction is
left associative and should thus be parsed as `(1-2)-3`.

The original calculator does not have this problem because it only
supports operators that evaluate the same no matter if parsed as left
associative or right associative.

If we strip the semantic actions from the `expr` rule it is easier to
see what creates this right associative parse:

```text
expr = digit '-' expr | digit
```

The topmost expression will be parsed as a digit followed by an
arbitrary complex expression: `(digit - (...))`.

Let\'s see if we can get a left associative parse instead.

[]{#8366ac579e1e4be9930317242abd5dd6}Left associative calculator
----------------------------------------------------------------

In order to get a left associate parse (that we need for subtraction) we
would like to write the `expr` rule like this instead:

```text
expr = expr '-' digit | digit
```

The whole grammar would then look like this:

```
1.  left.rlmeta
```

```rlmeta
Calculator {
  expr  = expr:x '-' digit:y -> sub(x y)
        | digit
  digit = '0'-'9':x          -> int(x)
}
```

Unfortunately, this will not work with the parsing algorithm that RLMeta
uses. In order to parse an `expr` it first has to parse an `expr` and so
on, and it will get stuck in an infinite loop. The parsing algorithm is
based on recursive descent parsing and the first choice in the `expr`
rule will thus be translated into something like this:

```text
def expr():
    x = expr()
    atom("-")
    y = digit()
    return sub(x, y)
```

We need to handle left associative operators differently.

[]{#855b50c552e54c0895128e7e86da55e9}Correct left associative calculator
------------------------------------------------------------------------

The right associative parse that we saw earlier can be rewritten using a
repetition:

```text
expr = digit '-' expr | digit
```

It then becomes this:

```text
expr = digit ('-' digit)*
```

It parses the same expressions, but it does not create a right
associative parse. What does it create? Something that consist of a
digit and a list of something. We can turn this into a parse tree using
a function that we call `leftAssoc`. Here is the complete grammar:

```
1.  left\_correct.rlmeta
```

```rlmeta
Calculator {
  expr  = digit:x (op:y digit:z -> [y z])*:xs -> leftAssoc(x xs)
  digit = '0'-'9':x                           -> int(x)
  op    = '-'                                 -> makeSub()
}
```

In the semantic action for `expr`, `x` is bound to a digit, and `xs` is
bound to a list of pairs consisting of an operator and a digit:
`[[op, digit], [op, digit], ...]`. The operator is a function that takes
two arguments: the left hand side and the right hand side. (`makeSub`
returns the `sub` function. It is used because currently global
variables can not be referenced from semantic actions. Only global
functions.) The function `leftAssoc` turns this into a left associative
tree:

```
1.  support.py
```

```py
def leftAssoc(expr, items):
    while items:
        op, rhs = items.pop(0)
        expr = op(expr, rhs)
    return expr
```

Here is how the expression `1-2-3` is parsed:

-   `leftAssoc` is called with `expr=1` and
    `items=[[sub, 2], [sub, 3]]`.
-   Since there are still items, the loop is entered
    -   `op=sub` and `rhs=2`
    -   `expr=sub(1, 2)`
-   Since there are still items, the loop is entered
    -   `op=sub` and `rhs=3`
    -   `expr=sub(sub(1, 2), 3)`
-   Since there are no more items, `sub(sub(1, 2), 3)` is returned

Let\'s verify that the calculator handles the following expressions as
intended:

```text
1-2
1-2-3
```

Below are the ASTs and the evaluated results:

```text
['-', 1, 2]
=>
-1

['-',
 ['-', 1, 2],
 3]
=>
-4
```

And indeed it does. We have now successfully parsed a left associate
operator using RLMeta.

[]{#736626ed0c3c4fea922cc4f14c5cda38}Right associative calculator without recursion
-----------------------------------------------------------------------------------

We can handle a right associative parse in RLMeta with a recursive rule
as we have seen before:

```text
expr = digit '-' expr | digit
```

However, we can also obtain a right associative parse by parsing
operators as a list and then converting them into an AST with a
`rightAssoc` function. Here is a grammar that supports only
exponentiation which is right associative:

```
1.  right\_no\_recursion.rlmeta
```

```rlmeta
Calculator {
  expr  = digit:x (op:y digit:z -> [y z])*:xs -> rightAssoc(x xs)
  digit = '0'-'9':x                           -> int(x)
  op    = '^'                                 -> makePow()
}
```

It has the exact same structure as the previous calculator, only now a
different function is called to create the parse tree. The `rightAssoc`
function is also similar in structure to the `leftAssoc` function, but
the loop is replaced with an if statement followed by a recursive call:

```
1.  support.py
```

```py
def rightAssoc(expr, items):
    if items:
        op, rhs = items.pop(0)
        expr = op(expr, rightAssoc(rhs, items))
    return expr
```

Here is how the expression `3^2^2` is parsed:

-   `rightAssoc` is called with `expr=3` and
    `items=[[pow, 2], [pow, 2]]`
-   Since there are still items, the if statement is entered
    -   `op=pow` and `rhs=2`
    -   `expr=pow(3, rightAssoc(2, [[pow, 2]])`
-   `rightAssoc` is called with `expr=2` and `items=[[pow, 2]]`
-   Since there are still items, the if statement is entered
    -   `op=pow` and `rhs=2`
    -   `expr=pow(2, rightAssoc(2, [])`
-   `rightAssoc` is called with `expr=2` and `items=[]`
-   Since there are no more items, `2` is returned
-   The parent call returns `pow(2, 2)`
-   The parent call returns `pow(3, pow(2, 2))`

The two `pow` functions are defined like this:

```
1.  ast.py
```

```py
def pow(left, right):
    return ["^", left, right]
```

```
1.  eval.py
```

```py
def pow(left, right):
    return left ** right
```

Let\'s verify that the calculator handles the following expressions as
intended:

```text
3^2
3^2^2
```

Below are the ASTs and the evaluated results:

```text
['^', 3, 2]
=>
9

['^',
 3,
 ['^', 2, 2]]
=>
81
```

And indeed it does.

[]{#ae39c406658e4cd287411ab2c76710f8}Calculator combining operators
-------------------------------------------------------------------

Now that we know how to handle both left and right associative
operators, we can extend the calculator to handle more operators:

```
1.  calculator.rlmeta
```

```rlmeta
Calculator {
  expr  = expr1
  expr1 = expr2:x (op1:y expr2:z -> [y z])*:xs -> leftAssoc(x xs)
  expr2 = expr3:x (op2:y expr3:z -> [y z])*:xs -> leftAssoc(x xs)
  expr3 = digit:x (op3:y digit:z -> [y z])*:xs -> rightAssoc(x xs)
  digit = '0'-'9':x                            -> int(x)
  op1 =
    | '+' -> makeAdd()
    | '-' -> makeSub()
  op2 =
    | '*' -> makeMul()
    | '/' -> makeDiv()
  op3 =
    | '^' -> makePow()
}
```

Different levels of expressions are used to handle operators having
different precedence (multiplication is done before subtraction for
example).

The additional arithmetic functions are defined like this:

```
1.  ast.py
```

```py
def add(left, right):
    return ["+", left, right]

def mul(left, right):
    return ["*", left, right]

def div(left, right):
    return ["/", left, right]
```

```
1.  eval.py
```

```py
def add(left, right):
    return left + right

def mul(left, right):
    return left * right

def div(left, right):
    return left / right
```

Let\'s see how the calculator handles the following expression:

```text
1+2-3^2^2-5
```

Below is the AST and the evaluated result:

```text
['-',
 ['-',
  ['+', 1, 2],
  ['^',
   3,
   ['^', 2, 2]]],
 5]
=>
-83
```

If we enter the same expression in the Python prompt, but replace `^`
with `**` as Python uses, we get the same result:

```text
>>> 1+2-3**2**2-5
-83
```

We can now parse complicated expressions correctly. However, if there
are many precedence levels in a grammar, it might become difficult to
read. Can we do better?

[]{#063f23a343574259bf31debb9b8425bf}Cleaner handling of precedence
-------------------------------------------------------------------

Here is the calculator grammar from the previous section rewritten in a
cleaner way:

```
1.  calculator\_ops.rlmeta
```

```rlmeta
Calculator {
  expr  = digit:x (op:y digit:z -> [y z])*:xs -> parseOps(x xs)
  digit = '0'-'9':x                           -> int(x)
  op    =
    | '+' -> Op(makeAdd() "1" "left")
    | '-' -> Op(makeSub() "1" "left")
    | '*' -> Op(makeMul() "2" "left")
    | '/' -> Op(makeDiv() "2" "left")
    | '^' -> Op(makePow() "3" "right")
}
```

In this version all operators are parsed as a list and then handed over
to the `parseOps` function. The operators also return an `Op` object
instead of just a function to evaluate the operator. The `Op` object has
information about the operator\'s precedence (given as a string only
because RLMeta can not express integers in semantic actions) and
associativity:

```
1.  support.py
```

```py
class Op(object):

    def __init__(self, fn, prec, assoc):
        self.fn = fn
        self.prec = int(prec)
        self.assoc = assoc
```

The `parseOps` function uses that information to create a parse tree:

```
1.  support.py
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

It combines the functionality of `leftAssoc` and `rightAssoc` and
additionally also handles precedence. The increment of level if the
operator is left associative ensures that only operators of higher
precedence are parsed in the recursive call. If the next operator is of
the same precedence, the recursive call will return immediately, and
`parseOps` will behave like `leftAssoc`.

Here is how the expression 1+2+3\*4 is parsed:

-   `parseOps` is called with `expr=1`,
    `items=[[OpAdd, 2], [OpAdd, 3], [OpMul, 4]]` and `min_level=0`
-   Since there are still items and the add operator has `prec >= 0`,
    the loop is entered
    -   `op=OpAdd` and `rhs=2`
    -   Since the add operator is left associative, `next_level=2`
    -   `expr=add(1, parseOps(2, [[OpAdd, 3], [OpMul, 4]], 2))`
-   `parseOps` is called with `expr=2`, `items=[[OpAdd, 3], [OpMul, 4]]`
    and `min_level=2`
-   Since the add operator has `prec < 2`, the loop is not entered, and
    `2` is returned
-   The outer loop continues with `expr=add(1, 2)`
    -   `op=OpAdd` and `rhs=3`
    -   Since the add operator is left associative, `next_level=2`
    -   `expr=add(add(1, 2), parseOps(3, [[OpMul, 4]], 2))`
-   `parseOps` is called with `expr=3`, `items=[[OpMul, 4]]` and
    `min_level=2`
-   Since there are still items and the mul operator has `prec >= 2`,
    the loop is entered
    -   `op=OpMul` and `rhs=4`
    -   Since the mul operator is left associative, `next_level=3`
    -   `expr=mul(3, parseOps(4, [], 3))`
-   `parseOps` is called with `expr=4`, `items=[]` and `min_level=3`
-   Since there are no more items, `4` is returned
-   The parent returns `mul(3, 4)`
-   The parent returns `add(add(1, 2), mul(3, 4))`

Let\'s see how the calculator handles the following expressions:

```text
1+2-3
1+2-3^2^2-5
```

Below are the ASTs and the evaluated results:

```text
['-',
 ['+', 1, 2],
 3]
=>
0

['-',
 ['-',
  ['+', 1, 2],
  ['^',
   3,
   ['^', 2, 2]]],
 5]
=>
-83
```

This looks correct. We now have a calculator whose grammar is more
cleanly written and only uses one support function instead of two.

[]{#24d9bf94e3b64dfba2841df8b3cf00b1}Comparison of operator parsers
-------------------------------------------------------------------

All three operator parser functions have a similar structure to them.
Here they are for comparison:

```text
def leftAssoc(expr, items):
    while items:
        op, rhs = items.pop(0)
        expr = op(expr, rhs)
    return expr
```

```text
def rightAssoc(expr, items):
    if items:
        op, rhs = items.pop(0)
        expr = op(expr, rightAssoc(rhs, items))
    return expr
```

```text
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

[]{#5cca99df2e2c4c78ab4bfe4cb946d89a}Resources
----------------------------------------------

The following articles helped me understand how to handle left
associative operators in recursive descent parsers:

-   [Implementing Associativity and Precedence in Recursive Descent
    Expression
    Grammars](http://beastie.cs.ua.edu/proglan/readings/precedence.pdf)
-   [Recursive descent, LL and predictive
    parsers](https://eli.thegreenplace.net/2008/09/26/recursive-descent-ll-and-predictive-parsers)
-   [Some problems of recursive descent
    parsers](https://eli.thegreenplace.net/2009/03/14/some-problems-of-recursive-descent-parsers)
-   [A recursive descent parser with an infix expression
    evaluator](https://eli.thegreenplace.net/2009/03/20/a-recursive-descent-parser-with-an-infix-expression-evaluator)
-   [Top-Down operator precedence
    parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing)
-   [Parsing expressions by precedence
    climbing](https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing)
-   [Pratt Parsers: Expression Parsing Made
    Easy](http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/)

[]{#c1716e6e63034983a86a1c9f621b2a52}Appendix: Test script
----------------------------------------------------------

I used the following Bash script to run the examples:

```
1.  expr.sh
```

```sh
compile() {
    echo "import sys"
    echo "import pprint"
    rlmeta --support
    rlmeta < "$1"
    cat "support.py"
    cat "$2"
    echo "makeAdd = lambda: add"
    echo "makeSub = lambda: sub"
    echo "makeMul = lambda: mul"
    echo "makeDiv = lambda: div"
    echo "makePow = lambda: pow"
    echo "try:"
    echo "    for expr in sys.stdin.read().splitlines():"
    echo "        pprint.pprint(Calculator().run('expr', expr), width=20)"
    echo "except _MatchError as e:"
    echo "    sys.stderr.write(e.describe())"
}

(
    while read -e expr; do
        echo "$expr" | python <(compile "$1" "ast.py")
        echo "=>"
        echo "$expr" | python <(compile "$1" "eval.py")
        echo ""
    done
) | head -n-1
```

It reads expressions from stdin one per line. Then it evaluates them
both using the AST function and the eval function and prints the result.
The `make*` functions are also defined here to just return the
respective function.
