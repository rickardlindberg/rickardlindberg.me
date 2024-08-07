---
title: 'RLMeta: a VM based approach'
date: 2019-08-06
tags: rlmeta
---
In this article I present an alternative implementation of
[RLMeta](/writing/rlmeta/index.html) in which grammars are compiled into
instructions for a virtual machine (VM).

The VM based version builds upon the [optimized
version](/writing/optimizing-rlmeta/index.html) and the implementation
is inspired by [Regular Expression Matching: the Virtual Machine
Approach](https://swtch.com/~rsc/regexp/regexp2.html) and [A Text
Pattern-Matching Tool based on Parsing Expression
Grammars](http://www.inf.puc-rio.br/%7Eroberto/docs/peg.pdf).

-   [Big picture difference](#3df386e2afb1452b87a29185c2de0673)
-   [Parser](#d1a2d050fcc444cfa4991b968a02a315)
-   [Code generator](#24aa83d59704475dba2a010f1b5d3eb0)
    -   [Grammar](#8fc432da8cb94c91baeba2ee6b69f40b)
    -   [Rule](#72b77f7c8a594757a67ef61f9502f8e9)
    -   [Or](#613d04f41a3e4521ad022a3d4e0237ea)
    -   [Scope](#e0d721aa4fae40af8de5a54253243cb0)
    -   [And](#34ee0862096d4fd4aa3342c4b3b6c294)
    -   [Bind](#8275c4d4e16c483f84be55f550e3d39c)
    -   [Star](#963d53648b304544be75045e59c6e183)
    -   [Not](#0efe424b680f477a9bb8c1b9f679ff84)
    -   [MatchCallRule](#24675b6707284a99ba3d3cb17b60e3f2)
    -   [Label](#629b7ffad2d54725ad0af4e73bf434d8)
    -   [SemanticAction](#4ddebcb41511403c8b3fa5c96f14c8f4)
    -   [MatchRule](#fc62b5be27fd4e40978d415ba4070a05)
    -   [MatchRange](#ce3171f9336a4f1a961f4d6fd6665361)
    -   [MatchString](#40fcdbbabfe44b009d3f405060a2055f)
    -   [MatchCharseq](#c6e2326b6b774601b761538455918bbe)
    -   [MatchAny](#de0847d410c6495d9a8ee4e75a0423cc)
    -   [MatchList](#1dc829aba4484ed399eb025d01ed843e)
    -   [Example revisited](#6cc83570ba674d71bccd12660fddefcc)
-   [VM](#e66f8cec6206420abe4736ae32849c53)
    -   [MATCH\_ANY](#8f0fffeab7084a1d9b01d928be4878a8)
    -   [MATCH\_STRING](#a8160d30b886418882ada3a2a884e580)
    -   [MATCH\_RANGE](#d8dcb57d5da141908f28f8c52983d1a6)
    -   [MATCH\_CHARSEQ](#df5dc1b5e5054e548d5c3606daba0d55)
    -   [PUSH\_SCOPE](#49ca0a2fa665466cb90c8c128d5cc835)
    -   [POP\_SCOPE](#b2510846e20d4cd08f9b0cd2dfb5c01d)
    -   [BIND](#1315305641224e4aa99dd00ec5b524b1)
    -   [ACTION](#738356fde86e4c7290b0f99850917ba8)
    -   [LABEL](#d7d285d9586b437ca7d7f51b5deb6193)
    -   [LIST\_START](#b3562467300348028f04ae5034db0f34)
    -   [LIST\_APPEND](#b4aacd38dec8406c9db784e0823c3554)
    -   [LIST\_END](#9940cd44d7fe4777b1c12e335bac33ae)
    -   [PUSH\_STREAM](#fee2dcf573f04ac3a9e7cfd92adbe0c8)
    -   [POP\_STREAM](#87a1eee2cfb94e1a9463a6561c226012)
    -   [FAIL](#34915cae29994179abbe19f2f7e4c267)
    -   [CALL](#8e19cbb2e71c412f95c7e241a69f0901)
    -   [MATCH\_CALL\_RULE](#d42df29209bf475280ac510d9ae31182)
    -   [RETURN](#d104aa6d6b8e43818f662498f6566e38)
    -   [BACKTRACK](#542906286e124874b2975d0ae23f034f)
    -   [COMMIT](#5b99ed36fbde45989829fde6e17d4821)
    -   [Handling failure](#77edfc243204447ba84468c78f05d8e5)
    -   [Optimizations](#95ffb1abf59546e1972c78ee7159b758)
-   [Note on size](#2e3ad1de345642658be2e5910ab5be4e)
-   [Note on performance](#c490369b053247c0b7761f324658aa15)
-   [Code listings for RLMeta](#7e7b4c5318ff4c849c6e98e9ddeaacf0)
    -   [parser.rlmeta](#53f0708b231b468d844291df3fe17cf2)
    -   [codegenerator.rlmeta](#74bf51f7e8d94bcba1747edb477137ef)
    -   [support.py](#60ff77cb85fa455589e3e45cdeac8160)
    -   [compile.sh](#338ad257e61e4025b8859d610a475c6a)
    -   [meta\_compile.sh](#272d17f2d55749b59d7e5ec7b9b8e0ec)

[]{#3df386e2afb1452b87a29185c2de0673}Big picture difference
-----------------------------------------------------------

The optimized version compiles grammars into Python classes which can be
used like this:

```text
g = Grammar()
result = g.run("foo", "input string")
```

The VM based version also compiles grammars into Python classes with the
same interface. The difference is how the `run` method is implemented.
In the optimized version, it calls methods that represent rules in the
grammar. The above call would result in `_rule_foo` being called. That
in turn would make calls to other methods representing other rules in
the grammar. The VM based version instead has a sequence of instructions
and a program counter that keeps track of which instruction to execute.
The VM is invoked from the `run` method.

To make the difference more clear, let\'s look at the `Scream` grammar
that turns its input into a screaming equivalent (`hello` to `HELLO!!`
for example):

```text
Scream {
  scream = char*:xs -> { xs "!!" }
  char   = .:x      -> upper(x)
}
```

The optimized version compiles it into the following class with one
method per rule in the grammar:

```text
class Scream(_Grammar):

    def _rule_scream(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('xs', (lambda: self._star((lambda: self._match_rule('char'))))())),
                (lambda: _SemanticAction(lambda: _Builder.create([
                    _vars.lookup('xs').eval(),
                    '!!',
                ]))),
            ]))()
        )(_Vars()))()

    def _rule_char(self):
        return (lambda: (lambda _vars:
            (lambda: self._and([
                (lambda: _vars.bind('x', self._match_any())),
                (lambda: _SemanticAction(lambda: upper(
                    _vars.lookup('x').eval(),
                ))),
            ]))()
        )(_Vars()))()
```

The VM based version compiles it into the following class with a
sequence of instructions (don\'t worry about understanding them now, it
will be clear later what the instructions mean):

```text
class Scream(_Grammar):

    def __init__(self):
        self._instructions = i = []
        self._labels = l = {}
        def I(name, x=None, y=None):
            i.append((name, x, y))
        def LABEL(name):
            l[name] = len(i)
        LABEL('scream')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(0)
        I('BACKTRACK', 1)
        I('CALL', 'char')
        I('LIST_APPEND')
        I('COMMIT', 0)
        LABEL(1)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: _Builder.create([scope['xs'].eval(), '!!']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('char')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('ACTION', lambda scope: upper(scope['x'].eval()))
        I('POP_SCOPE')
        I('RETURN')
```

The external interface of the classes is exactly the same, but
internally they look rather different. The `run` method in the optimized
version looks like this:

```text
def run(self, rule_name, input_object):
    self._memo = _Memo()
    self._stream = _Stream.from_object(self._memo, input_object)
    result = self._match_rule(rule_name).eval()
    if isinstance(result, _Builder):
        return result.build_string()
    else:
        return result
```

It does some setup and then calls `_match_rule` to start matching.

The `run` method in the VM based version looks like this:

```
1.  support.py
2.  [classes]{.cp}
```

```py
class _Grammar(object):

    def run(self, rule_name, input_object):
        if isinstance(input_object, basestring):
            stream = input_object
        else:
            stream = [input_object]
        result = rlmeta_vm(self._instructions, self._labels, rule_name, stream)
        if isinstance(result, _Builder):
            return result.build_string()
        else:
            return result
```

It also does some setup, but then it hands over the instructions (that
are created in the `__init__` method) to the VM that then executes them.

In the rest of this article I explain how grammars are compiled into VM
instructions and how the VM is implemented.

[]{#d1a2d050fcc444cfa4991b968a02a315}Parser
-------------------------------------------

The parser in the VM based version has one additional rule for labels:

```
1.  parser.rlmeta
2.  [expr1]{.cp}
```

```rlmeta
| space '#' -> ["Label"]
```

A label returns a semantic action that evaluates to a unique number. You
will see later (in [*Or*](#613d04f41a3e4521ad022a3d4e0237ea),
[*Star*](#963d53648b304544be75045e59c6e183), and
[*Not*](#0efe424b680f477a9bb8c1b9f679ff84)) how it is used in the code
generator.

The label feature first had to be added to the optimized version before
it could be used to compile the VM based version. That implementation is
not shown in this article, but the implementation of labels in the VM
based version is similar.

The rest of the parser is exactly the same as in the optimized version:

```
1.  parser.rlmeta
```

```rlmeta
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
    | space '%'                                -> ["MatchCallRule"]
    <<expr1>>
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

[]{#24aa83d59704475dba2a010f1b5d3eb0}Code generator
---------------------------------------------------

The code generator in the VM based version is similarly structured to
the code generator in the optimized version with a grammar and a support
library:

```
1.  codegenerator.rlmeta
```

```rlmeta
CodeGenerator {
  <<rules>>
}
```

```
1.  support.py
```

```py
<<imports>>

<<vm>>

<<classes>>
```

The `ast` rule is exactly the same as in the optimized version:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
ast = [%:x] -> x
```

Then there is an additional rule for when a Python representation of a
value is needed:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
py = .:x -> repr(x)
```

Let\'s move on and look at how VM instructions are generated.

### []{#8fc432da8cb94c91baeba2ee6b69f40b}Grammar

When a `Grammar` AST node is matched, a Python class inheriting
`_Grammar` is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Grammar = .:x ast*:ys -> { "class " x "(_Grammar):\n\n" >
                             "def __init__(self):\n" >
                               "self._instructions = i = []\n"
                               "self._labels = l = {}\n"
                               "def I(name, x=None, y=None):\n" >
                                 "i.append((name, x, y))\n"
                               <
                               "def LABEL(name):\n" >
                                 "l[name] = len(i)\n"
                               <
                               ys
                             <
                           < }
```

The name of the class is the same as the name of the grammar.

The `__init__` method has functionality for creating instructions. An
instruction is represented as a tuple with three elements: the name, the
first argument, and the second argument. Arguments can be `None`.
Instructions are stored in a list. Labels map names to positions in the
instruction list and are stored in a dictionary.

Shorthand names `i` and `l` are used instead of `self._instructions` and
`self._labels` because they are faster. Not using `self` reduces one
dictionary lookup.

The child AST nodes of `Grammar` are assumed to use the `I` and `LABEL`
functions to create instructions.

### []{#72b77f7c8a594757a67ef61f9502f8e9}Rule

When a `Rule` AST node is matched, instructions representing a function
are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Rule = py:x ast:y -> { "LABEL(" x ")\n"
                       y
                       "I('RETURN')\n" }
```

In assembly-like notation (where labels are in the first column and
instructions are in the second column) it looks like this:

```text
<x>:
    <y instructions>
    RETURN
```

The label name is the name of the rule.
[*RETURN*](#d104aa6d6b8e43818f662498f6566e38) instructs the VM to
continue execution at wherever it was before calling this rule.

### []{#613d04f41a3e4521ad022a3d4e0237ea}Or

When an `Or` AST node is matched, instructions representing a choice are
generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Or =
  | ast:x Or:y #:a #:b -> { "I('BACKTRACK', " a ")\n"
                            x
                            "I('COMMIT', " b ")\n"
                            "LABEL(" a ")\n"
                            y
                            "LABEL(" b ")\n" }
  | ast
```

In assembly-like notation it looks like this:

```text
    BACKTRACK a
    <x instructions>
    COMMIT b
a:
    <y instructions>
b:
```

[*BACKTRACK*](#542906286e124874b2975d0ae23f034f) instructs the VM to
push a backtrack entry onto the stack so that it can try matching again
at label `a` if the `x` instructions fail.
[*COMMIT*](#5b99ed36fbde45989829fde6e17d4821) instructs the VM to pop
this backtrack entry off the stack and continue execution at label `b`.
If `x` instructions fail, the second choice at label `a` is tried,
otherwise, execution continues at label `b`. The `y` instructions might
represent another choice or the last choice. If there is only once
choice, only instructions for that choice are generated. In that case,
no `BACKTRACK` and `COMMIT` are needed.

### []{#e0d721aa4fae40af8de5a54253243cb0}Scope

When a `Scope` AST node is matched, instructions creating a new scope
are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Scope = ast:x -> { "I('PUSH_SCOPE')\n"
                   x
                   "I('POP_SCOPE')\n" }
```

In assembly-like notation it looks like this:

```text
PUSH_SCOPE
<x instructions>
POP_SCOPE
```

[*PUSH\_SCOPE*](#49ca0a2fa665466cb90c8c128d5cc835) instructs the VM to
push a new scope onto the stack so that all bindings that are done by
`x` instructions happen in this new scope.
[*POP\_SCOPE*](#b2510846e20d4cd08f9b0cd2dfb5c01d) instructs the VM to
pop this scope off the stack.

### []{#34ee0862096d4fd4aa3342c4b3b6c294}And

When an `And` AST node is matched, instructions for all items in the
sequence are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
And = ast*
```

### []{#8275c4d4e16c483f84be55f550e3d39c}Bind

When a `Bind` AST node is matched, instructions binding the last result
to a name are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Bind = py:x ast:y -> { y
                       "I('BIND', " x ")\n" }
```

In assembly-like notation it looks like this:

```text
<y instructions>
BIND <x>
```

[*BIND*](#1315305641224e4aa99dd00ec5b524b1) instructs the VM to bind the
last result from `y` instructions to the name `x` in the current scope.

### []{#963d53648b304544be75045e59c6e183}Star

When a `Star` AST node is matched, instructions representing a
repetition are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Star = ast:x #:a #:b -> { "I('LIST_START')\n"
                          "LABEL(" a ")\n"
                          "I('BACKTRACK', " b ")\n"
                          x
                          "I('LIST_APPEND')\n"
                          "I('COMMIT', " a ")\n"
                          "LABEL(" b ")\n"
                          "I('LIST_END')\n" }
```

In assembly-like notation it looks like this:

```text
    LIST_START
a:
    BACKTRACK b
    <x instructions>
    LIST_APPEND
    COMMIT a
b:
    LIST_END
```

[*LIST\_START*](#b3562467300348028f04ae5034db0f34) instructs the VM to
create a new list for accumulating results.
[*LIST\_APPEND*](#b4aacd38dec8406c9db784e0823c3554) instructs the VM to
append the last result to this list.
[*LIST\_END*](#9940cd44d7fe4777b1c12e335bac33ae) instructs the VM to
make this list itself the last result. The `BACKTRACK` and `COMMIT`
instructions are used to create control flow for a loop. As long as `x`
instructions succeed, the program loops between label `a` and the
`COMMIT` instruction. As soon as `x` instructions fail, the program
continues execution at label `b`.

### []{#0efe424b680f477a9bb8c1b9f679ff84}Not

When a `Not` AST node is matched, instructions representing negative
lookahead are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Not = ast:x #:a #:b -> { "I('BACKTRACK', " b ")\n"
                         x
                         "I('COMMIT', " a ")\n"
                         "LABEL(" a ")\n"
                         "I('FAIL', 'no match expected')\n"
                         "LABEL(" b ")\n" }
```

In assembly-like notation it looks like this:

```text
    BACKTRACK b
    <x instructions>
    COMMIT a
a:
    FAIL 'no match expected'
b:
```

[*FAIL*](#34915cae29994179abbe19f2f7e4c267) instructs the VM to fail
with the given message. The `BACKTRACK` and `COMMIT` instructions are
used to create control flow for negative lookahead. If `x` instructions
succeed, the `COMMIT` instruction makes the program continue at label
`a`. That immediately fails because the negative lookahead does not
expect a match. If `x` instructions fail, the program continues
execution at label `b`, and the `FAIL` instruction is skipped.

### []{#24675b6707284a99ba3d3cb17b60e3f2}MatchCallRule

When a `MatchCallRule` AST node is matched, an instruction representing
that operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchCallRule = -> { "I('MATCH_CALL_RULE')\n" }
```

In assembly-like notation it looks like this:

```text
MATCH_CALL_RULE
```

[*MATCH\_CALL\_RULE*](#d42df29209bf475280ac510d9ae31182) instructs the
VM to call the rule denoted by the current input object.

### []{#629b7ffad2d54725ad0af4e73bf434d8}Label

When a `Label` AST node is matched, an instruction representing that
operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
Label = -> { "I('LABEL')\n" }
```

In assembly-like notation it looks like this:

```text
LABEL
```

[*LABEL*](#d7d285d9586b437ca7d7f51b5deb6193) instructs the VM to create
a semantic action that evaluates to a unique number.

### []{#4ddebcb41511403c8b3fa5c96f14c8f4}SemanticAction

When a `SemanticAction` AST node is matched, an instruction representing
that operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
SemanticAction = ast:x -> { "I('ACTION', lambda scope: " x ")\n" }
```

In assembly-like notation it looks like this:

```text
ACTION <python lambda>
```

[*ACTION*](#738356fde86e4c7290b0f99850917ba8) instructs the VM to create
a user defined a semantic action. If there is a match, it will be called
with the scope that was active when the action was defined.

Semantic actions are not evaluated by the VM, but rather by Python. The
VM is only responsible for matching and creating semantic actions as
results.

The lambda expression that is the first argument is generated by the
following rules similarly to how it was done in the optimized version:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
String        = py
List          = astList
Builder       = astItems:x      -> { "_Builder.create([" x "])" }
IndentBuilder =                 -> { "_IndentBuilder()"         }
DedentBuilder =                 -> { "_DedentBuilder()"         }
FnCall        = .:x astItems:y  -> { x "(" y ")"                }
VarLookup     = py:x            -> { "scope[" x "].eval()"      }
astItems      =
  | ast:x astItem*:xs           -> { x xs                       }
  |                             -> {                            }
astItem       = ast:x           -> { ", " x                     }
astList       = astListItem*:xs -> { "(" xs "[])"               }
astListItem   =
  | ["ListItemSplice" ast:x]    -> {     x  "+"                 }
  | ast:x                       -> { "[" x "]+"                 }
```

The related pieces in the support library are exactly the same as in the
optimized version:

```
1.  support.py
2.  [imports]{.cp}
```

```py
try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO
```

```
1.  support.py
2.  [classes]{.cp}
```

```py
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
        self.buffer = StringIO()
        self.indentation = 0
        self.on_newline = True

    @property
    def value(self):
        return self.buffer.getvalue()

    def write(self, value):
        for ch in value:
            is_linebreak = ch == "\n"
            if self.indentation and self.on_newline and not is_linebreak:
                self.buffer.write("    "*self.indentation)
            self.buffer.write(ch)
            self.on_newline = is_linebreak

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
```

### []{#fc62b5be27fd4e40978d415ba4070a05}MatchRule

When a `MatchRule` AST node is matched, an instruction representing that
operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchRule = py:x -> { "I('CALL', " x ")\n" }
```

In assembly-like notation it looks like this:

```text
CALL <x>
```

[*CALL*](#8e19cbb2e71c412f95c7e241a69f0901) instructs the VM to call the
given rule.

### []{#ce3171f9336a4f1a961f4d6fd6665361}MatchRange

When a `MatchRange` AST node is matched, an instruction representing
that operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchRange = py:x py:y -> { "I('MATCH_RANGE', " x ", " y ")\n" }
```

In assembly-like notation it looks like this:

```text
MATCH_RANGE <x> <y>
```

[*MATCH\_RANGE*](#d8dcb57d5da141908f28f8c52983d1a6) instructs the VM to
match an object in the given range.

### []{#40fcdbbabfe44b009d3f405060a2055f}MatchString

When a `MatchString` AST node is matched, an instruction representing
that operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchString = py:x -> { "I('MATCH_STRING', " x ")\n" }
```

In assembly-like notation it looks like this:

```text
MATCH_STRING <x>
```

[*MATCH\_STRING*](#a8160d30b886418882ada3a2a884e580) instructs the VM to
match the given string.

### []{#c6e2326b6b774601b761538455918bbe}MatchCharseq

When a `MatchCharseq` AST node is matched, an instruction representing
that operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchCharseq = py:x -> { "I('MATCH_CHARSEQ', " x ")\n" }
```

In assembly-like notation it looks like this:

```text
MATCH_CHARSEQ <x>
```

[*MATCH\_CHARSEQ*](#df5dc1b5e5054e548d5c3606daba0d55) instructs the VM
to match the given sequence of characters.

### []{#de0847d410c6495d9a8ee4e75a0423cc}MatchAny

When a `MatchAny` AST node is matched, an instruction representing that
operation is generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchAny = -> { "I('MATCH_ANY')\n" }
```

In assembly-like notation it looks like this:

```text
MATCH_ANY
```

[*MATCH\_ANY*](#8f0fffeab7084a1d9b01d928be4878a8) instructs the VM to
match any object.

### []{#1dc829aba4484ed399eb025d01ed843e}MatchList

When a `MatchList` AST node is matched, instructions changing the input
stream are generated:

```
1.  codegenerator.rlmeta
2.  [rules]{.cp}
```

```rlmeta
MatchList = ast:x -> { "I('PUSH_STREAM')\n"
                       x
                       "I('POP_STREAM')\n" }
```

In assembly-like notation it looks like this:

```text
PUSH_STREAM
<x instructions>
POP_STREAM
```

[*PUSH\_STREAM*](#fee2dcf573f04ac3a9e7cfd92adbe0c8) instructs the VM to
push the current input object onto the stack so that `x` instructions
see it as the current input stream.
[*POP\_STREAM*](#87a1eee2cfb94e1a9463a6561c226012) instructs the VM to
pop this input stream off the stack.

### []{#6cc83570ba674d71bccd12660fddefcc}Example revisited

The generated instructions for the `Scream` grammar from the beginning
of the article should now make more sense:

```text
Scream {
  scream = char*:xs -> { xs "!!" }
  char   = .:x      -> upper(x)
}
```

```text
class Scream(_Grammar):

    def __init__(self):
        self._instructions = i = []
        self._labels = l = {}
        def I(name, x=None, y=None):
            i.append((name, x, y))
        def LABEL(name):
            l[name] = len(i)
        LABEL('scream')
        I('PUSH_SCOPE')
        I('LIST_START')
        LABEL(0)
        I('BACKTRACK', 1)
        I('CALL', 'char')
        I('LIST_APPEND')
        I('COMMIT', 0)
        LABEL(1)
        I('LIST_END')
        I('BIND', 'xs')
        I('ACTION', lambda scope: _Builder.create([scope['xs'].eval(), '!!']))
        I('POP_SCOPE')
        I('RETURN')
        LABEL('char')
        I('PUSH_SCOPE')
        I('MATCH_ANY')
        I('BIND', 'x')
        I('ACTION', lambda scope: upper(scope['x'].eval()))
        I('POP_SCOPE')
        I('RETURN')
```

There are two labels for the two rules in the grammar. Both blocks of
instructions end with a `RETURN` instruction so that those functions can
be called and returned from. The blocks are are also wrapped in
`PUSH_SCOPE`/`POP_SCOPE` instructions so that variable bindings for the
different calls happen in different scopes. Otherwise they would
overwrite each other. The `scream` rule has a repetition and therefore
also the `LIST_*` instructions. It also has generated label names (`0`
and `1`) to create the loop. The `BIND` instructions bind the last
result to a name in the current scope. The `ACTION` instructions have
Python lambdas as first argument that are evaluated when there is a
match. They get one argument which is the scope that was active when the
action was defined.

Now let\'s move on to the implementation of the VM to understand how the
execution of these instructions work.

[]{#e66f8cec6206420abe4736ae32849c53}VM
---------------------------------------

The VM is implemented as a single Python function with the following
definition:

```
1.  support.py
2.  [vm]{.cp}
```

```py
def rlmeta_vm(instructions, labels, start_rule, stream):
    <<init>>
    <<loop>>
```

It takes a list of instructions to execute, a dictionary of labels, the
name of the start rule, and the input stream. The init section sets up
the VM state:

```
1.  support.py
2.  [vm]{.cp}
3.  [init]{.cp}
```

```py
label_counter = 0
last_action = _ConstantSemanticAction(None)
pc = labels[start_rule]
call_backtrack_stack = []
stream, pos, stream_pos_stack = (stream, 0, [])
scope, scope_stack = (None, [])
fail_message = None
latest_fail_message, latest_fail_pos = (None, tuple())
memo = {}
```

-   `label_counter` is used to generate unique
    [*labels*](#d7d285d9586b437ca7d7f51b5deb6193).
-   `last_action` stores the result of the last expression (which is
    always a semantic action).
-   `pc` is the program counter that determines what instruction to
    execute. It is initialized to the position of the start rule.
-   `call_backtrack_stack` keeps track of what
    [*call*](#8e19cbb2e71c412f95c7e241a69f0901) and
    [*backtrack*](#542906286e124874b2975d0ae23f034f) entries have been
    made.
-   `stream` and `pos` is the topmost item in `stream_pos_stack` which
    keeps track of the current input stream and the position in it.
    (Input streams can be nested.) The topmost item is not stored in the
    list because it would make it slightly less convenient to modify.
-   `scope` is the topmost item in `scope_stack` which keeps track of
    the current scope. The topmost item is not stored in the list
    because it would make it slightly less convenient to modify.
-   `fail_message` stores the current fail message as a tuple
    representing arguments to string formatting. The fail message is not
    formatted immediately to avoid the extra speed cost of formatting if
    the message is not used. (Most fail messages are never used.)
-   `latest_fail_message` and `latest_fail_pos` store the latest failure
    message and its position that will be presented to the user if all
    choices fail.
-   `memo` is the memoization table that stores results of rule matches.

In a picture it looks something like this:

![](image1.png)

<!-- image text -->
<center>
Overview of VM.
</center>

After the init section comes the VM loop:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
```

```py
while True:
    name, arg1, arg2 = instructions[pc]
    if name == "PUSH_SCOPE":
        <<PUSH_SCOPE>>
    elif name == "BACKTRACK":
        <<BACKTRACK>>
    elif name == "CALL":
        <<CALL>>
    elif name == "MATCH_CHARSEQ":
        <<MATCH_CHARSEQ>>
    elif name == "COMMIT":
        <<COMMIT>>
    elif name == "POP_SCOPE":
        <<POP_SCOPE>>
    elif name == "RETURN":
        <<RETURN>>
    elif name == "LIST_APPEND":
        <<LIST_APPEND>>
    elif name == "BIND":
        <<BIND>>
    elif name == "ACTION":
        <<ACTION>>
    elif name == "MATCH_RANGE":
        <<MATCH_RANGE>>
    elif name == "LIST_START":
        <<LIST_START>>
    elif name == "LIST_END":
        <<LIST_END>>
    elif name == "MATCH_ANY":
        <<MATCH_ANY>>
    elif name == "PUSH_STREAM":
        <<PUSH_STREAM>>
    elif name == "POP_STREAM":
        <<POP_STREAM>>
    elif name == "MATCH_CALL_RULE":
        <<MATCH_CALL_RULE>>
    elif name == "FAIL":
        <<FAIL>>
    elif name == "LABEL":
        <<LABEL>>
    elif name == "MATCH_STRING":
        <<MATCH_STRING>>
    else:
        raise Exception("unknown instruction {}".format(name))
    <<handle failure>>
```

First it fetches the instruction pointed to by the program counter. Then
it has an if-chain with cases that handle the different instructions. If
an instruction is not recognized, an exception is raised. Finally, it
has code to handle a failure. Many instructions can fail and therefore
this common code is at the end of the loop. If an instruction succeeds,
it ends with a `continue` statement to ensure that the loop is started
over immediately without executing the code to handle a failure.

The order in which the cases appear in the if-chain is important from a
performance perspective. To get to the last case, all previous cases
need to be tested, which takes time. So it is important that more common
cases appear earlier in the if-chain. I did an instruction frequency
analysis when RLMeta compiled itself to determine the most common
instructions.

The VM makes use of two semantic actions that represent results from
expressions: one for constant values and one for user defined functions:

```
1.  support.py
2.  [classes]{.cp}
```

```py
class _ConstantSemanticAction(object):

    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value
```

```
1.  support.py
2.  [classes]{.cp}
```

```py
class _UserSemanticAction(object):

    def __init__(self, fn, scope):
        self.fn = fn
        self.scope = scope

    def eval(self):
        return self.fn(self.scope)
```

Let\'s move on to the implementation of each instruction.

### []{#8f0fffeab7084a1d9b01d928be4878a8}MATCH\_ANY

This instruction matches any object from the input stream:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [MATCH\_ANY]{.cp}
```

```py
if pos >= len(stream):
    fail_message = ("expected any",)
else:
    last_action = _ConstantSemanticAction(stream[pos])
    pos += 1
    pc += 1
    continue
```

The only time it fails is when the end of stream has been reached. In
that case `fail_message` is set and no `continue` statement is executed
to ensure that the code to [*handle this
failure*](#77edfc243204447ba84468c78f05d8e5) is executed.

Otherwise, the last result is set to a semantic action that evaluates to
the current input object. `pos` is incremented because one object has
been consumed from the input stream. `pc` is incremented so that the
next instruction is executed in the next loop iteration.

### []{#a8160d30b886418882ada3a2a884e580}MATCH\_STRING

This instruction works like
[*MATCH\_ANY*](#8f0fffeab7084a1d9b01d928be4878a8) but also fails if the
current input object is not the string that is given as the first
argument:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [MATCH\_STRING]{.cp}
```

```py
if pos >= len(stream) or stream[pos] != arg1:
    fail_message = ("expected {!r}", arg1)
else:
    last_action = _ConstantSemanticAction(arg1)
    pos += 1
    pc += 1
    continue
```

### []{#d8dcb57d5da141908f28f8c52983d1a6}MATCH\_RANGE

This instruction works like
[*MATCH\_ANY*](#8f0fffeab7084a1d9b01d928be4878a8) but also fails if the
current input object is not in the range given as arguments:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [MATCH\_RANGE]{.cp}
```

```py
if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
    fail_message = ("expected range {!r}-{!r}", arg1, arg2)
else:
    last_action = _ConstantSemanticAction(stream[pos])
    pos += 1
    pc += 1
    continue
```

### []{#df5dc1b5e5054e548d5c3606daba0d55}MATCH\_CHARSEQ

This instruction works as
[*MATCH\_ANY*](#8f0fffeab7084a1d9b01d928be4878a8) but also fails if the
next input objects are not the characters in the string given as the
first argument:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [MATCH\_CHARSEQ]{.cp}
```

```py
for char in arg1:
    if pos >= len(stream) or stream[pos] != char:
        fail_message = ("expected {!r}", char)
        break
    pos += 1
else:
    last_action = _ConstantSemanticAction(arg1)
    pc += 1
    continue
```

### []{#49ca0a2fa665466cb90c8c128d5cc835}PUSH\_SCOPE

This instruction pushes a new scope onto the stack:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [PUSH\_SCOPE]{.cp}
```

```py
scope_stack.append(scope)
scope = {}
pc += 1
continue
```

Pushing a new scope onto the stack means moving the current scope to the
list and assigning a new scope to the current scope. A scope is a
dictionary mapping names to values.

### []{#b2510846e20d4cd08f9b0cd2dfb5c01d}POP\_SCOPE

This instruction pops a scope off the stack:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [POP\_SCOPE]{.cp}
```

```py
scope = scope_stack.pop()
pc += 1
continue
```

### []{#1315305641224e4aa99dd00ec5b524b1}BIND

This instruction binds the last result to the given name in the current
scope:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [BIND]{.cp}
```

```py
scope[arg1] = last_action
pc += 1
continue
```

### []{#738356fde86e4c7290b0f99850917ba8}ACTION

This instruction creates a user defined semantic action:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [ACTION]{.cp}
```

```py
last_action = _UserSemanticAction(arg1, scope)
pc += 1
continue
```

The first argument is a Python lambda that expects a scope as argument.

### []{#d7d285d9586b437ca7d7f51b5deb6193}LABEL

This instruction creates a semantic action that evaluates to a unique
number:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [LABEL]{.cp}
```

```py
last_action = _ConstantSemanticAction(label_counter)
label_counter += 1
pc += 1
continue
```

The label counter is incremented to make all labels unique.

### []{#b3562467300348028f04ae5034db0f34}LIST\_START

This instruction creates a list for accumulating results:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [LIST\_START]{.cp}
```

```py
scope_stack.append(scope)
scope = []
pc += 1
continue
```

The list is actually stored on the scope stack, but it could as well
have been stored in the current scope under a special name.

### []{#b4aacd38dec8406c9db784e0823c3554}LIST\_APPEND

This instruction appends the last result to the accumulation list:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [LIST\_APPEND]{.cp}
```

```py
scope.append(last_action)
pc += 1
continue
```

The current scope is assumed to be an accumulation list.

### []{#9940cd44d7fe4777b1c12e335bac33ae}LIST\_END

This instruction makes the accumulation list the last result:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [LIST\_END]{.cp}
```

```py
last_action = _UserSemanticAction(lambda xs: [x.eval() for x in xs], scope)
scope = scope_stack.pop()
pc += 1
continue
```

The current scope is assumed to be an accumulation list.

The accumulation list is turned into a semantic action that evaluates to
a list where each semantic action in the accumulation list is also
evaluated.

### []{#fee2dcf573f04ac3a9e7cfd92adbe0c8}PUSH\_STREAM

This instruction pushes the current input object onto the stack:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [PUSH\_STREAM]{.cp}
```

```py
if pos >= len(stream) or not isinstance(stream[pos], list):
    fail_message = ("expected list",)
else:
    stream_pos_stack.append((stream, pos))
    stream = stream[pos]
    pos = 0
    pc += 1
    continue
```

It fails if the current input object is not a list. Only lists can
become input streams.

Otherwise the current input object becomes the current input stream and
the position is set to 0.

### []{#87a1eee2cfb94e1a9463a6561c226012}POP\_STREAM

This instruction pops an input stream off the stack:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [POP\_STREAM]{.cp}
```

```py
if pos < len(stream):
    fail_message = ("expected end of list",)
else:
    stream, pos = stream_pos_stack.pop()
    pos += 1
    pc += 1
    continue
```

It fails if not all items in the input stream have been consumed.

The position that is stored on the stack refers to the position where
the input stream was found. This input stream has now been consumed so
the position is therefore incremented.

### []{#34915cae29994179abbe19f2f7e4c267}FAIL

This instruction causes an explicit failure with the given message:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [FAIL]{.cp}
```

```py
fail_message = (arg1,)
```

### []{#8e19cbb2e71c412f95c7e241a69f0901}CALL

This instruction calls the given rule:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [CALL]{.cp}
```

```py
key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))
if key in memo:
    last_action, stream_pos_stack = memo[key]
    stream_pos_stack = stream_pos_stack[:]
    stream, pos = stream_pos_stack.pop()
    pc += 1
else:
    call_backtrack_stack.append((pc+1, key))
    pc = labels[arg1]
continue
```

It fist generates a key which consists of the name of the rule and the
current position in the input stream. Example key:
`('Label', (0, 1, 3))`. If this rule has matched at this position
before, the memoized result is used. Otherwise a call is made.

The memoized result consists of the result from calling the rule and the
state of the input stream. The memoized state is assigned to the VM
state. The state of the input stream stored in the memoization table can
not be modified, hence the `stream_pos_stack[:]`.

To make a call, the next program counter and the key is appended to the
stack. The next program counter stores the position where to continue
execution, and the key is used to store the result in the memoization
table. See [*RETURN*](#d104aa6d6b8e43818f662498f6566e38) for how its
done.

### []{#d42df29209bf475280ac510d9ae31182}MATCH\_CALL\_RULE

This instruction works like [*CALL*](#8e19cbb2e71c412f95c7e241a69f0901)
but instead of getting the rule name from the instruction argument, it
gets it from the input stream.

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [MATCH\_CALL\_RULE]{.cp}
```

```py
if pos >= len(stream):
    fail_message = ("expected any",)
else:
    fn_name = str(stream[pos])
    key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
    if key in memo:
        last_action, stream_pos_stack = memo[key]
        stream_pos_stack = stream_pos_stack[:]
        stream, pos = stream_pos_stack.pop()
        pc += 1
    else:
        call_backtrack_stack.append((pc+1, key))
        pc = labels[fn_name]
        pos += 1
    continue
```

### []{#d104aa6d6b8e43818f662498f6566e38}RETURN

This instruction makes execution continue at wherever it was before the
current rule was called:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [RETURN]{.cp}
```

```py
if len(call_backtrack_stack) == 0:
    return last_action.eval()
pc, key = call_backtrack_stack.pop()
memo[key] = (last_action, stream_pos_stack+[(stream, pos)])
continue
```

If the stack is empty, it means that the end of the `start_rule` has
been reached. In that case, the result is returned from the VM.
Otherwise the `pc` is set to the position that was pushed onto the
stack. The memoization table is also filled in.

### []{#542906286e124874b2975d0ae23f034f}BACKTRACK

This instruction pushes a backtrack entry onto the stack:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [BACKTRACK]{.cp}
```

```py
call_backtrack_stack.append((labels[arg1], pos, len(stream_pos_stack), len(scope_stack)))
pc += 1
continue
```

A backtrack entry consists of the following:

-   The position where to continue execution. The label is given as the
    fist argument to, so its position is looked up in the labels
    dictionary.
-   The position in the input stream where to try matching again.
-   The length of the stream stack.
-   The length of the scope stack.

This information is enough to reset the state and try matching the next
choice at the current position. Actual backtracking is done in
[*Handling failure*](#77edfc243204447ba84468c78f05d8e5).

### []{#5b99ed36fbde45989829fde6e17d4821}COMMIT

This instruction pops a backtrack entry off the stack:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [COMMIT]{.cp}
```

```py
call_backtrack_stack.pop()
pc = labels[arg1]
continue
```

The popped item is assumed to be a backtrack entry, and not a call
entry.

The entry is ignored since the choice succeeded and no backtracking is
needed.

### []{#77edfc243204447ba84468c78f05d8e5}Handling failure

The first step in handling a failure is to figure out if this failure
should be presented to the user. It is done based on the position where
the failure occurred. Failures occurring at later positions are assumed
to be more relevant. The latest fail message is saved like this:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [handle failure]{.cp}
```

```py
fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])
if fail_pos >= latest_fail_pos:
    latest_fail_message = fail_message
    latest_fail_pos = fail_pos
```

Next actual backtracking is done. Items are popped off the stack until a
backtrack entry is found. Backtrack entries are tuples with 4 arguments.

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [handle failure]{.cp}
```

```py
call_backtrack_entry = tuple()
while call_backtrack_stack:
    call_backtrack_entry = call_backtrack_stack.pop()
    if len(call_backtrack_entry) == 4:
        break
```

If no backtrack entry is found, matching failed completely and the user
is notified with an exception. The latest fail message is passed to the
exception along with the position and input stream where the failure
occurred.

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [handle failure]{.cp}
```

```py
if len(call_backtrack_entry) != 4:
    fail_pos = list(latest_fail_pos)
    fail_stream = stream_pos_stack[0][0] if stream_pos_stack else stream
    while len(fail_pos) > 1:
        fail_stream = fail_stream[fail_pos.pop(0)]
    raise _MatchError(latest_fail_message, fail_pos[0], fail_stream)
```

The exception has a `describe` method that formats the error nicely for
the user:

```
1.  support.py
2.  [classes]{.cp}
```

```py
class _MatchError(Exception):

    def __init__(self, message, pos, stream):
        Exception.__init__(self)
        self.message = message
        self.pos = pos
        self.stream = stream

    def describe(self):
        message = ""
        if isinstance(self.stream, basestring):
            before = self.stream[:self.pos].splitlines()
            after = self.stream[self.pos:].splitlines()
            for context_before in before[-4:-1]:
                message += self._context(context_before)
            message += self._context(before[-1], after[0])
            message += self._arrow(len(before[-1]))
            for context_after in after[1:4]:
                message += self._context(context_after)
        else:
            message += self._context("[")
            for context_before in self.stream[:self.pos]:
                message += self._context("  ", repr(context_before), ",")
            message += self._context("  ", repr(self.stream[self.pos]), ",")
            message += self._arrow(2)
            for context_after in self.stream[self.pos+1:]:
                message += self._context("  ", repr(context_after), ",")
            message += self._context("]")
        message += "Error: "
        message += self.message[0].format(*self.message[1:])
        message += "\n"
        return message

    def _context(self, *args):
        return "> {}\n".format("".join(args))

    def _arrow(self, lenght):
        return "--{}^\n".format("-"*lenght)
```

If the input stream is a string, the character where the failure
occurred is highlighted with a few context lines around it like this:

```text
>     | expr:x expr*:xs                          -> ["Scope" ["And" x ~xs]]
>   expr =
>     | expr1:x space ':' name:y                 -> ["Bind" y x
>     | expr1
------^
>   expr1 =
>     | expr2:x space '*'                        -> ["Star" x]
>     | expr2:x space '?'                        -> ["Or" x ["And"]]
Error: expected ']'
```

If the input stream is a list, the whole list is printed as context, and
the item where the failure occurred is highlighted like this:

```text
> [
>   'Bind',
>   'x',
>   ['MatchRule', 'name'],
>   'foo',
----^
> ]
Error: expected end of list
```

Finally the state of the VM is restored:

```
1.  support.py
2.  [vm]{.cp}
3.  [loop]{.cp}
4.  [handle failure]{.cp}
```

```py
(pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry
if len(stream_pos_stack) > stream_stack_len:
    stream = stream_pos_stack[stream_stack_len][0]
stream_pos_stack = stream_pos_stack[:stream_stack_len]
if len(scope_stack) > scope_stack_len:
    scope = scope_stack[scope_stack_len]
scope_stack = scope_stack[:scope_stack_len]
```

The program counter and the position is restored from the backtrack
entry. The stack lengths are used to restore the stacks. A failure might
have occurred deeper in the input stream than when the backtrack entry
was created. Similarly for the scope. Those stacks are therefore
restored so they have the same length as in the backtrack entry.

### []{#95ffb1abf59546e1972c78ee7159b758}Optimizations

The `rlmeta_vm` function is heavily optimized for speed. Here are a few
choices made:

-   It is implemented as a single function to avoid function calls.
-   It handles instructions in a specific order based on how often they
    are used.
-   It duplicates code to avoid some Python instructions.
    -   Most instructions increments the program counter. That could be
        done always, and then only instructions that need to do
        something other than incrementing could do that. But in those
        cases, some extra Python instructions would be executed.
    -   The code for the two call instructions have some similar code.
    -   All instructions use `continue` statements instead of the
        failure handling checking if a fail message was actually set.

These optimizations makes the VM faster and also a little harder to
maintain. But it\'s a trade off.

[]{#2e3ad1de345642658be2e5910ab5be4e}Note on size
-------------------------------------------------

Compared to the optimized version, the VM based version is a little
bigger:

```text
 53 parser.rlmeta
 74 codegenerator.rlmeta
302 support.py
 45 compile.sh
474 total
```

That is 474 lines of code compared to 429 in the optimized version. The
`rlmeta_vm` function is quite optimized and therefore is slightly longer
than it could have been. But 474 lines is still small.

[]{#c490369b053247c0b7761f324658aa15}Note on performance
--------------------------------------------------------

The VM based version also turns out to be faster than the [optimized
version](/writing/optimizing-rlmeta/index.html#3616eda03f5c40458ac9439def097739):

![](image2.png)

<!-- image text -->
<center>
</center>

Being faster was not a goal with the VM based version, but it\'s and
interesting side effect. Perhaps more programming problems would benefit
from a VM based approach? There are probably also more optimizations
that can be made to the instructions to make the VM even faster.

[]{#7e7b4c5318ff4c849c6e98e9ddeaacf0}Code listings for RLMeta
-------------------------------------------------------------

### []{#53f0708b231b468d844291df3fe17cf2}parser.rlmeta

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
    | space '%'                                -> ["MatchCallRule"]
    | space '#'                                -> ["Label"]
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

### []{#74bf51f7e8d94bcba1747edb477137ef}codegenerator.rlmeta

```text
CodeGenerator {
  ast            = [%:x]           -> x
  py             = .:x             -> repr(x)
  Grammar        = .:x ast*:ys     -> { "class " x "(_Grammar):\n\n" >
                                          "def __init__(self):\n" >
                                            "self._instructions = i = []\n"
                                            "self._labels = l = {}\n"
                                            "def I(name, x=None, y=None):\n" >
                                              "i.append((name, x, y))\n"
                                            <
                                            "def LABEL(name):\n" >
                                              "l[name] = len(i)\n"
                                            <
                                            ys
                                          <
                                        <                                    }
  Rule           = py:x ast:y      -> { "LABEL(" x ")\n"
                                        y
                                        "I('RETURN')\n"                      }
  Or             =
    | ast:x Or:y #:a #:b           -> { "I('BACKTRACK', " a ")\n"
                                        x
                                        "I('COMMIT', " b ")\n"
                                        "LABEL(" a ")\n"
                                        y
                                        "LABEL(" b ")\n"                     }
    | ast
  Scope          = ast:x           -> { "I('PUSH_SCOPE')\n"
                                        x
                                        "I('POP_SCOPE')\n"                   }
  And            = ast*
  Bind           = py:x ast:y      -> { y
                                        "I('BIND', " x ")\n"                 }
  Star           = ast:x #:a #:b   -> { "I('LIST_START')\n"
                                        "LABEL(" a ")\n"
                                        "I('BACKTRACK', " b ")\n"
                                        x
                                        "I('LIST_APPEND')\n"
                                        "I('COMMIT', " a ")\n"
                                        "LABEL(" b ")\n"
                                        "I('LIST_END')\n"                    }
  Not            = ast:x #:a #:b   -> { "I('BACKTRACK', " b ")\n"
                                        x
                                        "I('COMMIT', " a ")\n"
                                        "LABEL(" a ")\n"
                                        "I('FAIL', 'no match expected')\n"
                                        "LABEL(" b ")\n"                     }
  MatchCallRule  =                 -> { "I('MATCH_CALL_RULE')\n"             }
  Label          =                 -> { "I('LABEL')\n"                       }
  SemanticAction = ast:x           -> { "I('ACTION', lambda scope: " x ")\n" }
  String         = py
  List           = astList
  Builder        = astItems:x      -> { "_Builder.create([" x "])"           }
  IndentBuilder  =                 -> { "_IndentBuilder()"                   }
  DedentBuilder  =                 -> { "_DedentBuilder()"                   }
  FnCall         = .:x astItems:y  -> { x "(" y ")"                          }
  VarLookup      = py:x            -> { "scope[" x "].eval()"                }
  astItems       =
    | ast:x astItem*:xs            -> { x xs                                 }
    |                              -> {                                      }
  astItem        = ast:x           -> { ", " x                               }
  astList        = astListItem*:xs -> { "(" xs "[])"                         }
  astListItem    =
    | ["ListItemSplice" ast:x]     -> {     x  "+"                           }
    | ast:x                        -> { "[" x "]+"                           }
  MatchRule      = py:x            -> { "I('CALL', " x ")\n"                 }
  MatchRange     = py:x py:y       -> { "I('MATCH_RANGE', " x ", " y ")\n"   }
  MatchString    = py:x            -> { "I('MATCH_STRING', " x ")\n"         }
  MatchCharseq   = py:x            -> { "I('MATCH_CHARSEQ', " x ")\n"        }
  MatchAny       =                 -> { "I('MATCH_ANY')\n"                   }
  MatchList      = ast:x           -> { "I('PUSH_STREAM')\n"
                                        x
                                        "I('POP_STREAM')\n"                  }
}
```

### []{#60ff77cb85fa455589e3e45cdeac8160}support.py

```text
try:
    from cStringIO import StringIO
except:
    from StringIO import StringIO

def rlmeta_vm(instructions, labels, start_rule, stream):
    label_counter = 0
    last_action = _ConstantSemanticAction(None)
    pc = labels[start_rule]
    call_backtrack_stack = []
    stream, pos, stream_pos_stack = (stream, 0, [])
    scope, scope_stack = (None, [])
    fail_message = None
    latest_fail_message, latest_fail_pos = (None, tuple())
    memo = {}
    while True:
        name, arg1, arg2 = instructions[pc]
        if name == "PUSH_SCOPE":
            scope_stack.append(scope)
            scope = {}
            pc += 1
            continue
        elif name == "BACKTRACK":
            call_backtrack_stack.append((labels[arg1], pos, len(stream_pos_stack), len(scope_stack)))
            pc += 1
            continue
        elif name == "CALL":
            key = (arg1, tuple([x[1] for x in stream_pos_stack]+[pos]))
            if key in memo:
                last_action, stream_pos_stack = memo[key]
                stream_pos_stack = stream_pos_stack[:]
                stream, pos = stream_pos_stack.pop()
                pc += 1
            else:
                call_backtrack_stack.append((pc+1, key))
                pc = labels[arg1]
            continue
        elif name == "MATCH_CHARSEQ":
            for char in arg1:
                if pos >= len(stream) or stream[pos] != char:
                    fail_message = ("expected {!r}", char)
                    break
                pos += 1
            else:
                last_action = _ConstantSemanticAction(arg1)
                pc += 1
                continue
        elif name == "COMMIT":
            call_backtrack_stack.pop()
            pc = labels[arg1]
            continue
        elif name == "POP_SCOPE":
            scope = scope_stack.pop()
            pc += 1
            continue
        elif name == "RETURN":
            if len(call_backtrack_stack) == 0:
                return last_action.eval()
            pc, key = call_backtrack_stack.pop()
            memo[key] = (last_action, stream_pos_stack+[(stream, pos)])
            continue
        elif name == "LIST_APPEND":
            scope.append(last_action)
            pc += 1
            continue
        elif name == "BIND":
            scope[arg1] = last_action
            pc += 1
            continue
        elif name == "ACTION":
            last_action = _UserSemanticAction(arg1, scope)
            pc += 1
            continue
        elif name == "MATCH_RANGE":
            if pos >= len(stream) or not (arg1 <= stream[pos] <= arg2):
                fail_message = ("expected range {!r}-{!r}", arg1, arg2)
            else:
                last_action = _ConstantSemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "LIST_START":
            scope_stack.append(scope)
            scope = []
            pc += 1
            continue
        elif name == "LIST_END":
            last_action = _UserSemanticAction(lambda xs: [x.eval() for x in xs], scope)
            scope = scope_stack.pop()
            pc += 1
            continue
        elif name == "MATCH_ANY":
            if pos >= len(stream):
                fail_message = ("expected any",)
            else:
                last_action = _ConstantSemanticAction(stream[pos])
                pos += 1
                pc += 1
                continue
        elif name == "PUSH_STREAM":
            if pos >= len(stream) or not isinstance(stream[pos], list):
                fail_message = ("expected list",)
            else:
                stream_pos_stack.append((stream, pos))
                stream = stream[pos]
                pos = 0
                pc += 1
                continue
        elif name == "POP_STREAM":
            if pos < len(stream):
                fail_message = ("expected end of list",)
            else:
                stream, pos = stream_pos_stack.pop()
                pos += 1
                pc += 1
                continue
        elif name == "MATCH_CALL_RULE":
            if pos >= len(stream):
                fail_message = ("expected any",)
            else:
                fn_name = str(stream[pos])
                key = (fn_name, tuple([x[1] for x in stream_pos_stack]+[pos]))
                if key in memo:
                    last_action, stream_pos_stack = memo[key]
                    stream_pos_stack = stream_pos_stack[:]
                    stream, pos = stream_pos_stack.pop()
                    pc += 1
                else:
                    call_backtrack_stack.append((pc+1, key))
                    pc = labels[fn_name]
                    pos += 1
                continue
        elif name == "FAIL":
            fail_message = (arg1,)
        elif name == "LABEL":
            last_action = _ConstantSemanticAction(label_counter)
            label_counter += 1
            pc += 1
            continue
        elif name == "MATCH_STRING":
            if pos >= len(stream) or stream[pos] != arg1:
                fail_message = ("expected {!r}", arg1)
            else:
                last_action = _ConstantSemanticAction(arg1)
                pos += 1
                pc += 1
                continue
        else:
            raise Exception("unknown instruction {}".format(name))
        fail_pos = tuple([x[1] for x in stream_pos_stack]+[pos])
        if fail_pos >= latest_fail_pos:
            latest_fail_message = fail_message
            latest_fail_pos = fail_pos
        call_backtrack_entry = tuple()
        while call_backtrack_stack:
            call_backtrack_entry = call_backtrack_stack.pop()
            if len(call_backtrack_entry) == 4:
                break
        if len(call_backtrack_entry) != 4:
            fail_pos = list(latest_fail_pos)
            fail_stream = stream_pos_stack[0][0] if stream_pos_stack else stream
            while len(fail_pos) > 1:
                fail_stream = fail_stream[fail_pos.pop(0)]
            raise _MatchError(latest_fail_message, fail_pos[0], fail_stream)
        (pc, pos, stream_stack_len, scope_stack_len) = call_backtrack_entry
        if len(stream_pos_stack) > stream_stack_len:
            stream = stream_pos_stack[stream_stack_len][0]
        stream_pos_stack = stream_pos_stack[:stream_stack_len]
        if len(scope_stack) > scope_stack_len:
            scope = scope_stack[scope_stack_len]
        scope_stack = scope_stack[:scope_stack_len]

class _Grammar(object):

    def run(self, rule_name, input_object):
        if isinstance(input_object, basestring):
            stream = input_object
        else:
            stream = [input_object]
        result = rlmeta_vm(self._instructions, self._labels, rule_name, stream)
        if isinstance(result, _Builder):
            return result.build_string()
        else:
            return result

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
        self.buffer = StringIO()
        self.indentation = 0
        self.on_newline = True

    @property
    def value(self):
        return self.buffer.getvalue()

    def write(self, value):
        for ch in value:
            is_linebreak = ch == "\n"
            if self.indentation and self.on_newline and not is_linebreak:
                self.buffer.write("    "*self.indentation)
            self.buffer.write(ch)
            self.on_newline = is_linebreak

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

class _ConstantSemanticAction(object):

    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value

class _UserSemanticAction(object):

    def __init__(self, fn, scope):
        self.fn = fn
        self.scope = scope

    def eval(self):
        return self.fn(self.scope)

class _MatchError(Exception):

    def __init__(self, message, pos, stream):
        Exception.__init__(self)
        self.message = message
        self.pos = pos
        self.stream = stream

    def describe(self):
        message = ""
        if isinstance(self.stream, basestring):
            before = self.stream[:self.pos].splitlines()
            after = self.stream[self.pos:].splitlines()
            for context_before in before[-4:-1]:
                message += self._context(context_before)
            message += self._context(before[-1], after[0])
            message += self._arrow(len(before[-1]))
            for context_after in after[1:4]:
                message += self._context(context_after)
        else:
            message += self._context("[")
            for context_before in self.stream[:self.pos]:
                message += self._context("  ", repr(context_before), ",")
            message += self._context("  ", repr(self.stream[self.pos]), ",")
            message += self._arrow(2)
            for context_after in self.stream[self.pos+1:]:
                message += self._context("  ", repr(context_after), ",")
            message += self._context("]")
        message += "Error: "
        message += self.message[0].format(*self.message[1:])
        message += "\n"
        return message

    def _context(self, *args):
        return "> {}\n".format("".join(args))

    def _arrow(self, lenght):
        return "--{}^\n".format("-"*lenght)
```

### []{#338ad257e61e4025b8859d610a475c6a}compile.sh

```
1.  compile.sh
```

```sh
#!/bin/bash

set -e

rlmeta_compiler="$(pwd)/$1"

cd "$(dirname "$0")"

to_python_string() {
    python -c 'import sys; sys.stdout.write(repr(sys.stdin.read()))'
}

support_py_string=$(to_python_string < support.py)
support_py=$(python "$rlmeta_compiler" --support)
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

### []{#272d17f2d55749b59d7e5ec7b9b8e0ec}meta\_compile.sh

```
1.  meta\_compile.sh
```

```sh
#!/bin/bash

set -e

cd "$(dirname "$0")"

./compile.sh rlmeta.py > rlmeta1.py

./compile.sh rlmeta1.py > rlmeta2.py

./compile.sh rlmeta2.py > rlmeta3.py

diff rlmeta2.py rlmeta3.py

diff support.py <(python rlmeta3.py --support)

mv rlmeta3.py rlmeta2.py

mv rlmeta2.py rlmeta1.py

mv rlmeta1.py rlmeta.py

echo OK
```
