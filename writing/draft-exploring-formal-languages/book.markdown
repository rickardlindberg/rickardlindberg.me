ONE point:

* Implementing languages can and should be an accessible tool

How much to cover?

* Show the simplest way how to completely implement programming languages
* With examples that work
* It should be real
* It shall be an entry point to programming language implementation
* It should be practical yet simple
* It should omit unnecessary details
* The reader should feel that implementing languages are fascinating

Other potentials

* Inventing languages (how do we come up with languages?)
    * Weave throughout the book?
* Alternatives
    * Other grammars (CFG, ...). We choose PEGs because they are easy to implement.

Random questions:

* Do you write "PEGs"?

================================================
Programming language implementation demystified
Demystifying programming languages
Developing a framework for implementing languages
================================================

# Introduction

As programmers, we express our thoughts in programming languages. But do you
know how languages are implemented? I believe many programmers don't, and see
this as an activity for experts. They see themselves only as users of
programming languages, not implementers. I want to demystify this topic and
make it accessible to more programmers.

During the course of this book I show you different programming languages and
how to implement them. The first part focuses on the simplest possible
implementation of different languages. The second part discusses some of the
shortcuts taken in the first chapter.

# Implementing a languages #

# Describing languages

In this section we examine a mathematical expression language that comprises
addition and multiplication. Here are some valid strings in that language:

    3+4*5
    7
    9*9+1

Here are some invalid strings:

    1++2
    3*

A language is described by two elements: syntax and semantics. Syntax describes
what constitutes valid strings in the language--it accepts `3+4*5` and rejects
`3*`. Semantics gives meaning to expressions. In this language the operators
are assigned the meaning they have in mathematics, and thus, the result of
evaluating `3+4*5` is `23`. For comparison, imagine that multiplication was
assigned the meaning of repetition and addition the meaning of concatenation,
evaluating `3+4*5` would instead result in `344444`. The syntax is the same,
but the semantics is different.

## PEGs

How can we formally describe the syntax of this language? One option is parsing
expression grammars (PEG). PEGs consist of a set of parsing rules that maps
expressions to names. Think of it as a powerful variant of regular expressions.
Here are the rules in PEG:

------------------  ----------------------------------------
Parsing expression  Semantics
------------------  ----------------------------------------
'...'               Match exact text
rule                Match what rule `rule` matches
()                  Match empty string
------------------  ----------------------------------------
a b                 Match expression `a` and `b` in sequence
a / b               Match `a` or `b` if `a` fails
------------------  ----------------------------------------

With these, the grammar for our expression language can be described as follows
(a rule is written as a name, followed by an equals sign, followed by a parsing
expression, followed by a semicolon):

    additive
        = multitive '+' additive
        / multitive
        ;

    multitive
        = number '*' multitive
        / number
        ;

    number
        = '0' / '1' / '2' / '3' / '4'
        / '5' / '6' / '7' / '8' / '9'
        ;

The first rule is the starting rule, and it is read like this: To parse an
additive, first parse a multitive, then parse the plus character, then parse
another additive. If that fails, parse only a multitive. If that also fails,
fail to parse an additive.

## Hand implementation

This grammar can be implement by hand. We will do it now in the Python
programming language. We are aiming for a class called `ExpressionGrammar` with
a single method `parse` that takes a string as input and returns the evaluated
expression:

    >>> grammar = ExpressionGrammar()
    >>> grammar.parse("3+4*5")
    23
    >>> grammar.parse("3*")
    ParseError(...)

The base of our grammar class looks like this:

    class ExpressionGrammar(BaseGrammar):

        def parse(self, text):
            self._input = Input.from_string(text)
            return self.additive()

It inherits from a `BaseGrammar` that defines methods for common parsing
expressions. The `parse` method sets up the input and calls the start rule.
Here is the implementation of additive:

    def additive(self):
        return self._or([self.additive_case_1, self.multitive])

`_or` is a generic method from the base class that implements the rules for
prioritized choice. We will look at its implementation later.  Here is
`additive_case_1`:

    def additive_case_1(self):
        result = self._and([self.multitive, self.plus, self.additive])
        return result[0] + result[2]

`_and` is similarly a generic function that implements the rules for sequence.
It returns a list with the result of each parse function. `multitive` and
`additive` both return numbers, and since this case represents addition, we add
them. Here is `plus`:

    def plus(self):
        return self._text("+")

`multitive` looks similar:

    def multitive(self):
        return self._or([self.multitive_case_1, self.number])

    def multitive_case_1(self):
        result = self._and([self.number, self.times, self.multitive])
        return result[0] * result[2]

    def times(self):
        return self._text("*")

And finally the implementation of `number`:

    def number(self):
        return self._or([
            self.create_digit_parser("0"),
            self.create_digit_parser("1"),
            self.create_digit_parser("2"),
            self.create_digit_parser("3"),
            self.create_digit_parser("4"),
            self.create_digit_parser("5"),
            self.create_digit_parser("6"),
            self.create_digit_parser("7"),
            self.create_digit_parser("8"),
            self.create_digit_parser("9"),
        ])

The result from `create_digit_parser` is a string. We use the `int` function to
convert it to a number so that we can calculate with it in Python.

    def create_digit_parser(self, text):
        def parser():
            return int(self._text(text)),
        return parser

## Base class

Describe base class:

The first piece we need is an abstraction for the input:

    class Input(object):

        @classmethod
        def from_string(data):
            return Inpit(data, 0)

        def __init__(self, buffer, position):
            self._buffer = buffer
            self._position = position

        def head(self):
            return self._buffer[self._position]

        def tail(self):
            return Input(self._buffer, self._position+1)

Here are some example usages:

    >>> i = Input.from_string("hello")
    >>> i.head()
    'h'
    >>> i.tail().head()
    'e'

What we have written above is a recursive descent parser with backtracking.

## Semantic actions

When we implemented the parser for the expression language by hand, we also
implemented the semantics of mathematics: we converted the digits to numbers
and applied addition and multiplication. These are not present in the grammar.
But they can be. We augment the rules with so called semantic actions.  The
grammar would then look like this:

    additive
        = multitive:x '+' additive:y => x+y
        / multitive
        ;

    multitive
        = number:x '*' multitive:y => x*y
        / number
        ;

    number
        = '0':x => int(x)
        / '1':x => int(x)
        / '2':x => int(x)
        / '3':x => int(x)
        / '4':x => int(x)
        / '5':x => int(x)
        / '6':x => int(x)
        / '7':x => int(x)
        / '8':x => int(x)
        / '9':x => int(x)
        ;

The semantic actions are introduced after the `=>` sign. We also need to add
names to expressions so that actions can access the intermediate results.
Names are introduced with `rule:name`.  If no semantic action is given, the
result of the last expression is returned.

The implementation of additive would now look like this:

    def additive(self):
        return self._or([self.additive_case_1, self.multitive])

    def additive_case_1(self):
        result = self._and([self.multitive, self.plus, self.additive])
        x = result[0]
        y = result[2]
        return x+y

    def plus(self):
        return self._text("+")

The only difference is that the intermediate results are named, and the return
expression is copied verbatim from the semantic action.

## Wrap up

What we have now is a way of formally describing both the syntax and semantics
of a language.

# Meta language

In the previous section we developed a syntax for expressing the syntax and
semantics of languages. We used that syntax to describe an expression language
and implemented a parser for it manually. It was manageable because the grammar
was small. But we are going to implement parsers for multiple parsers
throughout this book, and we need a better way of doing it.

The PEG uses syntax and is just like any other programming language. We can
therefore write a PEG that describes how valid PEGs look like.

    grammar
        = *rule:x space end => gen_grammar("GeneratedGrammar", x)
        ;

On bootstrapping: When I first did this, the grammar didn't come out perfect. I
had to iterate many times. I changed the grammar, I update the parser
accordingly, and iterated. After some time I had enough of the parser that it
could parse itself. At that point I could throw away the manually written
parser.

# Exploring virtual machines

# Exploring another language feature...

# Advanced topics #

In the previous chapter we focused on simplicity. In doing so, we skipped
details that are important in practice.

* Left recursion in PEGs
* Good error messages
* What if I don't want to use Python?

# Endings #

In preceding chapters we learned to implement different programming languages.
But we learned something even more useful: a technique for solving programming
problems. We can invent a language in which the problem is easily expressed,
and then implement that language. Our thinking is no longer restricted to the
constructs that our favourite programming language provides.

--

Where to go from here?

I wrote this book to show you that implementing programming languages can be
easy. In doing so, I skipped details, left out useful tools and techniques...
