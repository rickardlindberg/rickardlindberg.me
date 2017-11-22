---
title: The expr programming language
date: 2017-11-18
---

In this article we implement a programming language called *expr*. It is a
language for describing simple calculations with integers. It is a subset of
most general purpose programming languages.

An *expr* program consists of a single expression. An expression can either be
an integer or a binary operation. The following are valid *expr* programs:

    99
    1 + 99
    3 * 3 - 1

In this article we skip writing a parser. A parser is a function that turns the
textual representation into an abstract syntax tree (AST). In this article we
instead enter *expr* programs directly in AST form so that we can focus on
writing an interpreter for *expr* and also a compiler to bytecode for execution
on a virtual stack machine.

We use Clojure to implement this programming language. Here are the above
examples represented as data structures in Clojure:

```clojure
99
'(+ 1 99)
'(- (* 3 3) 1)
```

And here is a graphical representation of those ASTs:

![ASTs for sample expressions.](ast.png)

Integers are represented as integers in Clojure and binary operations are
represented as lists with three elements: the operator symbol, the left
expression, and the right expression. We define primitives for working with
them:

```clojure
(defn binop? [expr]
  (and (list? expr) (= 3 (count expr))))

(defn binop-op    [expr] (nth expr 0))
(defn binop-left  [expr] (nth expr 1))
(defn binop-right [expr] (nth expr 2))
```

Examples:

```clojure
(binop? 99)              ;; => false
(binop? '(+ 1 99))       ;; => true
(binop-op '(+ 1 99))     ;; => +
(binop-left '(+ 1 99))   ;; => 1
(binop-right '(+ 1 99))  ;; => 99
```

## Interpreter

The purpose of the interpreter is to convert an *expr* AST into an integer.

We implement it as a recursive function:

```clojure
(defn interpret-expr [expr]
  (cond
    (number? expr) expr
    (binop? expr) (({'+ +
                     '- -
                     '* *
                     '/ /} (binop-op expr))
                       (interpret-expr (binop-left expr))
                       (interpret-expr (binop-right expr)))))
```

If the expression is a number, we just return it as is. Otherwise we interpret
both the left and right side of the binary operation and apply the
corresponding Clojure function to the results.

Examples:

```clojure
(interpret-expr 99)              ;; => 99
(interpret-expr '(+ 1 99))       ;; => 100
(interpret-expr '(- (* 3 3) 1))  ;; => 8
```

## Bytecode compiler

Instead of interpreting an expression yielding an integer, the purpose of the
bytecode compiler is to generate code than when run on a virtual stack machine
produces the resulting integer.

A virtual stack machine works by having a stack of numbers and operations for
modifying the stack. For example, the expression

    1 + 99

could be expressed in bytecode as

    PUSH 1
    PUSH 99
    PLUS

* Push the number 1 onto the stack
* Push the number 99 onto the stack
* Pop 2 numbers off the stack, add them, and push the result onto the stack

We implement this conversion similarly to `interpret-expr` but instead of
calculating the result, we generate a list of bytecode instructions:

```clojure
(defn expr-to-bytecode [expr]
  (cond
    (number? expr) (list {:op :push :value expr})
    (binop? expr) (concat
                    (expr-to-bytecode (binop-left expr))
                    (expr-to-bytecode (binop-right expr))
                    [({'+ {:op :plus}
                       '- {:op :minus}
                       '* {:op :mult}
                       '/ {:op :div}} (binop-op expr))])))
```

The bytecode is represented in Clojure as a list of instructions. An
instruction is represented as a map with the mandatory key `:op` representing
the name of the operation, and the optional key `:value` present only for
`:push` operations. If the expression is a number, the bytecode is simply one
instruction that pushes that number onto the stack. Otherwise we generate
bytecode for the left expression and the right expression. After those
instructions have been run, the stack contains two numbers. Then we add the
binary operation instruction that pops those two numbers and pushed the result
onto the stack.

Examples:

```clojure
(expr-to-bytecode 99)              ;; => ({:op :push, :value 99})
(expr-to-bytecode '(+ 1 99))       ;; => ({:op :push, :value 1}
                                   ;;     {:op :push, :value 99}
                                   ;;     {:op :plus})
(expr-to-bytecode '(- (* 3 3) 1))  ;; => ({:op :push, :value 3}
                                   ;;     {:op :push, :value 3}
                                   ;;     {:op :mult}
                                   ;;     {:op :push, :value 1}
                                   ;;     {:op :minus})
```

## Virtual machine implementation

In order to run the bytecode produced in the previous section, we need an
implementation of the stack virtual machine.

```clojure
(defn interpret-bytecode
  ([code] (interpret-bytecode code '()))
  ([[next_code & rest_code] [right left & srest :as stack]]
   (cond
     (nil? next_code) right
     (= (next_code :op) :plus)  (recur rest_code (cons (+ left right) srest))
     (= (next_code :op) :minus) (recur rest_code (cons (- left right) srest))
     (= (next_code :op) :mult)  (recur rest_code (cons (* left right) srest))
     (= (next_code :op) :div)   (recur rest_code (cons (/ left right) srest))
     (= (next_code :op) :push)  (recur rest_code (cons (next_code :value) stack)))))
```

Examples:

```clojure
(interpret-bytecode (expr-to-bytecode 99))
(interpret-bytecode (expr-to-bytecode '(+ 1 99)))
(interpret-bytecode (expr-to-bytecode '(- (* 3 3) 1)))
```

## Summary

...
