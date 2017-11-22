(def expr '(+ 1 (* 2 3)))
(def invalid-expr '(+ 1 (* 2 /)))

;; Evaluator

(defn binop? [expr]
  (and (list? expr) (= 3 (count expr))))

(defn binop-op    [expr] (nth expr 0))
(defn binop-left  [expr] (nth expr 1))
(defn binop-right [expr] (nth expr 2))

(binop? 99)
(binop? '(+ 1 99))
(binop-op '(+ 1 99))
(binop-left '(+ 1 99))
(binop-right '(+ 1 99))

(defn eval-expr [expr]
  (cond
    (number? expr) expr
    (binop? expr) (({'+ +
                     '- -
                     '* *
                     '/ /} (binop-op expr))
                       (eval-expr (binop-left expr))
                       (eval-expr (binop-right expr)))))

(eval-expr 99)
(eval-expr '(+ 1 99))
(eval-expr '(- (* 3 3) 1))

;; Bytecode + Evaluator

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

(expr-to-bytecode 99)
(expr-to-bytecode '(+ 1 99))
(expr-to-bytecode '(- (* 3 3) 1))

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

(interpret-bytecode (expr-to-bytecode 99))
(interpret-bytecode (expr-to-bytecode '(+ 1 99)))
(interpret-bytecode (expr-to-bytecode '(- (* 3 3) 1)))

;; Bytecode to C

(defn bytecode-part-to-c [code]
  (cond
    (= (code :op) :plus)  "{PLUS,  0}"
    (= (code :op) :minus) "{MINUS, 0}"
    (= (code :op) :div)   "{DIV,   0}"
    (= (code :op) :mult)  "{MULT,  0}"
    (= (code :op) :push)  (format "{PUSH,  %d}" (code :value))))

(defn bytecode-to-c [code]
  (str
    "int PLUS = 0;\n"
    "int MINUS = 1;\n"
    "int DIV = 2;\n"
    "int MULT = 3;\n"
    "int PUSH = 4;\n"
    "struct op {\n"
    "  int code;\n"
    "  int value;\n"
    "};\n"
    "void main() {\n"
    "op ops[] = {\n"
    (apply str (clojure.string/join ",\n" (map bytecode-part-to-c code)))
    "};\n"
    "};\n"
    ))
(print (bytecode-to-c (expr-to-bytecode expr)))

(print (bytecode-to-c (expr-to-bytecode 1)))
(print (bytecode-to-c (expr-to-bytecode '*)))
(print (bytecode-to-c (expr-to-bytecode expr)))
