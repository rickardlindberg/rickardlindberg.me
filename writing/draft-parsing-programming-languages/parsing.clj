;; expr = product eof
;;
;; product
;;  = additive ("*" additive)*
;;  | additive ("|" additive)*
;;
;; additive
;;  = primitive ("+" primitive)*
;;  | primitive ("-" primitive)*
;;
;; primitive
;;  = number
;;  | "(" expr ")"

;; Runtime

(defn p-eof [input]
  (if (= (count input) 0) [true nil input] [false nil input]))

(defn p-number [input]
  (cond
    (= (first input) \1) [true 1 (rest input)]
    (= (first input) \2) [true 2 (rest input)]
    (= (first input) \3) [true 3 (rest input)]
    (= (first input) \4) [true 4 (rest input)]
    (= (first input) \5) [true 5 (rest input)]
    (= (first input) \6) [true 6 (rest input)]
    (= (first input) \7) [true 7 (rest input)]
    (= (first input) \8) [true 8 (rest input)]
    (= (first input) \9) [true 9 (rest input)]
    (= (first input) \0) [true 0 (rest input)]
    :else [false nil input]))

(defn p-str [text]
  (fn [input]
    (cond
      (= (take (count text) input) (seq text))
      [true (take (count text) input) (drop (count text) input)]
      :else
      [false nil input])))

(defn p-and-helper [parsers output input]
  (if (> (count parsers) 0)
    (let [first-result ((first parsers) input)]
      (if (first first-result)
        (let [rest-result (p-and-helper (rest parsers)
                                        (conj output (nth first-result 1))
                                        (nth first-result 2))]
          (if (first rest-result)
            rest-result
            [false nil input]))
        [false nil input]))
    [true output input]))

(defn p-and [parsers]
  (fn [input]
    (p-and-helper parsers [] input)))

(defn p-or-helper [parsers input]
  (if (> (count parsers) 0)
    (let [first-result ((first parsers) input)]
      (if (first first-result)
        first-result
        (p-or-helper (rest parsers) input)))
    [false nil input]))

(defn p-or [parsers]
  (fn [input]
    (p-or-helper parsers input)))

(defn p-star-helper [parser result input]
  (let [first-result (parser input)]
    (if (first first-result)
      (p-star-helper parser (conj result (nth first-result 1)) (nth first-result 2))
      [true result input])))

(defn p-star [parser]
  (fn [input]
    (p-star-helper parser [] input)))

(defn run [parser input]
  (let [[success result input] (parser input)]
    (if success
      result
      nil)))

;; DSL

(declare expr
         product product-clause-1 product-clause-2
         additive additive-clause-1 additive-clause-2
         primitive primitive-clause-1 primitive-clause-2)

(defn expr [input]
  ((p-and [product p-eof]) input))

(defn product [input]
  ((p-or [product-clause-1 product-clause-2]) input))

(defn product-clause-1 [input]
  ((p-and [additive (p-star (p-and [(p-str "+") additive]))]) input))

(defn product-clause-2 [input]
  ((p-and [additive (p-star (p-and [(p-str "-") additive]))]) input))

(defn additive [input]
  ((p-or [additive-clause-1 additive-clause-2]) input))

(defn additive-clause-1 [input]
  ((p-and [primitive (p-star (p-and [(p-str "*") primitive]))]) input))

(defn additive-clause-2 [input]
  ((p-and [primitive (p-star (p-and [(p-str "|") primitive]))]) input))

(defn primitive [input]
  ((p-or [primitive-clause-1 primitive-clause-2]) input))

(defn primitive-clause-1 [input]
  (p-number input))

(defn primitive-clause-2 [input]
  ((p-and [(p-str "(") expr (p-str ")")]) input))

(run expr "3*4")
(run expr "3+4*5")
