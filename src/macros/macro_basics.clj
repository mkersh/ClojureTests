(ns macros.macro-basics)

;; ref: https://www.braveclojure.com/writing-macros/ 
(defmacro infix
  "Use this macro when you pine for the notation of your childhood"
  [infixed]
  (list (second infixed) (first infixed) (last infixed)))

(defmacro infix-2
  [[operand1 op operand2]]
  (list op operand1 operand2))

(comment
  ;; To expand a macros  
  (macroexpand '(-> {} prn))
  (infix (45 + 12))
  (infix-2 (45 + 12))
  ;;
)

(defmacro and2
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(comment 
(macroexpand '(and))
(macroexpand '(and 5))
(macroexpand '(and 4 6))
(macroexpand '(and2 4 6))  
;;  
)

;; syntax-quoting using ` and unquote within using ~
(defmacro test-mac [x]
  `(prn ~x ~(+ 1 2 3 4) (+ 1 2 3 4))
  )

(comment 
(macroexpand '(test-mac '56))
(test-mac 56)
;;  
)