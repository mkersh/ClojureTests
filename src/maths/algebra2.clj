;;; Just enough algebra manipulations to allow me to replicate loan installment  calculations
;;; Previously I did this in Python using SymPy library:
;;; https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb
;;; I am struggling to find a CAS (https://en.wikipedia.org/wiki/Computer_algebra_system) library for Clojure
;;; So have decided to implement a cutdown version of my own - Enough for my loan installment calculation
;;; 
;;; GitHub: https://github.com/mkersh/ClojureTests/blob/master/src/maths/algebra.clj 
(ns maths.algebra2
  (:require [clojure.pprint :as pp]))

;;--------------------------------------------------------------------
;; Term functions
;; Terms are the parts of an Expression

;; A term represents an atomic algebraic item
;; So terms can be things like:
;; Pure numbers: 3, 10, 15 etc
;; These are represented in my notation as:
;; 3 = term(3,[])
;; 15 = term(15, [])
;; Multiple of a pure number and variables:
;; 3x, 5x^2, 6xy, etc
;; These variables are represented by the var-list. So:
;; 5x^2 = term(5,[:x :x])
;; 6xy = term(6,[:x :y])
;; NOTE: In my notation you need to use keywords as the variables
(defn term [num var-list]
  (let [sorted-vlist (into [] (sort var-list))]
    {:term-num (bigdec num)
     :term-vlist sorted-vlist}))

;; Multiple a term by mult1, where mult1 is either a number or a variable
(defn term-multiply [term1 mult1]
  (let [num (:term-num term1)
        vlist (:term-vlist term1)]
      (if (keyword? mult1)
        (term num (conj vlist mult1))
        (term (* num mult1) vlist))))

;;--------------------------------------------------------------------
;; Expression functions
;; An Expression consists of a number of Term parts
;; Represented as a map keyed by (:term-vlist term)

(declare add-expr-to-expr)

(defn add-to-expr [expr-map term1]
  (if (:term-num term1) ;; check that term1 is a Term and not an Expr
    (let [num (:term-num term1)
          vlist (:term-vlist term1)
          curr_val (let [curr_term (get expr-map vlist)]
                     (get curr_term :term-num 0))
          new_val  (+ curr_val num)]
      (assoc expr-map vlist (term new_val vlist)))
    ;; Else term1 is actually an Expression, so add differently
    (add-expr-to-expr expr-map term1)))

;; Generates a fn to use in a reduce
(defn multiply-and-add-to-expr [mult1]
  (fn [expr-map term1]
    (let [new-term (term-multiply term1 mult1)]
      (add-to-expr expr-map new-term))))

(defn expr [& term-list]
  (reduce add-to-expr {} term-list))

(defn term-list-from-expr [expr1]
  (let [expr2 (dissoc expr1 :sub-map)
        term-list (vals expr2)]
    term-list))

(defn add-expr-to-expr [new-expr to-add-expr]
  (reduce add-to-expr new-expr (term-list-from-expr to-add-expr)))

(defn expr-multiply [expr mult1]
  (reduce (multiply-and-add-to-expr mult1) {} (vals expr)))

(defn replace-vars-in-term [term1 sub-map]
  (let [num (:term-num term1)
        vlist (:term-vlist term1)]
    (reduce
     (fn [new-term var1]
       (let [var-val (get sub-map var1)]
         (if var-val
           (term-multiply new-term var-val) ;; if the var is in the sub-map multiply the value
           (term-multiply new-term var1) ;; else just preserve the original variable
           )))
     (term num []) ;; start new term with just the num part
     vlist ;; Then multiply the vlist into this new term
     )))

(defn apply-sub-to-expr [expr-map term1]
  (let [new-term (replace-vars-in-term term1 (:sub-map expr-map))]
    (add-to-expr expr-map new-term)))

(defn expr-sub [expr sub-map]
  (reduce apply-sub-to-expr {:sub-map sub-map} (vals expr)))

(defn expr-simplify [expr]
  (let [_ (assert (= (count expr) 1) "ERROR: Cannot simplify (1)")
        _ (assert (get expr []) (str "ERROR: Cannot simplify (2)"))
        val (:term-num (get expr []))]
    val))


;; Tidyup and simplify expr
(defn expr-sub2 [expr sub-map]
  (let [expr1 (expr-sub expr sub-map)
        expr2 (dissoc expr1 :sub-map)]
    (expr-simplify expr2)))

(defn solve
  ([expr var1] (solve expr var1 0))
  ([expr var1 eq-amount]
   (let [expr2 (dissoc expr :sub-map)
         _ (assert (= (count expr2) 2) "ERROR: We can only solve when there is 1 unknown variable")
         _ (assert (get expr [var1]) (str "ERROR: Unknown variable " var1))
         val1 (:term-num (get expr []))
         var-mult (- (:term-num (get expr [var1])))
         eq-amount2 (- val1 eq-amount)]
     {var1 (with-precision 10 (/ eq-amount2 var-mult))} )))


(comment   ;; My REPL test area

(def prin-remain {[:P] {:term-num 1M, :term-vlist [:P]}, [:P :r] {:term-num 1M, :term-vlist [:P :r]}, [:E] {:term-num -1M, :term-vlist [:E]}})
(expr-sub prin-remain {:P 5000 :r 0.1 :E 500.00})

(conj [1 2] 3)
(get nil :a)
(keyword? 1)
(def t1 (term 3 []))
(def t2 (term 3 [:a]))
(def t3 (term 3 [:a :b :a]))
(def t4 (term 115 []))
(def t5 (term 30 [:a]))
(def t7 (term 4 [:z]))

(def t6 (expr t1 t2 t3 t4 t5))
(add-to-expr (add-to-expr {} t1) t4)
(vals (expr t1 t2 t3 t4 t5))

(def e1 (expr t2 t3 t7))
(replace-vars-in-term t3 {:a 2})

(def e2 (expr-sub e1 {:a 2 :z 1} ))
e2
(solve e2 :b 1000)

(expr t1 e1)
t6
(expr-multiply t6 3)
(expr-multiply t6 :r)

(term-multiply t1 3434345.787)

(def map1 {[1 2] "hello"})

(get map1 [2 1 3] 0)

(sort [:b :a :c])

(into [] (sort [2 1]))

;;
)