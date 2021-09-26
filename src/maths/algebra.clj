;;; Just enough algebra manipulations to allow me to replicate loan installment  calculations
;;; Previously I did this in Python using SymPy library:
;;; https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb
;;; I am struggling to find a CAS (https://en.wikipedia.org/wiki/Computer_algebra_system) library for Clojure
;;; So have decided to implement a cutdown version of my own - Enough for my loan installment calculation

(ns maths.algebra)

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

;; Multiple a term by mult1, where mult1 is either a number of a variable
(defn term-multiple [term1 mult1]
  (let [num (:term-num term1)
        vlist (:term-vlist term1)]
      (if (keyword? mult1)
        (term num (conj vlist mult1))
        (term (* num mult1) vlist))))

;;--------------------------------------------------------------------
;; Expression functions
;; An Expression consists of a number of Term parts
;; Represented as a map keyed by (:term-vlist term)

(defn add-to-expr [expr-map term1]
  (let [num (:term-num term1)
        vlist (:term-vlist term1)
        curr_val (let [curr_term (get expr-map vlist)]
                   (get curr_term :term-num 0))
        new_val  (+ curr_val num)]
    (assoc expr-map vlist (term new_val vlist))))


(defn expr [& term-list]
  (reduce add-to-expr {} term-list))


(comment

(get nil :a)
(keyword? 1)
(def t1 (term 3 []))
(def t2 (term 3 [:a]))
(def t3 (term 3 [:a :b :a]))
(def t4 (term 115 []))
(def t5 (term 30 [:a]))

(expr t1 t2 t3 t4 t5)
(add-to-expr (add-to-expr {} t1) t4)

(term-multiple t1 3434345.787)

(def map1 {[1 2] "hello"})

(get map1 [2 1 3] 0)

(sort [:b :a :c])

(into [] (sort [2 1]))

;;
)