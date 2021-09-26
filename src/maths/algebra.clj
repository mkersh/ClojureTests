;;; Just enough algebra manipulations to allow me to replicate loan installment  calculations
;;; Previously I did this in Python using SymPy library:
;;; https://github.com/mkersh/MambuAPINotebook/blob/master/Interest%20Calculations.ipynb
;;; I am struggling to find a CAS (https://en.wikipedia.org/wiki/Computer_algebra_system) library for Clojure
;;; So have decided to implement a cutdown version of my own - Enough for my loan installment calculation

(ns maths.algebra)

(defn term [num var-list]
  (let [sorted-vlist (into [] (sort var-list))]
    {:term-num (bigdec num)
     :term-vlist sorted-vlist}))

(defn term-multiple [term1 mult1]
  (let [num (:term-num term1)
        vlist (:term-vlist term1)]
      (if (keyword? mult1)
        (term num (conj vlist mult1))
        (term (* num mult1) vlist))))

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