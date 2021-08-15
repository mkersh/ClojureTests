;;; https://docs.google.com/document/d/1tiz0qjGfqv0On5k0R3EeIdOgf_9ZZovqcW8yPb5QhCs/edit
;;; GitHub: https://github.com/mkersh/JupyterNotebooks/blob/master/ClojureTests/src/algorithms/puzzles/8queens/8queens.clj
;;; 
(ns algorithms.puzzles.8queens.8queens
;;   (:require
;;    [clojure.pprint :as pp]
;;    )
  )

(declare permute2)

;; loop-through-items:
;; From permute2 this function is called to loop through all the remain
;; items. 
;; 
(defn loop-through-items [res-list it new-perm remain]
  (let [new-remain (remove #(= % it) remain)]
    ;; Remove it from the remain list and add it to the new-perm we are building
    ;; When the new-perm is finished (i.e. no more remain items) we will add to the res-list 
    (permute2 new-remain (conj new-perm it) res-list)))

(defn permute2 [remain new-perm res-list]
  (if (empty? remain)
    (conj res-list new-perm)
    (reduce #(loop-through-items %1 %2 new-perm remain) res-list remain)))

(defn permute
  "Given a sequence 2 return all the permutations of the items"
  [s]
  (permute2 s [] []))

(defn enumerate [s]
  (map #(vector %1 %2) s (range)))

(defn diag-check [f res-set col-it]
  (let [row-pos (first col-it)
        col-pos (second col-it)]
    (conj res-set (f row-pos col-pos))))

(defn queens-on-same-diagonals? [perm]
  (let [enum-perm (enumerate perm)
        set1 (reduce #(diag-check + %1 %2) #{} enum-perm)
        set2 (reduce #(diag-check - %1 %2) #{} enum-perm)
        c (count perm)]
    ;;(prn "set 1: " set1)
    ;;(prn "set 2: " set2)
    (if (= c (count set1) (count set2))
      true
      false)))

;; An alternative implemenation of permute
;; From: https://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list
;; It is half the speed of my version
(defn permute3 [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permute3 (remove #{x} s)))))
     [s])))

(require '[clojure.math.combinatorics :as combo])

;; Another alternative permute function. Slightly quicker than mine
;; but not by much
(def permute4 combo/permutations)

(permute4 (range 8))

;; This is a lot neater than my version BUT very difficult to understand
;; The for is performing some magic here!!
;; Also it is not as quick as my permute
(defn permute5 [s]
  (if (= (count s) 1)
    (list s)
    (for [head s
          tail (permute5 (remove #{head} s))]
      (cons head tail))))

(permute5 (range 3))

(remove #{1} [1 2 3])

(queens-on-same-diagonals? [2 4 6 0 3 1 7 5])

(defn eight-queens
  "Produces all 92 solutions for the 8 Queens solution.
   The board is represent as the columns vector with 8 columns.
   Each item in the vector identified the row that the queen is on for the given column.

   The solution works are follows:
   1) Get all the permutations of (range 8). This gives you all the
   ways that the queens can be positioned without overlapping on any column or row
   2) queens-on-same-diagonals? then checks each permutation to see if they have queens on same diagonal."
  ([n]
  (let [perm-list (permute (range n))]
    (filter queens-on-same-diagonals?  perm-list)))
  
  ([]
   (eight-queens 8)
   )
  )

(count (eight-queens))

(time (count (eight-queens)))


(defn queens-on-same-diagonals2?
  [perm]
  (let [c (count perm)]
    (if (< c 2)
      true
      (queens-on-same-diagonals? perm)
      )
    )
  )

(declare permute-with-filter)

(defn loop-through-items2 [f res-list it new-perm remain]
  (let [new-remain (remove #(= % it) remain)]
    (permute-with-filter f new-remain (conj new-perm it) res-list)))

(defn permute-with-filter
  "Given a sequence s return all the permutations of the items"
  ([f s]
   (permute-with-filter f s [] []))

  ([f remain new-perm res-list]

   (if (f new-perm)
     (if (empty? remain)
       (conj res-list new-perm)
       (reduce #(loop-through-items2 f %1 %2 new-perm remain) res-list remain))
     res-list)))

(defn eight-queens2
  "Produces all 92 solutions for the 8 Queens solution.
   The board is represented as a columns vector with 8 columns.
   Each item in the vector identified the row that the queen is on for the given column.

   The solution works are follows:
   1) Generate permutations but check the diagonal rule as we work out the permutations.
   NOTE: It is a lot faster than my original eight-queen"
  ([]
  (permute-with-filter queens-on-same-diagonals2? (range 8)))
  
  ([n]
   (permute-with-filter queens-on-same-diagonals2? (range n)))
  )

(comment

;; The inefficient version takes ~ 273 ms
(time (count (eight-queens)))

;; For a 10x10 board the inefficient version takes ~ 30 secs
;; NOTE: This is compared to ~ 10 for a version I wrote in python
;; but the python version was using a permutations function in a C library
(time (count (eight-queens 10)))

;; This efficient version takes ~ 34 ms
(time (count (eight-queens2)))

;; The efficient version takes ~ 611 ms for a 10x10 boards
;; This is comparable with a version I wrote in python but no quicker
(time (count (eight-queens2 10)))

;; 12x12 boards takes 20 secs, my python was 18.5
(time (count (eight-queens2 12)))
  
;; 14x 14 board takes 800 secs
(time (count (eight-queens2 14)))



  
  
)
