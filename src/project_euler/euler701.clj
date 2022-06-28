(ns project-euler.euler701
  (:require [project-euler.support :as supp]
            [algorithms.puzzles.8queens.8queens :as maths]))

(defn gen-piece [colour & other-attrs]
  (fn [] {:colour colour :attrs other-attrs}))

(defn white [] (gen-piece :white) )
(defn black [] (gen-piece :black))

(defn colour[sq] (:colour (sq)))

(defn permutations-of-squares [sqr-list]
  (into #{} (map (fn [perm] (map (fn [sqr-fn] (colour sqr-fn)) perm))
                 (maths/permute sqr-list))))

(comment
  

(let [c1 (count (permutations-of-squares [(white) (white) (white) (white)]))
      c2 (count (permutations-of-squares [(black) (white) (white) (white)]))
      c3 (count (permutations-of-squares [(black) (black) (white) (white)]))
      c4 (count (permutations-of-squares [(black) (black) (black) (white)]))
      c5 (count (permutations-of-squares [(black) (black) (black) (black)]))]
  (+ c1 c2 c3 c4 c5))

(colour (black))
  ;;
  )