;;; Creating my own permute function to get into the  clojure/functional mindset again
;;; This is sufficiently complex to make you have to think 
(ns maths.permute2)

;; This first version is easier to understand but the permutation results are delivered in a nested list structure
;; which is not what we want
(defn permute [items perm]
  (if (first items)
    (for [opt items]
      (permute (disj items opt)  (conj perm opt)))
      perm))

;; This next version works and delivers the results in the way I want
;; It uses two accumulator parameters perm and res
;;    res - will store all our permutation results. 
(defn permute2 [items perm res]
  (if (first items) 
    ;; perm is still incomplete when there are items left. Loop through remaining items
    (reduce  
     (fn [res opt]
       (permute2 (disj items opt)  (conj perm opt) res))
     res
     items)
    ;; if false - perm is complete add to the results list 
    (conj res perm)
    )
)

(comment

;; Clojure set functions that may be useful
(conj #{1 2 3 4})
(disj #{1 2 3 4} 2)
(pop #{1 2 3 4}) ;; Doesn't work for sets
(first #{1 2 3 4})
(first #{})
(into #{} [1 1 2 2 3 3 4 5 6 7 7 8 22])
(map identity #{1 2 3 4})
(conj [1] 2)

(def perm-set (into #{} (range 4)))
(def res (permute perm-set []))
(permute perm-set [])
(permute2 perm-set [] [])
(+ 1 1)
res
(mapcat identity (mapcat identity res))

(reduce (fn [old new] (prn "Old:" old "new: " new)) [] [1 2 3 4])

;;
)
