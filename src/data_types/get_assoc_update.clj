;;; Testing the 3 essential functions get, assoc and update and their nested equivalents
;;; Which are the ways to extract and update data in a clojure data structuure
(ns data-types.get-assoc-update)

(def test-map {
    :people [{:name "mark"
              :age "18"}
             {:name "John"
              :age "22"}
             {:name "Lisa"
              :age "31"}]
    :f1 "f1_value"
    :f2 {:f2_1 {:f2_1_2 "hello"}}
    :f7 100
    }) 

(def test-list [1 2 {:f1{:f2 {:f3 "hello"}}} 4 5 6 7 8 9 10])
              
(comment

;;; [1] getting things out of data-types

;; getting stuff out of maps
(get test-map :people)
(:people test-map)
(test-map :people)
(get test-map :people2) ;; doesn't exist returns nil
(:people2 test-map)
;; if doesn't exist return a defauly value
(get test-map :people3 "default")

;; getting stuff out of a list/vector
(get test-list 0)
(get test-list 20) ;; doesn't exist returns nil
(get test-list 20 "default")

;; Getting things from nested data structures
(get-in test-map [:people 1 :name])
(get-in test-list [2 :f1 :f2 :f3])
(get-in test-list [0 :f1 :f2 :f3] "default") ;; doesn't exist returns nil/default

;;; [2] Changing or adding a value in a data type
(assoc test-map :f1 "new f1 val")
(assoc test-map :f3 "new f3 val")

;; changing the value in a list
(assoc test-list 2 "new f1 val")
;; If doesn't exist get IndexOutOfBoundsException 
(assoc test-list 20 "new f1 val")

;; changing a nest item in a data structure
(assoc-in test-map [:people 1 :name] "Giles")
(assoc-in test-list [2 :f1 :f2 :f3] "Hey Up!")


;;; [3] Updating a value in a data type
;; More efficient way than getting the current value and then assoc

;; update is similar to assoc but takes a function rather than a value 
(update test-map :f7 inc)
(update test-map :f7 #(* % 3))

(update-in test-list [2 :f1 :f2 :f3] #(str % " - add to end"))


;;; [4] How assoc-in can be implemented
;; This is not needed because it is already a core function but understanding how it can be implemented
;; is interesting for better understanding of how to implement things in a functional language like clojure

(defn assoc-in2 [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in2 (get m k) ks v)) ;; This is where the magic happens using recursion
    (assoc m k v)))

(assoc-in2 test-map [:people 1 :name] "Giles")

;; The 1 key below is added as a map (which is all that can happen) 
(assoc-in2 {} [:people 1 :name] "Giles")
(assoc-in {} [:people 1 :name] "Giles")


;;
)