(ns project-euler.euler701
  (:require [project-euler.support :as supp]
            [algorithms.puzzles.8queens.8queens :as maths]
            [functions.pow :as maths2]))

(defn gen-piece [colour & other-attrs]
  (fn [] {:colour colour :attrs other-attrs}))

(defn white [] (gen-piece :white))
(defn black [] (gen-piece :black))

(defn colour [sq] (:colour (sq)))

(defn permutations-of-squares [sqr-list]
  (prn "permutations-of-squares" sqr-list)
  (into #{} (map (fn [perm] (map (fn [sqr-fn] (colour sqr-fn)) perm))
                 ;; next line blows up when we look at 4x4 permutations
                 ;; need to find another way of doing this
                 (maths/permute sqr-list))))

(defn number-2x2-blocks []
  (let [white-black-comb-list (into #{} (map sort (for [s1 [:white :black] s2 [:white :black] s3 [:white :black] s4 [:white :black]]
                                                    [s1 s2 s3 s4])))
        ;; convert the white-black-comb-list into separate white/black squares
        sq-lists (map (fn [wb-com]
                        (reduce (fn [res col] (if (= col :white) (conj res (white)) (conj res (black)))) [] wb-com))
                      white-black-comb-list)]
    (reduce (fn [res it]
              (+ res (count (permutations-of-squares it)))) 0 sq-lists)))

(defn get-options
  ([n] (get-options n [] []))
  ([n opt res]
   (if (= n 0)
     (conj res opt)
     (let [res1 (get-options (dec n) (conj opt :white) res)]
       (get-options (dec n) (conj opt :black) res1)))))

(defn number-wxh-blocks [w h]
  (let [white-black-comb-list (into #{} (map sort (get-options (* w h))))
        _ (prn "HERE1")
        ;; convert the white-black-comb-list into separate white/black squares
        sq-lists (map (fn [wb-com]
                        (reduce (fn [res col] (if (= col :white) (conj res (white)) (conj res (black)))) [] wb-com))
                      white-black-comb-list)
        _ (prn "HERE2")]
    (reduce (fn [res it]
              (+ res (count (permutations-of-squares it)))) 0 sq-lists)))

;; *****************************************************
;; 2nd Attempt to do solve this

(defn number-nxn-alternatives [n]
  (let [num-squares (* n n)]
    (maths2/pow 2 num-squares)))

;; Let's see if it is possible to iterate through 562949953421312N
;; The answer is no, you can't process this many items
;; So there must be a clever mathematical way to determine the answer
(defonce PROCESS-COUNT (atom 0))
(defn process-perm [_res-list _perm-res]
  (swap! PROCESS-COUNT inc))

(defn gen-nxn-perm-aux [res-list black-list white-list n perm-res]
  (if (= n 0)
    (process-perm res-list perm-res)
    (let [res1 (if (empty? black-list)
                 res-list
                 (gen-nxn-perm-aux res-list (rest black-list) white-list (dec n) (conj perm-res :black)))]
      (if (empty? white-list)
        res1
        (gen-nxn-perm-aux res1 black-list (rest white-list) (dec n) (conj perm-res :white))))))

(defn gen-nxn-perm [n]
  (let [num-squares (* n n)
        num-black-list (range (+ num-squares 1))]
    (loop [bl-list num-black-list
           perm-list []]
      (if (empty? bl-list)
        perm-list
        (let [num-blacks (first bl-list)
              black-list (repeat num-blacks :black)
              num-whites (- num-squares num-blacks)
              white-list (repeat num-whites :white)
              perm-list2 (gen-nxn-perm-aux perm-list black-list white-list num-squares [])]
          (recur (rest bl-list) perm-list2))))))


;;; What I need to do


;; DONE - number of permutations 2^n  e.g 2x2 -> n=4 p = 16
;; See number-nxn-alternatives
;; DONE calculate thee permutations with func perms (wlist black)
;; See gen-nxn-perm
;; wlist will contain a list of whites etc
;; use similar technic to get-options above
;; if there are items leeft on wlist and blist then first try white route and then black
;; if there isn't ant white or blacks left only try one path
;;
;; Idea/plan i flawed though. Doesn't work for (gen-nxn-perm 7) - There is just too many permutations (562949953421312N)
;; You simply can't store these in a list and process
;;
;; Once I have the above will need a way to detect edges
;; For each of the perms create white or black objects with coordinates
;; Interprete the black squares to find the linked edges. Don't think this will be too difficult :)


(comment

  (gen-nxn-perm 2)
  (number-nxn-alternatives 7)
  (supp/start-task :job-pp1  (fn [] (gen-nxn-perm 7)))
  (supp/wait-task :job-pp1 4000)
  (supp/kill-task :job-pp1)
  ;; Number of items we need to process for a 7x7 grid = 562949953421312N
  @PROCESS-COUNT



  (supp/start-task :job-pp1  (fn [] (get-options 4 [] [])))
  (supp/start-task :job-pp1  (fn [] (number-wxh-blocks 2 2)))
  (supp/start-task :job-pp1  (fn [] (number-wxh-blocks 4 4)))
  (supp/start-task :job-pp1  (fn [] (get-options 4 [] [])))
  (supp/start-task :job-pp1  (fn [] (maths/permute [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])))
  (supp/wait-task :job-pp1 4000)
  (supp/kill-task :job-pp1)

;; Below is manual version of number-2x2-blocks
  (let [c1 (count (permutations-of-squares [(white) (white) (white) (white)]))
        c2 (count (permutations-of-squares [(black) (white) (white) (white)]))
        c3 (count (permutations-of-squares [(black) (black) (white) (white)]))
        c4 (count (permutations-of-squares [(black) (black) (black) (white)]))
        c5 (count (permutations-of-squares [(black) (black) (black) (black)]))]
    (+ c1 c2 c3 c4 c5))

  (for [s1 [(white) (black)] s2 [(white) (black)] s3 [(white) (black)] s4 [(white) (black)]]
    (permutations-of-squares [s1 s2 s3 s4]))

  (number-2x2-blocks)

  (colour (black))

  (* 50.1 2 45)
  ;;
  )