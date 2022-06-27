;;; https://projecteuler.net/problem=4
;;;
;;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;;; Find the largest palindrome made from the product of two 3-digit numbers.
;;; 

(ns project-euler.euler4
  (:require [project-euler.support :as supp]
            [clojure.string :as str]))

;; Approach-1 
;;
(defn find-biggest-palindrome
  ([](find-biggest-palindrome 100 1000))
  ([st-range end-range]
   (let [res-list (for [x (range st-range end-range) y (range st-range end-range)]
                    (let [prod (* x y)
                          prod-str (str prod)
                          rev-prod-str (str/reverse prod-str)
                          rev-prod-num (Integer/parseInt rev-prod-str)
                          is-palindrome (= prod rev-prod-num)]
                      (if is-palindrome
                        {:x x :y y :palindrome prod}
                        nil)))
         palin-list (filter #(not (nil? %)) res-list)
         biggest-palin (reduce (fn [old obj]
                                 (max old (:palindrome obj))) 0 palin-list)
         _ (prn "find-biggest-palindrome FINISHED")]
     biggest-palin)))

(comment
  ;; Run 
  (supp/start-task :job-pal1 (fn [] (find-biggest-palindrome 10 100)))
  (supp/start-task :job-pal1 (fn [] (find-biggest-palindrome 100 1000)))
  (supp/start-task :job-pal1 (fn [] (find-biggest-palindrome 1000 10000)))
  (supp/wait-task :job-pal1 4000)
  (supp/kill-task :job-pal1)

  (Integer/parseInt (str/reverse "9001"))
  (* 91 99)
  (* 999 999)
  (not (nil? nil))
  (range 100 999)

  ;;
  )