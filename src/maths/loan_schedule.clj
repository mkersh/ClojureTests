;;; Experiments in calculating loan schedules from algebraic expressions
;;; Uses a very simple algebraic library I have created maths.algebra
;;; GitHub: https://github.com/mkersh/ClojureTests/blob/master/src/maths/loan_schedule.clj  

(ns maths.loan-schedule
  (:require [maths.algebra :as cas]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(defonce CSV-ROOT (atom "CSV-FILES/"))

(defn get-file-path [fn]
  (str @CSV-ROOT fn))

(defn save-to-file
  [file-name s]
  (prn "spitting to" file-name)
  (spit file-name s))

;; Get the string format to save to the file
;; NOTE: Planning to save as Clojure EDN format (at least initially)
(defn get-object-str [object]
  (let [out (java.io.StringWriter.)]
    (pp/pprint object out)
    (.toString out)))

(defn dump-sched-to-csv [instal-list]
  (let [next-instal (first instal-list)
        rest-instal (rest instal-list)]
    (println
     (str
      (:num next-instal) ","
      (:interest_expected next-instal) ","
      (:principle_remaining next-instal)))
    ;; recurse to next line
    (when (not-empty rest-instal) (dump-sched-to-csv rest-instal))))

(defn save-to-csv-file [fn sched]
  (let [fpath (get-file-path fn)]
    (io/make-parents fpath)
    (spit fpath "" :append false)
    (with-open [out-data (io/writer fpath)]
      (binding [*out* out-data]
        (println "#, Interest Expected, Principle Remaining")
        (dump-sched-to-csv (:instalments sched))))))

(comment ;; Testing sanbox area
  (save-to-csv-file "test.csv" (cas/expand-schedule 5000 1 5))
  (pp/pprint (cas/expand-schedule 5000 1 5))
  (pp/pprint (cas/expand-schedule 5000 10 12))
  (save-to-csv-file "test.csv" (cas/expand-schedule 100000 0.4 100))
  (pp/pprint (cas/expand-schedule 100000 0.4 100))

;; Currently I am not expanding expr as we go along. This results in some very big expresssions
;; The next one was blowing the heap
;; I need to change my functions to allow for expr to be expanded as we go along
  (pp/pprint (cas/expand-schedule 100000 0.4 300)) ;; This one will blow the heap


;;
  )