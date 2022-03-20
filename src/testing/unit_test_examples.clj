;;; #bookmark= 2dce401d-2a1e-4c8d-83c6-b5c18c9f137d
;;; Examples using clojure.test
;;; Using tips from http://blog.jayfields.com/2010/08/clojuretest-introduction.html 
(ns testing.unit-test-examples
  (:use clojure.test))

(defn add [x y] (+ x y))

(deftest add-1-to-1
  (is (= 2 (+ 1 1)))
  (is (= 3 (+ 2 1 )))
  )

(deftest add-x-to-y-a-using-are
  (are [x y] (= 5 (add x y))
    2 3
    1 4
    3 2))

(comment

;; Run all tests across all namespaces loaded
(run-all-tests)

;; Run tests from specific namespaces
(run-all-tests #"testing.unit-test-examples")

;; Run tests in current namespace
(run-all-tests #"nomatch-will-run-tests-in-current-namespace")

;;
)