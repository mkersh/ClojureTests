;;; https://en.wikipedia.org/wiki/Fizz_buzz
(ns exercises.fizzbuzz)


(defn fizzbuzz-num [num]
  (let [fizz? (= (mod num 3) 0)
        buzz? (= (mod num 5) 0)
        fizzbuzz? (and fizz? buzz?)]
    (cond
      fizzbuzz? "FizzBuzz!"
      fizz? "Fizz!"
      buzz? "Buzz!"
      :else num)))

(defn fizzbuzz-list [lst]
  (map fizzbuzz-num lst))

(comment
(fizzbuzz-num 15)

(fizzbuzz-list (range 1 100))

(fizzbuzz-list [1 2 3 3 5 5 20 15 30])

(map fizzbuzz-num (range 1 100))




;
)