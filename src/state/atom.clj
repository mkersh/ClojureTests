;;; https://clojure.org/reference/atoms
(ns state.atom)


;; NOTE: memoize already exists in clojure.core BUT here is how it can be implemented
(defn memoize [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn fib [n]
  (if (<= n 1)
    n
    ;; NOTE: recursive calls to fib below may not call this same function!!!
    ;; After the memoize below they will call the new efficient version
    ;; This caught me out initially when I tried to change this function name to fib-slow
    (+ (fib (dec n)) (fib (- n 2)))))

(time (fib 35))

(def fib (memoize fib))

(time (fib 35))

(comment
  ;;; We can do a similar thing with function like factorial as well.
  (defn fac [n]
    (if (= n 0N)
      1N
      (* n (fac (dec n)))))

  (time (fac 100))

  (def fac (memoize fac)))














