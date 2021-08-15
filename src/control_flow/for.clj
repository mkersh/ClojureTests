(ns control-flow.for)

(defn for-test []
  (for
   [x (range 3)
    y (range 2)]
    [x y]))

(for-test)

(defn test
  [x zero])



