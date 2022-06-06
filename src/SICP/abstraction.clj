(ns SICP.abstraction)

;; From watching https://www.youtube.com/watch?v=DrFkf-T-6Co:
;; Wanted to test whether defn in clojure workss the same as define in scheme:
;; In terms of inner defn declarations only being available in the block they are defined

(defn outer-func []

  (defn inner-func []  ;; The kondo warning is a warning that you should not do this
    (println "inner func called"))

  (println "outer func called" (inner-func)))


(defn outer-func2 []

  (let [inner-func2 (fn [] (println "inner func22 called"))]
    (println "outer func22 called" (inner-func2)))

  )


(comment
  
(outer-func)
;; No the inner-func has global scope
(inner-func)


;; If you want to hide the inner-func
(outer-func2)
(inner-func2)

  )