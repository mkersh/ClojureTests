(ns functions.defmulti2)

(def num-add clojure.core/+)

(defmulti + (fn [ & vars ] (type (first vars))))

(defmethod + java.lang.Long [& vars]
  (prn "add up Longs" vars)
  (apply num-add vars))

(defmethod + java.lang.String [& vars]
  (prn "Concat strings together" vars)
  (apply str vars))


(comment 
(ns-unmap *ns* 'add)
(type 1)
(type "sfsfs")
(+ 1 2 3 4 5)
(+ "this" "that" 3 4 5)
)