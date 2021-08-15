(ns data-types.maps.navigate.fun_with_maps
;;   (:require
;;    [clojure.pprint :as pp]
;;    )
  )

;;; Tests and experiments for navigating and manipulating map data-structures.
;;; This is particularly important for being able to filter and disect API response data.


(println "Welcome to Fun with Maps!!")

;; Got the following from: https://stackoverflow.com/questions/28091305/find-value-of-specific-key-in-nested-map
;; 
(defn find-nested
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (some k)))

(def map1 {:a {:b {:c 1}, :d 2}})

(def map2
  (->> (tree-seq map? vals map1)
       (filter map?)))

(->> (tree-seq map? vals map1)
     (filter map?))

map2
(some :c map2)

;; ------------------------------------------


(defn find-all-nested
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (keep k)))

(find-all-nested {:a {:b {:c 1}, :c 2}} :c)


;; --------------------------------------------


;; I however am looking for a function that can:
;;     - Search for a thing in a nested EDN datastructure and match on either key or value
;;     - Record the access path to the found items

(defn find-path [m x]
  
  )

(tree-seq map? vals map1)

;; Some test data
(def testMap
  {:f1 {:f1.1 {:f1.1.1 "val :f1.1.1" :f1.1.2 "val :f1.1.2"}
        :f1.2 3}
   :f2 {:f2.1 {:f2.1.1 "val :f2.1.1"} :f2.2 "hello world"}})