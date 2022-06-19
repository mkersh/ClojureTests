;;; Testing/playing with destructing in clojure
(ns functions.destruct)

(def client {:name "Super Co."
             :location "Philadelphia"
             :description "The worldwide leader in plastic tableware."})

(defn test1 [{name :name
        location :location
        description :description}]
  (println name location "-" description))

(defn test2 [{:keys [name location description]}]
  (println name location "-" description))

  (defn test3 [ & {:keys [name location description] :or {location "London", description "Best in world"}}]
    (println name location "-" description))

(comment
  (test1 client)
  (test2 client)
  (test3 :name "harry" :location "Liverpool" :description "Shoes")
  (test3 :name client)
  
  ;;
  )
