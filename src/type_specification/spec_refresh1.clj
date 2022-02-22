;; Still haven't actively started using spec yet, which I know I need to do soon (even for just hobby projects)
;; Reading http://blog.cleancoder.com/uncle-bob/2021/06/29/MoreOnTypes.html reminded me about this
(ns type-specification.spec-refresh1
  (:require
   [clojure.spec.alpha :as s]))

(s/def ::position (s/tuple number? number?))
(s/def ::heading (s/and number? #(<= 0 % 360)))
;; s/or takes key+pred pairs, which is different for s/and which just takes preds
;; This allows you to use s/conform to see which branch of the s/or matched - which you don't need with an s/and
(s/def ::pen-start (s/or :nil nil?
                         :pos (s/tuple number? number?)))

(s/def ::mouse (s/keys :req-un [::position
                                ::heading]
                        :opt-un [::pen-start]))

(s/def ::mouse2 (s/keys :req [::position
                                ::heading]
                       :opt [::pen-start]))

;; unqualified version
(defn test-fn-return-valid-map []
  {:post [(s/assert ::mouse %)]}
  {:position [0.0 0.0]
   :heading 0.0
   :pen-start [0.0 6.8]}
   )

;; Qualified keywords version
(defn test-fn-return-valid-map2 []
  {:post [(s/assert ::mouse2 %)]}
  {::position [0.0 0.0]
   ::heading 0.0
   ::pen-start [0.0 6.8]})


(comment
  (s/check-asserts true) ;; Need to turn spec asserts on
  (test-fn-return-valid-map)
  (test-fn-return-valid-map2)
  
  ;; The following will show which part of the s/or is matched
  (s/conform ::pen-start nil)
  (s/conform ::pen-start [0.0 0.0])


  ;; following works even when asserts are off
  (s/conform ::mouse (test-fn-return-valid-map))
;;
  )