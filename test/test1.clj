
(ns testing
  ;{:clj-kondo/config {:linters {:unresolved-symbol {:exclude [get-loan addition3]}}}}
  (:require [http.api.mambu.account :refer [get-loan]] 
            [clojure.test :refer [deftest run-all-tests is]])
  )

(comment
  (def NewAccountID "SCGC121")

  
  (time (get-loan NewAccountID))
  
  )

(deftest addition3
  (is (= 2 (+ 2 2)))
  (is (= 7 (+ 3 4))))


(run-all-tests )
