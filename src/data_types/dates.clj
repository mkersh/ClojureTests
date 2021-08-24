;; How do you manipulate dates in Clojure
(ns data-types.dates
  (:require [java-time :as t])
  )

;;(t/time-between :days (t/local-date 2015) (t/local-date 2016))

;;(refer-clojure :exclude [range iterate format max min])
;;(use 'java-time)


(comment 


(t/time-between :days (t/local-date 2015 10 1) (t/local-date 2015 11 1))

(t/time-between :days (t/local-date "yyyy-MM-dd" (subs "2020-06-18T13:37:50+02:00" 0 10)) (t/local-date "yyyy-MM-dd" "2015-11-01"))


;;
)