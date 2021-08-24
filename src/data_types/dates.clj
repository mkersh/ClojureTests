;; How do you manipulate dates in Clojure?
;; I am currently using https://github.com/dm3/clojure.java-time 
(ns data-types.dates
  (:require [java-time :as t])
  )

;;(t/time-between :days (t/local-date 2015) (t/local-date 2016))

;;(refer-clojure :exclude [range iterate format max min])
;;(use 'java-time)


(comment 


(t/time-between :days (t/local-date 2015 10 1) (t/local-date 2015 11 1))

(t/time-between :months (t/local-date "yyyy-MM-dd" (subs "2020-06-18T13:37:50+02:00" 0 10)) (t/local-date "yyyy-MM-dd" "2020-11-17"))

(t/zoned-date-time 2015 10)


(t/with-zone (t/zoned-date-time 2021 05) "Europe/London")
(t/with-zone (t/zoned-date-time 2021 07) "Europe/Berlin")

;;
)