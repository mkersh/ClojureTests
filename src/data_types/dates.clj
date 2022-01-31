;; How do you manipulate dates in Clojure?
;; I am currently using https://github.com/dm3/clojure.java-time 
(ns data-types.dates
  (:require [java-time :as t])
  )

(defn adjust-timezone [dateStr timezone]
  (let [date-local (t/zoned-date-time dateStr)
        date2 (t/with-zone date-local timezone)
        zone-offset (str (t/zone-offset date2))
        date-minus-offset (subs dateStr 0 (- (count dateStr) 6))]
    (str date-minus-offset zone-offset)))

(comment 


(t/time-between :days (t/local-date 2015 10 1) (t/local-date 2015 11 1))

(t/time-between :months (t/local-date "yyyy-MM-dd" (subs "2020-06-18T13:37:50+02:00" 0 10)) (t/local-date "yyyy-MM-dd" "2020-11-17"))

(t/zoned-date-time 2015 10)


(t/with-zone (t/zoned-date-time 2021 05) "Europe/London")
(t/with-zone (t/zoned-date-time 2021 07) "Europe/Berlin")

(adjust-timezone "2021-08-27T13:37:50+00:00" "Europe/Berlin")
(adjust-timezone "2021-08-27T13:37:50+00:00" "Europe/London")
(adjust-timezone "2021-08-27T13:37:50+00:00" "Universal")
(adjust-timezone "2021-01-01T13:37:50+00:00" "Europe/Berlin")
(adjust-timezone "2021-01-01T13:37:50+00:00" "Europe/London")
(adjust-timezone "2021-08-27T13:37:50+00:00" "Universal")


(t/available-zone-ids)
;;
)