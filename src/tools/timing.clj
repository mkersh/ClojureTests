(ns tools.timing)

(defn getTimer []
  (. System (nanoTime)))

(defn showTimeDiff [title startTimer]
  (let [curr-time (getTimer)
        time-diff (/ (- curr-time startTimer) 1000000.0)]
    (prn title time-diff)
    time-diff))


(comment

;; time function is a common way in clojure to time things  
(time (prn "Hello"))

;; Finer control over timing how long things take  
(let [startTimer (getTimer)
      _ (Thread/sleep 2000)
      timeDiff (showTimeDiff "API call took (ms):" startTimer)]  
    (prn "Time Took:" timeDiff))
  
;;  
)