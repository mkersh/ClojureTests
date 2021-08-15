;;; Write a function that given an input string like "aaabbbaaacccbdddd" returns
;;; (["a",6], ["b", 4], ["c", 3], ["d", 4])
(ns charCount)


;; countMap is a map, keyed by characters and with a list of each of the characters
;; which we will later count to get our result
(defn- add-char-to-map [countMap ch]
  (let [currVal (get countMap ch)]
    ;; could have done the count here but doing a conj likely to be faster
    (assoc countMap ch (conj currVal ch))))

;; It will be passed a (key,value) pair from the countMap described above
;; All we need to do is count the value to determine the number of times
;; the (first it) character was in the string
(defn- count-chars [it]
  (vector (str (first it)) (count (second it))))

(defn count-str-chars [str-to-count]
  (map count-chars (reduce add-char-to-map {} str-to-count)))

;; could have used the standard partition-by function to achieve what I have done in the 
;; reduce:
;; (partition-by identity "aaabbbccc")


(comment

  (count-str-chars "aaabbbaaaabbbcccdddfffgggrrr"))