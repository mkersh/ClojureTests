(ns tools.capability-category.cap-cat
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(defonce CATFILE "CAPCAT/all-categories.txt")
(defonce ALLCATS (atom []))
(defonce CAT-MAP (atom {}))
(defonce UI-CATMAP (atom {}))
(defonce UI-CATSTACK (atom []))

(defn get-or-create-submap [catmap part]
  (let [submap1 (get catmap part)]
    (if submap1
      catmap
      (assoc catmap part {}))))



(defn add-parts [catmap parts]
  (let [part1 (first parts)
        rest-parts (rest parts)]
    (if (not part1)
      catmap
      (let [catmap1 (get-or-create-submap catmap part1)
            submap (get catmap1 part1)
            submap2 (add-parts submap rest-parts)
            catmap2 (assoc catmap1 part1 submap2)]
        catmap2))))


(defn add-to-hierarchy [cat-map item]
  (let [parts (subvec (str/split item #"/") 1)]
    ;;(prn "parts:" parts)
    (add-parts cat-map parts)))


(defn read-cat-index []
  (let [fileStr (slurp CATFILE)
        all-cats (str/split-lines fileStr)
        chars-to-remove (count "/capability-category")
        all-cats1 (mapv (fn [obj] (subs obj chars-to-remove)) all-cats)
        _ (reset! ALLCATS all-cats)
        catmap (reduce add-to-hierarchy {} all-cats1)
        _ (reset! CAT-MAP catmap)]

    catmap))

(defn keys-and-sort [m]
  (sort (keys m)))

(defn number-or-nil [opt]
  (try
    (Integer/parseInt opt)
    (catch Exception _ nil)))

(defn drill-into [opt]
  (let [
      num-or-nil (number-or-nil opt)
      part (if (number? num-or-nil)
               (let [
                   options (into [] (keys-and-sort @UI-CATMAP))
                   cat (get options num-or-nil)]
                 cat)
               opt)
        catmap (get @UI-CATMAP part)]
    (when catmap
      (reset! UI-CATSTACK (conj @UI-CATSTACK part))
      (reset! UI-CATMAP catmap))))

(defn popit [v]
  (try
    (pop v)
    (catch Exception _ v)))

(defn backup-stack []
  (let [newstack (popit @UI-CATSTACK)]
    (reset! UI-CATSTACK newstack)
    (reset! UI-CATMAP @CAT-MAP) ;; Set back to top-level
    (mapv (fn [part]
            (reset! UI-CATMAP (get @CAT-MAP part))) ;; Reset to previous level
          newstack)))

(defn show-uistack []
  ;;(prn "show-uistack" @UI-CATSTACK)
  (print "Context: /")
  (doall (map #(print (str % "/")) @UI-CATSTACK))
  (println " "))

(defn show-options [capmap]
  (show-uistack)
  (println "Select one of the following:")
  (doall
   (map #(println %2 %1) (keys-and-sort capmap) (iterate inc 0))))

(defn terminal-ui []
  (reset! UI-CATMAP @CAT-MAP) ;; set to the toplevel map
  (reset! UI-CATSTACK [])
  (loop []
    (show-options @UI-CATMAP)
    (println "COMMANDS:")
    (println "b - backup, q - quit program, <part> - to drill into")
    (let [option (read-line)]
      (condp = option
        "q" (println "Goodbye!")
        "b" (backup-stack)
        (drill-into option))
      (if (not= option "q")
            ;; Recurse into loop above again
        (recur)
        nil))))


(comment
(pop [])
(sort [1 6 7 8 2 3 4])
(Integer/parseInt "1g")
(pp/pprint (read-cat-index))
@ALLCATS
@UI-CATMAP
@CAT-MAP
(keys @CAT-MAP)
(terminal-ui)

(show-options @CAT-MAP)

(subs "/capability-category/functional" (count "/capability-category/"))
(first '())
(str/split "/architecture" #"/")
;;;
)