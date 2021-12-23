(ns tools.capability-category.cap-cat
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            ))

(import java.util.UUID)

(defonce CATFILE "CAPCAT/all-categories.txt")
(defonce ANSWERS-DIR "CAPCAT/Answers/")
(defonce ALLCATS (atom []))
(defonce CAT-MAP (atom {}))
(defonce UI-CATMAP (atom {}))
(defonce UI-CATSTACK (atom []))


(defn delete-directory-recursive
  "Recursively delete a directory."
  [^java.io.File file]
  ;; when `file` is a directory, list its entries and call this
  ;; function with each entry. can't `recur` here as it's not a tail
  ;; position, sadly. could cause a stack overflow for many entries?
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file)]
      (delete-directory-recursive file-in-dir)))
  ;; delete the file or directory. if it it's a file, it's easily
  ;; deletable. if it's a directory, we already have deleted all its
  ;; contents with the code above (remember?)
  (io/delete-file file))

;; WARNING: This next function will delete the entire answers directory
;;          only use this when debgging/testing
(defn delete-ANSWERS-DIR []
  (try (delete-directory-recursive (clojure.java.io/file ANSWERS-DIR))
       (catch Exception _ "Nothing to DELETE")))

(defn save-to-file
  [file-name s]
  (spit file-name s))

;; Get the string format to save to the file
;; NOTE: Planning to save as Clojure EDN format (at least initially)
(defn get-object-str [object]
  (let [out (java.io.StringWriter.)]
    (pp/pprint object out)
    (.toString out)))

(defn save-question-answer [que-ans-map]
  (let [root-dir ANSWERS-DIR
        file-name (str (UUID/randomUUID) ".txt")
        file-path (str root-dir file-name)
        question (:question que-ans-map)
        answer (:answer que-ans-map)
        tags (:tags que-ans-map)
        sep0a "----"
        sep0b "----------------------"
        sep1 (str sep0a "[Question]" sep0b)
        sep2 (str sep0a "[Answer]" sep0b)
        sep3 (str sep0a "[Tags]" sep0b)
        object-str (str sep1 "\n"
                        question "\n"
                        sep2 "\n"
                        answer "\n"
                        sep3 "\n"
                        tags "\n")]
    (io/make-parents file-path)
    (save-to-file file-path object-str)))

(defn create-question-answer []
 (loop []
    
    (let [_ (println "Question:")
          question (read-line)
          _ (println "Answer:")
          answer (read-line)
          _ (println "Category Tags:")
          cat-tags (read-line)]
          (save-question-answer {:question question :answer answer :cat-tags cat-tags})
          )

    (println "q - quit program, <anykey> - to add another question+answer")
    (let [option (read-line)]
      (condp = option
        "q" (println "Goodbye!")
        nil 
        )
      (if (not= option "q")
            ;; Recurse into loop above again
        (recur)
        nil))))

(comment
(UUID/randomUUID)
(java.io.File/createTempFile "filename" ".txt")
(gensym)
(create-question-answer)
(delete-ANSWERS-DIR)
;;;
)

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
  (when (= @CAT-MAP {})(read-cat-index))
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
(terminal-ui)
(pop [])
(sort [1 6 7 8 2 3 4])
(Integer/parseInt "1g")
(pp/pprint (read-cat-index))
@ALLCATS
@UI-CATMAP
@CAT-MAP
(keys @CAT-MAP)

(show-options @CAT-MAP)

(subs "/capability-category/functional" (count "/capability-category/"))
(first '())
(str/split "/architecture" #"/")
;;;
)