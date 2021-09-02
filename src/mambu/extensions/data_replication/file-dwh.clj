;;; Simple file/folder DWH for storing objects
(ns mambu.extensions.data-replication.file-dwh
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            ;;[clojure.data.json :as json]
            ))

(defn dwh-root-dir [_]
  "MAMBU-DWH/")

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

(defn delete-DWH []
  (try (delete-directory-recursive (clojure.java.io/file (dwh-root-dir {})))
       (catch Exception _ "Nothing to DELETE")))

(defn dwh-get-file-path [root-dir object-type object]
  (str root-dir (symbol object-type) "/" (get object "encodedKey") ".edn"))

(defn dwh-get-lastpos-path [root-dir object-type]
  (str root-dir (symbol object-type) "/.lastPosition" ))

(defn dwh-get-cache-path [root-dir object-type]
  (str root-dir (symbol object-type) "/.cache"))

(defn save-to-file
  [file-name s]
  (spit file-name s))

;; Get the string format to save to the file
;; NOTE: Planning to save as Clojure EDN format (at least initially)
(defn get-object-str [object]
  (let [out (java.io.StringWriter.)]
    (pp/pprint object out)
    (.toString out)))

;; Save object to a file
(defn save-object [object context]
  (let [object-type (:object-type context)
        root-dir (dwh-root-dir context)
        get-file-path-fn (or (:get-file-path-fn context) dwh-get-file-path)
        file-path (get-file-path-fn root-dir object-type object)
        object-str (get-object-str object)]
    ;;(prn "save-object:" file-path)    
    (io/make-parents file-path)
    (save-to-file file-path object-str)))

(defn read-object [fpath]
  (read-string (slurp fpath)))

(defn save-last-position [object-type last-position]
  (let [root-dir (dwh-root-dir {})
        file-path (dwh-get-lastpos-path root-dir object-type)]
    ;;(prn "save last-position:" file-path)
    ;;(io/make-parents file-path)
    (save-to-file file-path last-position)))

(defn read-last-position [object-type]
  (let [root-dir (dwh-root-dir {})
        file-path (dwh-get-lastpos-path root-dir object-type)]
    (read-object file-path)))

(defn save-cache [object-type cache-map]
  (let [root-dir (dwh-root-dir {})
        file-path (dwh-get-cache-path root-dir object-type)]
    (io/make-parents file-path)
    (save-to-file file-path cache-map)))

(defn read-cache [object-type]
  (let [root-dir (dwh-root-dir {})
        file-path (dwh-get-cache-path root-dir object-type)]
    (read-object file-path)))


;; https://rosettacode.org/wiki/Walk_a_directory/Recursively#Clojure
(defn walk-dir [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

;; ---------------------------------------------------------
;; functions to call when walking the DWH

(defn object-last-mod [^java.io.File f]
  (let [obj (read-object f)
        lastModDate (get obj "lastModifiedDate")
        encodedKey (get obj "encodedKey")]
    [lastModDate encodedKey]
    ))

;; Maker functions for creating a function that can be called by map or reduce
;; We need this to capture the to-find as part of the func  
(defn find-string-maker [to-find]
  (fn [^java.io.File f]
    (let [obj (read-object f)
          objStr (pr-str obj)
          encodedKey (get obj "encodedKey")
          isMatch  (str/includes? objStr to-find)]
      (if isMatch encodedKey nil))))

(defn print-file-details [^java.io.File f]
  (prn (type f))
  (println (.getPath f)))

;; ----------------------------------------------------------
;; The main entry points for walking the DWH

(defn map-all-DWH-files [func]
  (map func (walk-dir (dwh-root-dir {}) #".*\.edn")))

(defn reduce-all-DWH-files [func res]
  (reduce func res (walk-dir (dwh-root-dir {}) #".*\.edn")))

;; Sort all the entries in the DWH by lastModifiedDate
(defn sort-DWH-lastmoddate []
(let [unsorted-list (map-all-DWH-files object-last-mod)]
  ;; To reverse change the order of the compare params
  (sort #(compare %2 %1) unsorted-list))
)

;; Find all matches to a string in the DWH
(defn find-all-matches-DWH [to-match]
  (filter some? (map (find-string-maker to-match) (walk-dir (dwh-root-dir {}) #".*\.edn"))))

(comment
(delete-DWH) ;; This will recursively delete the DWH folder structure

(save-object
 {"encodedKey" "encKey1" :f1 "value1" :f2 {:f2.1 "val2.1"} :f3 [1 2 3 4]}
 {:object-type :client})

(map-all-DWH-files print-file-details)
(map-all-DWH-files object-last-mod)

(pp/pprint (take 1000 (sort-DWH-lastmoddate)))

(find-all-matches-DWH "James")


(reduce-all-DWH-files (fn [_ new] (print-file-details new)) []) 

(read-object "MAMBU-DWH/client/8a19cde572757f19017275dbf9dd0109.edn")

(save-last-position :client [1 2 3])
(read-last-position :client)


;;
)