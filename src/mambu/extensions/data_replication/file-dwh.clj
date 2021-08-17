;;; Simple file/folder DWH for storing objects
(ns mambu.extensions.data-replication.file-dwh
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
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
  (delete-directory-recursive (clojure.java.io/file (dwh-root-dir {}))))

(defn dwh-get-file-path [root-dir object-type object]
  (str root-dir "/" (symbol object-type) "/" (get object "encodedKey") ".edn"))

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
        file-path (dwh-get-file-path root-dir object-type object)
        object-str (get-object-str object)]
    (io/make-parents file-path)
    (save-to-file file-path object-str)))


(comment
(delete-DWH) ;; This will recursively delete the DWH folder structure

(save-object
 {"encodedKey" "encKey1" :f1 "value1" :f2 {:f2.1 "val2.1"} :f3 [1 2 3 4]}
 {:object-type :client})

;;
)