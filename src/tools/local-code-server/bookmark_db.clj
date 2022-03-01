;;; Bookmark functions:
;;; To support GotoFile working from a bookmark only
;;;
;;; Requires:
;;; - (1) Keeping an in-memory cache of known bookmark
;;; - (2) Persisting #1 to disk
;;; - (3) Being able to refresh #1 with new added bookmarks
;;;
;;; Bookmark's have the following format: #bookmark= 9d814ac2-02dc-47a5-980d-3a23108de57a
;;; The plan is to place these in clojure src files and for the GotoFile tool to be able to jump to these
;;;
(ns tools.local-code-server.bookmark-db
  (:require  [clojure.java.io :as io]
             [clojure.java.shell :as sh]
             [clojure.pprint :as pp]
             [clojure.string :as str]))

(defonce BM-ROOT (atom "BOOKMARKS/"))
(defonce BOOKMARK_CACHE (atom {}))

;; *************************************
;; Functions for reading/writing the @BOOKMARK_CACHE to disk
;;

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

(defn delete-BM []
  (try (reset! BOOKMARK_CACHE {})
       (delete-directory-recursive (clojure.java.io/file @BM-ROOT))
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

;; Save object to a file
(defn save-bookmarks [object]
  (let [file-path (str @BM-ROOT "bookmarks.edn")
        object-str (get-object-str object)]
    ;;(prn "save-object:" file-path)    
    (io/make-parents file-path)
    (save-to-file file-path object-str)))

(defn read-object [fpath]
  (read-string (slurp fpath)))

(defn read-bookmarks []
  (let [fpath (str @BM-ROOT "bookmarks.edn")]
   (reset! BOOKMARK_CACHE (read-object fpath))))


;; *************************************
;; Main Bookmark functions
;;

(defn add-bookmark [bookmark-uuid bookmark-obj]
  (reset! BOOKMARK_CACHE (assoc @BOOKMARK_CACHE bookmark-uuid bookmark-obj)))

;; Find the placeholder in the file and return the line-number it is on
(defn find-placeholder-in-file [fn bookmark]
  (let [filestr (slurp fn)
        match-str bookmark
        pos (str/index-of filestr match-str)
        res-str (if pos (subs filestr 0 pos) "")
        num-line (count (str/split-lines res-str))]
    num-line))

(defn cache-bookmark [fpath]
  ;; return a fn to use in a (map ..)
  (fn [[bm-str bm-uuid]]
    (let [line-num (find-placeholder-in-file fpath bm-str)]
      (add-bookmark bm-uuid {:file fpath :line line-num}))
    ))


(defn find-all-bookmarks [fpath]
  (let [filestr (slurp fpath)
      match-list (re-seq #"#bookmark= (\w+-\w+-\w+-\w+-\w+)" filestr)]
    (doall (map (cache-bookmark fpath) match-list))))

;; https://rosettacode.org/wiki/Walk_a_directory/Recursively#Clojure
(defn walk-dir [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

(defn process-file [fpath]
  (let [bookmark-list (find-all-bookmarks fpath)]
    bookmark-list))

(defn parse-find-bookmarks [placestolook-list _ignore-list]
  (loop [dir-list placestolook-list]
    (let [dir1 (first dir-list)
          rest-list (next placestolook-list)]
      (doall (map #(process-file (.getPath %)) (walk-dir dir1 #".*\..*")))
      (when rest-list (recur rest-list)))))

(comment

(parse-find-bookmarks ["src/tools/local-code-server"] "")
(parse-find-bookmarks ["src"] "")

@BOOKMARK_CACHE
(save-bookmarks {:f1 :v1 :f2 [2 3 4 5 6 7]})
(save-bookmarks @BOOKMARK_CACHE)
(read-bookmarks)
(delete-BM)
(add-bookmark "sgsgsgs" {})
(add-bookmark "sgsgsgs2" {})
(add-bookmark "sgsgsgs3" {})

(def matcher (re-matcher #"#bookmark= ((\d+)-(\d+)-(\d+)-(\d+)-(\d+))" "#bookmark= 453bd6f6-f98b-48be-bb5d-94ef3ea5eafb"))
(def matcher (re-matcher #"((\d+)-(\d+)-(\d+)-(\d+))" "672-345-456-3212"))
(def matcher (re-matcher #"#bookmark= (\w+-\w+-\w+-\w+-\w+)" "#bookmark= 453bd6f6-f98b-48be-bb5d-94ef3ea5eafb #bookmark= 453bd6f6-f98b-48be-bb5d-94ef3ea5eafccc"))


(re-seq #"#bookmark= (\w+-\w+-\w+-\w+-\w+)" "#bookmark= 453bd6f6-f98b-48be-bb5d-94ef3ea5eafb #bookmark= 453bd6f6-f98b-48be-bb5d-94ef3ea5eafccc")

@BOOKMARK_CACHE
;;
)