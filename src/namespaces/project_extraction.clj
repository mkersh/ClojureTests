;;; Functions to create a mini project from a given namespace:
;;; I do most of my work in https://github.com/mkersh/ClojureTests, which is a big project with lots of stuff/experiemnts in.
;;; On occassions I want to share these experiments with collegues/clients but don't want to point them to the whole of ClojureTests
;;; Instead I want to create a separate mini project that I can host on my work GitHub account.
;;; 
(ns namespaces.project_extraction
  (:require  [clojure.string :as str]
             [clojure.java.io :as io]))

(defn file-exists? [fp]
  (.exists (io/file fp)))

(defn convert-to-filepath
  ([ns-id]
   (convert-to-filepath "src/" ns-id))
  ([root-dir ns-id]
   (str root-dir (str/replace ns-id #"\." "/") ".clj")))

(defn extract-requires-from-file [fp]
  (let [file-str (slurp fp)
        require-expr (re-find #"\(:require[^)]*" file-str)]
        (if require-expr
          (let [require-expr1 (subs require-expr 9) ;; remove the :require from the front
                req-list (re-seq #"\[.*\]" require-expr1)
                req-list2 (map
                           (fn [it]
                             (subs (get (str/split it #" ") 0) 1))
                           req-list)]
            req-list2)
          [])
  )
  )

(defn find-local-ns-deps [res-set ns-id]
  (let [fp (convert-to-filepath ns-id)
        file-exists (file-exists? fp)]

    (if file-exists
      (let [req-list (extract-requires-from-file fp)]
        (reduce find-local-ns-deps (conj res-set ns-id) req-list)
        )
      res-set)
    ))

;; next function returns a mapper functionm
(defn create-project-file [proj-root]
  (fn [namespace-id-str]
    (let [from-fp (convert-to-filepath namespace-id-str)
        to-fp (convert-to-filepath proj-root namespace-id-str)]
      (prn "create project file:")
      (prn "From:" from-fp)
      (prn "To:" to-fp)
      ))
  )

(defn create-new-project [proj-root ns-id]
  (let [ns-set (find-local-ns-deps #{} ns-id)]
    (doall (map (create-project-file proj-root) ns-set))))

(comment
  ;; [1] Function for creating a mini-project from a given namespace
  (create-new-project "/Users/mkersh/clojure/Shared/" "http.api.mambu.examples.edit_schedule")

  ;; Testing stuff whilst developing this library
  (find-local-ns-deps #{} "http.api.mambu.examples.edit_schedule")
  (re-find #":require[^)]*" "1112(:require [clojure.string :as str]\n[clojure.java.io :as io])")
  (re-seq #"\[.*\]" "[clojure.string :as str] \n [clojure.java.io :as io]")
;;
  )

(comment
  ;; = re-find - Example 1 = 

  user=> (def matcher (re-matcher #"\d+" "abc12345def"))
  #'user/matcher

  user=> (re-find matcher)
  "12345"

  ;; If you only want the first match, it is shorter to call re-find with the
  ;; pattern and the string to search, rather than explicitly creating a matcher
  ;; as above.
  user=> (re-find #"\d+" "abc12345def")
  "12345"

  ;; If you want all matches as a sequence, use re-seq.  Creating a matcher
  ;; explicitly with re-matcher and passing it to re-find is only the best way
  ;; if you want to write a loop that iterates through all matches, and do not
  ;; want to use re-seq for some reason.

  ;; See also:
  re-groups
  re-matcher
  re-pattern
  re-seq
  re-matches
  subs
  clojure.string/replace
  )

