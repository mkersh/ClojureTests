;;; Functions to create a mini project from a given namespace:
;;; I do most of my work in https://github.com/mkersh/ClojureTests, which is a big project with lots of stuff/experiemnts in.
;;; On occassions I want to share these experiments with collegues/clients but don't want to point them to ClojureTests
;;; Instead I want to create a separate mini project that I can host on my work GitHub account
;;;
(ns namespaces.project_extraction
  (:require  [clojure.string :as str]
             [clojure.java.io :as io]))

(defn file-exists? [fp]
  (.exists (io/file fp)))

(defn convert-to-filepath [ns-id]
  (str "src/" (str/replace ns-id #"\." "/") ".clj"))

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

(comment

  (find-local-ns-deps #{} "http.api.mambu.examples.edit_schedule")

(re-find #":require[^)]*" "1112(:require [clojure.string :as str]\n[clojure.java.io :as io])")

(re-seq #"\[.*\]" "[clojure.string :as str] \n [clojure.java.io :as io]")
;;
  )