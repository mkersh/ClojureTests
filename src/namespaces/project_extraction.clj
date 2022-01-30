;;; Functions to create a mini project from a given namespace:
;;; I do most of my work in https://github.com/mkersh/ClojureTests, which is a big project with lots of stuff/experiemnts in.
;;; On occassions I want to share these experiments with collegues/clients but don't want to point them to the whole of ClojureTests
;;; Instead I want to create a separate mini project that I can host on my work GitHub account.
;;; 
(ns namespaces.project_extraction
  (:require  [clojure.string :as str]
             [clojure.java.io :as io]
             [clojure.java.shell :as sh]
             ))

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

(defn delete-dir [dir-path]
  (try (delete-directory-recursive (clojure.java.io/file dir-path))
       (catch Exception _ "Nothing to DELETE")))

;; next function returns a mapper functionm
(defn create-project-file [proj-root local-root]
  (fn [namespace-id-str]
    (let [
        from-fp (convert-to-filepath (str local-root "src/") namespace-id-str)
        to-fp (convert-to-filepath (str proj-root "src/") namespace-id-str)
        cmd-str (str from-fp " " to-fp)
        ]
      (prn "create project file:")
      (prn "From:" from-fp)
      (prn "To:" to-fp)
      (prn "CMD:" cmd-str)
      (io/make-parents to-fp)
      ;; Creating hardlinks to the files. NOTE: Needs to be hardlinks and not soft for GitHub to work on new mini project directory
      ;; So in local file system the files in the mini project dir link to the originals
      (sh/sh "ln" from-fp to-fp)
      ))
  )

(defn create-new-project [proj-root local-root ns-id]
  (let [ns-set (find-local-ns-deps #{} ns-id)
        project-file "project.clj"
        git-ignore-file ".gitignore"
        repl-start-file "src/repl_start.clj"
        repl-start-file-template "src/namespaces/repl_start_template.clj"
        ENV-template-file "src/http/ENV-example.clj"
        ]
    (io/make-parents (str proj-root "dummy.txt"))

    (sh/sh "ln" (str local-root project-file) (str proj-root project-file))
    (sh/sh "ln" (str local-root git-ignore-file) (str proj-root git-ignore-file))
    ;; TBD - This should be a cp rather than a ln
    (sh/sh "ln" (str local-root repl-start-file-template) (str proj-root repl-start-file))
    (sh/sh "ln" (str local-root ENV-template-file) (str proj-root ENV-template-file))
    (doall (map (create-project-file proj-root local-root) ns-set))))

(comment
  ;; [1] Function for creating a mini-project from a given namespace
  (create-new-project "/Users/mkersh/clojure/Shared/example-220130/" "/Users/mkersh/clojure/ClojureTests/" "http.api.mambu.examples.edit_schedule")

  ;; Testing stuff whilst developing this library
  (delete-dir "/Users/mkersh/clojure/Shared/NewProj/")
  (find-local-ns-deps #{} "http.api.mambu.examples.edit_schedule")
  (re-find #":require[^)]*" "1112(:require [clojure.string :as str]\n[clojure.java.io :as io])")
  (re-seq #"\[.*\]" "[clojure.string :as str] \n [clojure.java.io :as io]")


;; 
(sh/sh "ls" "-aul")
(sh/sh "ln" "/Users/mkersh/clojure/ClojureTests/src/http/ENV.clj" "/Users/mkersh/clojure/Shared/NewProj/src/http/ENV.clj")

(io/make-parents "/Users/mkersh/clojure/Shared/NewProj/tt.txt")
(sh/sh "ln" "/Users/mkersh/clojure/ClojureTests/project.clj" "/Users/mkersh/clojure/Shared/NewProj/project.clj")



;;
  )
