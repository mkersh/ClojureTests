;;; ****************************************
;;;
;;; GotoFile Tool/App
;;;
;;; Creates a local webserver that can display files in your VSCode editor
;;; Allows you to bookmark code URLs in your notes applications (OneNote, Miro etc) and when you click 
;;; on them they display the code/file in VSCode
;;;
;;; Starting the Webserver: See [1] below
;;;
;;; Example URLS:
;;; http://localhost:3000/goto-file2?file=<filepath>&line=<linenum>
;;; NOTE: Passing line param is optional. If line not passed then line=1 will be assumed
;;;
;;; Supports a number of placeholders that can be added to the <filepath> to make them relative
;;;     {{CLOJURE_TESTS}} - E.g  http://localhost:3000/goto-file2?file={{CLOJURE_TESTS}}/<filepath>&line=<linenum>

(ns tools.local-code-server.main
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jet]
            [ring.middleware.reload :as reload]
            [ring.middleware.params :as wrap]
            [ring.middleware.resource :as res]
            [ring.util.response :as resp]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            ))

(def placeholder-list
  {"{{CLOJURE_TESTS}}" "/Users/mkersh/clojure/ClojureTests"})

(defn expand-placeholder [file]
  (let [placeholders (re-seq #"\{\{[^\}]*\}\}" file)]
    (reduce (fn [res placeholder]
              (let [placeholder-value (get placeholder-list placeholder)]
                (if placeholder-value
                  (str/replace res placeholder placeholder-value)
                  res)))
            file placeholders)))

(comment
(expand-placeholder "{{CLOJURE_TESTS}}/src/tools/traceclient.clj")


(re-seq #"\{\{[^\}]*\}\}" "shshs {{shsh}}  shssh {{aaa}}")

(str/replace "The color is red" "red" "blue")

;;
)

(defn goto-file [query-params]
  (let [file (expand-placeholder (get query-params "file"))
        line (or (get query-params "line") 1)]
    (sh/sh "code" "-g" (str file ":" line))
    ;; Close the browser window 
    (str "<script>window.close();</script>")
    ))

(defroutes app
  (GET "/" [] "Simple web-server for jumping to a file in VSCode</br> Use /goto-file/:file/:line")
  (GET "/about" request (str "<h1>Hello World!!!</h1>" request))
  (GET "/goto-file/:file/:line" [file line] (str "Goto File: " file " Line:" line))
  (GET "/goto-file2" {query-params :query-params} (goto-file query-params))
  (route/not-found "<h1>Page not found</h1>"))

;; https://github.com/ring-clojure/ring/issues/104
(def app-with-reload
  ;; Using two middleware handlers here
  (res/wrap-resource (wrap/wrap-params (reload/wrap-reload #'app)) "public"))

(defonce server (jet/run-jetty #'app-with-reload {:join? false :port 3000}))

(comment
;; https://ring-clojure.github.io/ring/ring.adapter.jetty.html
;;(jet/run-jetty app {:join? false :port 3000})

;; [1] Start/Stop the webserver
  (.start server) ;; http://localhost:3000
  (.stop server)

;;
  )