;;; ****************************************
;;;
;;; GotoFile Tool/App
;;;
;;; Creates a local webserver that can display files in your VSCode editor
;;; Allows you to bookmark code URLs in your note applications (OneNote, Miro etc) and when you click 
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
;;;
;;; NOTE: The placeholder values are stored in placeholder-list 

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

(import java.util.UUID)

(import '[java.awt.datatransfer DataFlavor StringSelection Transferable])

(defn clipboard []
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))

(defn copy-to-clipboard [text]
  (let [selection (StringSelection. text)]
    (.setContents (clipboard) selection selection)
    text
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

(defn generate-placeholder []
  (let [uuid (UUID/randomUUID)]
    (copy-to-clipboard (str "#bookmark= " uuid))))

(defn generate-temp-bookmark1 []
  (let [url-str "http://localhost:3000/goto-file?file={{CLOJURE_TESTS}}/&bookmark=xxx"]
    (copy-to-clipboard url-str)))

(defn generate-temp-bookmark2 []
  (let [url-str "http://localhost:3000/goto-file?file={{CLOJURE_TESTS}}/&line=1"]
    (copy-to-clipboard url-str)))

;; Find the placeholder in the file and return the line-number it is on
(defn find-placeholder-in-file [filepath bookmark]
  (let [filestr (slurp filepath)
        match-str (str "#bookmark= " bookmark)
        pos (str/index-of filestr match-str)
        res-str (if pos (subs filestr 0 pos) "")
        num-line (count (str/split-lines res-str))]
    num-line))

(comment
(clipboard)
(UUID/randomUUID) 
;; #bookmark= bcc294fe-03f0-4812-a326-2d552148c8f1
(copy-to-clipboard "Copy this text to the clipboard")
(generate-placeholder)
(find-placeholder-in-file "/Users/mkersh/JupyterNotebooks/ClojureTests/src/tools/local-code-server/main.clj"
"bcc294fe-03f0-4812-a326-2d552148c8f1"
)

(expand-placeholder "{{CLOJURE_TESTS}}/src/tools/traceclient.clj")

(str/index-of "bbba" "a")

(re-seq #"\{\{[^\}]*\}\}" "shshs {{shsh}}  shssh {{aaa}}")

(str/replace "The color is red" "red" "blue")

;;#bookmark=placeholderval
;;
)

(defn goto-file [query-params]
  (let [file (expand-placeholder (get query-params "file"))
        line (or (get query-params "line") 1)
        bookmark (get query-params "bookmark")
        line2 (if bookmark (find-placeholder-in-file file bookmark) line)
        ]
    (sh/sh "code" "-g" (str file ":" line2))
    ;; Close the browser window 
    (str "<script>window.close();</script>")
    ))

(defroutes app
  (GET "/" [] (resp/resource-response "public/goto-file.html"))
  (GET "/gen-placeholder" [] (do (generate-placeholder) "<a href='/'>BACK</a>"))
  (GET "/gen-temp-bookmark1" [] (do (generate-temp-bookmark1) "<a href='/'>BACK</a>"))
  (GET "/gen-temp-bookmark2" [] (do (generate-temp-bookmark1) "<a href='/'>BACK</a>"))
  (GET "/about" request (str "<h1>Hello World!!!</h1>" request))
  (GET "/goto-file" {query-params :query-params} (goto-file query-params))
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