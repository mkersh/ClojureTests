;;; ****************************************
;;;
;;; GotoFile Tool/App
;;;
;;; Creates a local webserver that can display files in your VSCode editor
;;; Allows you to place code-URLs in your note applications (OneNote, Miro etc) and when you click 
;;; on them they display the code/file in VSCode
;;;
;;; Starting the Webserver: See [1] below
;;;
;;; Example URLS:
;;; * http://localhost:3000/goto-file2?bookmark=<bookmark>
;;;       - Searches for the file containing <bookmark> and jumps to that position
;;; * http://localhost:3000/goto-file2?file=<filepath>[&line=<linenum>]
;;; * http://localhost:3000/goto-file2?file=<filepath>&bookmark=<bookmark>
;;;       - Searches for #bookmark= <bookmark> in file and jumps to this place
;;;
;;; Supports a number of placeholders that can be added to the <filepath> to make them relative
;;;     {{CLOJURE_TESTS}} - E.g  http://localhost:3000/goto-file2?file={{CLOJURE_TESTS}}/<filepath>&line=<linenum>
;;;
;;; NOTE: The placeholder values are stored in placeholder-list 

(ns tools.local_code_server.main
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jet]
            [ring.middleware.reload :as reload]
            [ring.middleware.params :as wrap]
            [ring.middleware.resource :as res]
            [ring.util.response :as resp]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [tools.local_code_server.bookmark_db :as bm]
            ))

(import java.util.UUID)
(import '[java.awt.datatransfer DataFlavor StringSelection Transferable])
(defonce LAST-PLACEHOLDER (atom ""))
;; declare(s) to suppress clj-kondo unresolved-symbol
;; NOTE: Below stopped working (noticed 30 May 2022), so have had to disable
;;(declare defroutes GET app request query-params)

(defn clipboard []
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))

(defn copy-to-clipboard [text]
  (let [selection (StringSelection. text)]
    (.setContents (clipboard) selection selection)
    text
    ))

(defn paste-from-clipboard []
  (try
    (.getTransferData (.getContents (clipboard) nil) (DataFlavor/stringFlavor))
    (catch java.lang.NullPointerException _e nil)))


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
    (reset! LAST-PLACEHOLDER uuid)
    (copy-to-clipboard (str "#bookmark= " uuid))))

(defn generate-temp-bookmark1 []
  (let [url-str (str "http://localhost:3000/goto-file?file={{CLOJURE_TESTS}}/" (paste-from-clipboard) "&bookmark=" @LAST-PLACEHOLDER)]
    (copy-to-clipboard url-str)))

(defn generate-temp-bookmark2 []
  (let [url-str (str "http://localhost:3000/goto-file?file={{CLOJURE_TESTS}}/" (paste-from-clipboard) "&line=1")]
    (copy-to-clipboard url-str)))

;; Pretty confident that this will be thee onee I use most
;; Will search for the bookmark across a range of files
;; NOTE: Uses an in-memory cache to make it performant
(defn generate-temp-bookmark3 []
  (let [url-str (str "http://localhost:3000/goto-file?&bookmark=" @LAST-PLACEHOLDER)]
    (copy-to-clipboard url-str)))

;; Find the placeholder in the file and return the line-number it is on
(defn find-placeholder-in-file [filepath bookmark]
  (let [filestr (slurp filepath)
        match-str (str "#bookmark= " bookmark)
        pos (str/index-of filestr match-str)
        res-str (if pos (subs filestr 0 pos) "")
        num-line (count (str/split-lines res-str))]
    num-line))

;; ***********************************
;; Apply a VSCode code command to open up a file in your VSCode App
;; Which file to open and where to jumpto is passed in the query-params:
;;
;;     * file      - <filepath> to jump to, can contain {{<placeholder>}}
;;     * line      - <line> to jumpto, if missing default=1
;;     * bookmark  - <bookmark> to jumpto. Searches for #bookmark= <bookmark> and jumpto this line
;;
;; A number of special {{<placeholder>}}'s are supported:
;;
;;     * {{CLOJURE_TESTS}} - Expanded to the value in placeholder-list
;;

(defn goto-file [query-params]
  (let [file0 (get query-params "file")
        file (when file0 (expand-placeholder (get query-params "file")))
        line (or (get query-params "line") 1)
        bookmark (get query-params "bookmark")
        line2 (if (and file bookmark)
                (if bookmark
                  (find-placeholder-in-file file bookmark)
                  line)
                line)
        bm_obj (when (not file) (bm/get-bookmark-obj bookmark))
        file2 (or (:file bm_obj) file)
        line3 (or (:line bm_obj) line2)]
    (sh/sh "code" "-g" (str file2 ":" line3))
    ;; Close the browser window 
    (str "<script>window.close();</script>")))

;; *********************************
;; Define the possible routes of our webserver
;; The home page http://localhost:3000/ provides a menu of options for:
;;     * Generating a Bookmark
;;     * Creating a gotofile-url, from previous bookmark and clipboard-paste-buffer for the file
;;
;; The webserver also supports /goto-file route that is the one that can open VSCode files and jump
;; to a specific line position of search+jump to a bookmark
;;
(defroutes app
  (GET "/" [] (resp/resource-response "public/goto-file.html")) ; #search= dfbab3ef-0544-4037-bc6b-ebafe0186efc
  (GET "/gen-placeholder" [] (do (generate-placeholder) "<a href='/'>BACK</a>"))
  (GET "/gen-temp-bookmark1" [] (do (generate-temp-bookmark1) "<a href='/'>BACK</a>"))
  (GET "/gen-temp-bookmark2" [] (do (generate-temp-bookmark2) "<a href='/'>BACK</a>"))
  (GET "/gen-temp-bookmark3" [] (do (generate-temp-bookmark3) "<a href='/'>BACK</a>"))
  (GET "/about" request (str "<h1>Hello World!!!</h1>" request))
  (GET "/goto-file" {query-params :query-params} (goto-file query-params))
  (route/not-found "<h1>Page not found</h1>"))

;; https://github.com/ring-clojure/ring/issues/104
(def app-with-reload
  ;; Using two middleware handlers here
  (res/wrap-resource (wrap/wrap-params (reload/wrap-reload #'app)) "public"))

(defonce server (jet/run-jetty #'app-with-reload {:join? false :port 3000}))

(comment
  ;; *********************************************
  ;; [1] Start/Stop the webserver

  (.start server) ;; http://localhost:3000
  (.stop server)


;;
  )