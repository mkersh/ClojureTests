(ns http.tools.compojure.comp1
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jet]
            [ring.middleware.reload :as reload]
            ))

(defroutes app
  (GET "/" [] "<h1>Hello World!!!</h1>")
  (route/not-found "<h1>Page not found</h1>"))

;; https://github.com/ring-clojure/ring/issues/104
(def app-with-reload
  (reload/wrap-reload #'app))

(defonce server (jet/run-jetty #'app-with-reload {:join? false :port 3000}))

(comment
;; https://ring-clojure.github.io/ring/ring.adapter.jetty.html

;;(jet/run-jetty app {:join? false :port 3000})
(.start server)
(.stop server)
;;
)