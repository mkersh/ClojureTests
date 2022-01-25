;; Test creating a simple ring webserver using ring, jetty and compojure
;; Compojure doc: https://github.com/weavejester/compojure/wiki/Getting-Started 
(ns http.tools.compojure.comp1
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.adapter.jetty :as jet]
            [ring.middleware.reload :as reload]
            [ring.middleware.params :as wrap]
            [ring.middleware.resource :as res]
            [ring.util.response :as resp]
            
            ))

(defroutes app
  (GET "/" [] "<h1>Hello World!!</h1>")
  (GET "/public" [] (resp/resource-response "public/test.txt"))
  (GET "/about" request (str "<h1>Hello World!!!</h1>" request))
  (GET "/api/get-all-customers" [] "<h1>Return all the customers</h1>")
  (GET "/api/get-customer/:id" [id] (str "<h1>Getting Customer " id "</h1>"))
  ;; The following inline regexpressions don't work for me!! {[0-9]+}
  (GET "/api/get-customer2/:id" [id] (str "<h1>Getting Customer " id "</h1>"))
  (GET ["/api/get-customer3/:id", :id #"[0-9]+"] [id] (str "<h1>Getting Customer " id "</h1>"))  
  (route/not-found "<h1>Page not found</h1>"))

;; https://github.com/ring-clojure/ring/issues/104
(def app-with-reload
  ;; Using two middleware handlers here
  (res/wrap-resource (wrap/wrap-params (reload/wrap-reload #'app)) "public"))

(defonce server (jet/run-jetty #'app-with-reload {:join? false :port 3000}))

(comment
;; https://ring-clojure.github.io/ring/ring.adapter.jetty.html

;;(jet/run-jetty app {:join? false :port 3000})
(.start server) ;; http://localhost:3000
(.stop server)

(resp/resource-response "public/test.txt")
;;
)