;;; https://http-kit.github.io/server.html 

(ns http.web-services.http-kit-web-services
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [org.httpkit.server :as httpkit]
            [org.httpkit.timer :as timer]
            [compojure.route :refer [files not-found]]
            [compojure.core :refer [defroutes GET POST DELETE ANY context]]
            ))
  ;;(:use [compojure.route :only [files not-found]]
  ;;      [compojure.core :only [defroutes GET POST DELETE ANY context]]))


(defn app [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "hello XXXXXX HTTP!"})


(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server [app & args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and https://http-kit.github.io/migration.html#reload
  
  ;; This next hotload version is better if you only have 
  ;; a single app BUT if you want to experiment with stopping and starting lots 
  ;; of different app(s) on the webserver then don't use
  ;;(reset! server (httpkit/run-server #'app {:port 8080}))

  (reset! server (httpkit/run-server app {:port 8080}))
  
  
  )

(defn app2 [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (str "hello this is what you XXXX passed me: <br/>" req)})

(def json-data1
  {:users [
      {:name "Gary"
      :age 21}
      {:name "Charles"
       :age 19}
      {:name "Jane"
       :age 25}
      {:name "Maria"
       :age 40}
  ]})

;; First json web service
(defn app3 [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (api/to-json json-data1)})


(defn get-local-api [context]
  {:url (str "http://localhost:8080")
   :method api/GET
   :query-params {}
   :headers {"Content-Type" "application/json"}})

;; First Compojure example
(defn show-landing-page [_]
  "Landing Page")
(defn chat-handler [req]
  (let [user-id (-> req :params :id)]
    (str "chat-handler " user-id)))

(defn chat-handler2 [_]
  "chat-handler")
(defn async-handler [_]
  "async-handler")


(defn get-user-by-id [req]
  (let [user-id (-> req :params :id)]
  (str "get-user-by-id " user-id)))


(defn update-userinfo [_]
  "update-userinfo")

(defroutes all-routes
  (GET "/" [] show-landing-page)
  (GET "/ws/:id" [] chat-handler)     ;; websocket
  (GET "/async" [] async-handler) ;; asynchronous(long polling)
  ;;; I originally had a problem with the next context block 
  ;;; I have got it working now. The example I had copied from was using
  ;;; using (GET / [] get-user-by-id) - The / not being in a string was the problem
  (context "/user/:id" [id] 
    (GET "/" [] get-user-by-id) ;; get-user-by-id show 1 one of accessing the :id param
    (GET "/that" [] (fn [_] (str "hello world " id))) ;; This is the 2nd way of accessing the :id param
    (POST "/" [] update-userinfo))
  (files "/static/") ;; static file url prefix /static, in `public` folder
  (not-found "<p>Page not found.</p>")) ;; all other, return 404

(defn streaming-app [request]
  (httpkit/with-channel request channel
    (httpkit/on-close channel (fn [status] (println "channel closed, " status)))
    (loop [id 0]
      (when (< id 100)
        (timer/schedule-task (* id 200) ;; send a message every 200ms
                       (httpkit/send! channel (str "message from server #" id "<br/>") false)) ; false => don't close after send
        (recur (inc id))))
    (timer/schedule-task 50000 (httpkit/close channel)))) ;; close in 10s.

(comment

;; [1] Let's start with some very basic web server
;; That just responds with a fixed meessage regardless of the URI that is passed

;; Start the web server
;; goto http://localhost:8080/ to see the results
  (start-server app)
;; Stop the web server
  (stop-server)
  
;; [2] This next example display the ring object that was passed to the webserver
(start-server app2)
(stop-server)

;; [3] Return JSON data from app
(start-server app3)
(stop-server)
(steps/apply-api get-local-api {} )


;; [4] First compojure example
(start-server all-routes)
(stop-server)
(steps/apply-api get-local-api {})

;; [5] A streaming example
(start-server streaming-app)
(stop-server)
(steps/apply-api get-local-api {})
  
  ;;
  )




