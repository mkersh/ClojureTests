;;; https://github.com/dakrone/clj-http
;;; 
;;; Started to look at this library for making JSON API calls
;;; BUT I abandoned and moved to http-kit because the status handling
;;; in clj-http appeared to be too complicated 
;;; i.e. I just could not easily get the http response code
;;; 
(ns http.tests.clj-http-test
  (:require
   [clj-http.client :as client]))

(defn reportERROR [e]
   (println "here1")
   ;;(let [status e]
    (let [status (Throwable->map e)]
     (println "ERROR - HTTP Status: " status)
   )
  (println "here2")
  ) 

(defn GET [url] 
(try
  ;;(client/get url {:accept :json :throw-entire-message? true})
  (client/get url {:throw-entire-message? true})
  (catch Exception e (reportERROR e))
))

(GET "http://dummy.restapiexample.com/api/v1/employeesxxx")