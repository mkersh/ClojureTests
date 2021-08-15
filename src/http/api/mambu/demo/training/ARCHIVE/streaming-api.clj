;;; Early attempt to get Mambu streaming APIs working
;;; See https://github.com/mkersh/JupyterNotebooks/blob/master/ClojureTests/src/http/api/mambu/demo/training/stream-api.clj


(ns http.api.mambu.demo.training.streaming-api
  (:require [http.api.json_helper :as api]
            [org.httpkit.client :as client]
            [http.api.api_pipe :as steps]))

(defn get-cust-api [context]
  {:url (str "{{*env*}}/clients/" (:custid context))
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :as :stream
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn get-allcusts-api [context]
  {:url (str "{{*env*}}/clients")
   :method api/GET
   :query-params {"detailsLevel" "FULL"}
   :headers {"Accept" "application/vnd.mambu.v2+json"
             "Content-Type" "application/json"}})

(defn create-subscription-api [context]
  {:url "{{*env*}}/v1/subscriptions" 
   :method api/POST
   :query-params {"size" "10"}
   :body   {"owning_application" "demo-app"
            "event_types" ["mrn.event.europeshowcase.streamingapi.client_activity"]}
   :headers {"Content-Type" "application/json"}})



(defn delete-subscription-api [context]
  {:url (str "{{*env*}}/v1/subscriptions/" (:subscription_id context))
   :method api/DELETE
   :headers {"Content-Type" "application/json"}})

(defn consume-events-api [context]
  {:url (str "{{*env*}}/v1/subscriptions/" (:subscription_id context) "/events")
   :query-params {"batch_flush_timeout" 1
                  "batch_limit" 1
                  "commit_timeout" 1}
   :as :stream
   :method api/GET
   :headers {"Content-Type" "application/x-json-stream"
   }})

(defn test-sub2 [context]
  (client/get (str "{{*env*}}/v1/subscriptions/" (:subscription_id context) "/events")
              {:as :stream
               :query-params {"batch_flush_timeout" "1"
                              "batch_limit" 1
                              "commit_timeout" 1}}
              (fn [{:keys [status headers body error opts]}]
          ;; body is a java.io.InputStream
                (prn "You are here!!!"))))

(comment
  (api/setenv "env8") ;; Make sure you use an APIKey for streaming
  (api/setenv "env9")
  (api/setenv "env6")

  (def subscription_id "5c75e51c-2b7d-49a6-b172-2e5437b48a41")
  (steps/apply-api create-subscription-api {})
  (steps/apply-api delete-subscription-api {:subscription_id subscription_id})


  ;; Possible references
  ;; https://stackoverflow.com/questions/40215516/streaming-connection-long-polling-with-http-kit
  (steps/apply-api consume-events-api {:subscription_id subscription_id})

  (let [in-stream (:last-call (steps/apply-api consume-events-api {:subscription_id subscription_id}))]
    (slurp in-stream))



  (test-sub2 {:subscription_id subscription_id})

  (api/setenv "env6")
  (steps/apply-api get-allcusts-api {})
  (steps/apply-api get-cust-api {:custid "335356139"})
  (let [in-stream (:last-call (steps/apply-api get-cust-api {:custid "335356139"}))]
    (slurp in-stream))

;;
  )