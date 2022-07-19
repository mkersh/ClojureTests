;; Mambu Streaming APIs from Cloure - Examples
;; GitHub: https://github.com/mkersh/ClojureTests/tree/master/src/http/api/mambu/demo/training/stream_api.clj
;; See Also the more official Mambu Java streaming client:
;;     https://github.com/mambu-gmbh/Streaming-API-Java-Sample-Client/blob/master/src/main/java/streamingapi/client/StreamingApiClient.java
;;
;; #bookmark= 8564e243-eb88-4cdc-990b-f611de7e79f4
;;
(ns http.api.mambu.demo.training.stream_api
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [http.ENV :as env]
            [clojure.data.json :as json]
            [clojure.pprint :as pp]
            [clojure.java.io]
            ))

;;; -----------------------------------------------------------------
;;; Atoms to allow for behaviours to be controlled
;;;

;; Defines the output file that process-event will write results to 
(defonce output-file (atom "STREAM-VIEWER"))
(defn setup-logfile [fpath]
  (reset! output-file fpath))

;; stop-atom - controls when to stop the main thread/loop in consume-sse-stream
(defonce stop-atom (atom false))
(defn stop-client []
  (reset! stop-atom true))
(defn start-client []
  (reset! stop-atom false))

;; commit-atom - controls whether or not to commit the events being received 
;; NOTE: If you don't commit then the event will be resent next time you consume
;; If you don't commit an event-cursor in 60 secs then Mambu will close the connection - assuming something is wrong
(defonce commit-atom (atom true))
(defn stop-commit []
  (reset! commit-atom false))
(defn start-commit []
  (reset! commit-atom true))

;;; -----------------------------------------------------------------
;;; The 3 main streaming API admin endpoints
;;; NOTE: Consuming the stream is the other endpoint but is not a standard API - See consume-sse-stream below
(defn create-subscription-api [context]
  {:url "{{*env*}}/v1/subscriptions"
   :method api/POST
   :query-params {"size" "10"}
   :body   {"owning_application" "demo-app"
            "event_types" (:topic-list context)}
   :headers {"Content-Type" "application/json"}})

(defn delete-subscription-api [context]
  {:url (str "{{*env*}}/v1/subscriptions/" (:subscription_id context))
   :method api/DELETE
   :headers {"Content-Type" "application/json"}})

(defn commit-cursor-api [context]
  {:url (str "{{*env*}}/v1/subscriptions/" (:subscription_id context) "/cursors")
   :method api/POST
   :body   {"items" (:cursor-list context)}
   :headers {"Content-Type" "application/json"
             "X-Mambu-StreamId" (:x-mambu-streamid context)}})

(declare subscriptionid)
(defn commit-cursor [data streamID]
  (prn "In commit-cursor:")
  (let [cursor (get data "cursor")]
    (if cursor
      ;;(pp/pprint cursor)
      (steps/apply-api commit-cursor-api
                       {:subscription_id subscriptionid
                        :cursor-list [cursor]
                        :x-mambu-streamid  streamID})
      nil)))

;;; -----------------------------------------------------------------
;;; Helper functions

;; Function to extract the ApiKey from the ENV.clj file
;; NOTE: From a security perspective it is important that this ENV.clj file is not stored in github
(defn get-auth2
  ([envId]
   (:ApiKey (get env/ENV-MAP envId))))

(defn get-json [buffer]
  (if buffer (json/read-str buffer)
      nil))

(defn append-to-file
  "Uses spit to append to a file specified with its name as a string, or
   anything else that writer can take as an argument.  s is the string to
   append."
  [file-name s]
  (spit file-name s :append true))

;;; -----------------------------------------------------------------
;;; Functions for processing the events on the stream

(defn process-event [data]
  ;;(prn "In process event:" data)
  (let [body (get data "body")]
    (if body
      (do (pp/pprint body)
          (if @output-file (append-to-file @output-file (str body "\n")) nil))
      nil)))

(defn process-events [data streamID]
  (let [events (get data "events")]
    (if events
      (do
        (doall (map process-event events)) ;; need for doall always catches me out!
        (when @commit-atom (commit-cursor data streamID))) 
      (prn ".") ;; This is a useless cursor, no new events in it. Why are these being sent? Are they reminders that you need to commit??
      ))
  )

;;; -----------------------------------------------------------------
;;; consume-sse-stream:
;;; This is the main function for consuming events from a SSE-style Mambu stream
;;;

(defn consume-sse-stream [url apikey]
  (start-client) ;; make sure @stop-atom = false when we start

  ;; The bindings section of this let connects to the events stream URL
  ;; and opens up a bufferReader file socket that you can then listen on for events.
  ;; NOTE: It took me a long time to get this working. I ended up copying the connection code from: 
  ;; https://github.com/mambu-gmbh/Streaming-API-Java-Sample-Client/blob/master/src/main/java/streamingapi/client/StreamingApiClient.java
  (let [url (new java.net.URL url)
        urlConn (.openConnection url)
        _ (.setRequestProperty urlConn "apikey" apikey)
        streamID (.getHeaderField urlConn "X-Mambu-StreamId") ;; This forces an implicit connect to the URL
        inStream (.getInputStream urlConn)
        inStreamReader (new java.io.InputStreamReader inStream)
        bufferReader (new java.io.BufferedReader inStreamReader)]

    (prn "X-Mambu-StreamId: " streamID) ;; need to use this when comitting cursors
    ;; Run the next loop in a separate thread - to prevent blocking REPL
    ;; The loop will run repeatedly until the connection is closed (from server side) or @stop-atom is set to true
    (future 
      (loop []
        (if (not @stop-atom) ;; call (stop-client) to stop this loop and terminate the thread
          (let [data (get-json (.readLine bufferReader))]
            (process-events data streamID)
            (if data (recur) (prn "END of Streaming")))
          (prn "Stopping stream consumption"))))))

;;; -----------------------------------------------------------------
;;;  Next functions allow you to create some activity on a stream
;;;

(defn patch-customer [apikey id middleName]
  (let [options {:headers {"Accept" "application/vnd.mambu.v2+json"
                           "Content-Type" "application/json"
                           "apikey" apikey}
                 :query-params {}
                 :body [{"op" "ADD"
                         "path" "middleName"
                         "value" middleName}]}
        url (str "https://europeshowcase.sandbox.mambu.com/api/clients/" id)]
    (api/PATCH url options)))

(defn modify-customer [apikey id stem startNum endNum]
  (doall ;; remember to force your way through the LazySeq that the for returns
   (for [i (range startNum endNum)]
     (do
       (prn "Change name to: " (str stem i))
       (patch-customer apikey id (str stem i))))))

;;; -----------------------------------------------------------------
;;;  Script for testing the Mambu Streaming API
;;;  Plan: Go through each step below and excecute the function in your REPL to test the streaming APIs
;;;  NOTE: My test is based around a client-update Topic on our europeshowcase.sandbox.mambu.com tenant
;;;        You will need to modify slightly to run your own tests

(api/setenv "env8") ;; setup API calls to use europeshowcase.sandbox.mambu.com with streaming API consumer
(def subscriptionid "7bd2428c-1084-48cc-8a69-01daf407b079") ;; See create-subscription-api call below - to get value for this
(def url (str "https://europeshowcase.sandbox.mambu.com/api/v1/subscriptions/" subscriptionid "/events?batch_flush_timeout=20&batch_limit=1&commit_timeout=60"))
(def apiKey (get-auth2 "env8")) ;; Get streaming apikey for europeshowcase.sandbox.mambu.com
(def apiKey2 (get-auth2 "env6")) ;; Get apikey for europeshowcase.sandbox.mambu.com with core API consumer

(comment
  ;; #bookmark= 1989a54f-774f-46f0-a1da-5c50645c7394
  ;;
  ;; [0] - PreRequisities - Needed before you can start to stream events
  ;; [0.1] Setup a StreamingAPI Topic in the Mambu UI
  ;; This is very similar to setting up a webhook
  ;; For my test on europeshowcase.sandbox.mambu.com the Topic is mrn.event.europeshowcase.streamingapi.client_activity
  ;; [0.2]
  ;; You also need to setup a StreamingAPI Consumer/ApiKey to access the StreamingAPI endpoints

  ;; [1] - Get a SubscriptionID for 1 or more StreamingAPI topics (from step #0 above)
  ;; This next function will create a new subscription for mrn.event.europeshowcase.streamingapi.client_activity
  ;; If one already exists it will return the same
  ;; NOTE: For testing run and then copy into subscriptionid def above (if it differs)
  (steps/apply-api create-subscription-api {:topic-list ["mrn.event.europeshowcase.streamingapi.client_activity"]})
  ;; Next function will delete the subscription for mrn.event.europeshowcase.streamingapi.client_activity
  (steps/apply-api delete-subscription-api {:subscription_id subscriptionid})

  ;; [2] Config settings - for consume-sse-stream
  ;; Calling these is optional - All have sensible defaults
  ;; NOTE: You can execute these at any time to change behaviour - even after consume-sse-stream is started
  (stop-commit) ;; excecute this to stop commit(s) from happening
  (start-commit) ;; excecute this to start commit(s)
  (setup-logfile "STREAM-VIEWER") ;; write output to file 
  (setup-logfile nil) ;; cancel file output

  ;; [3] - Call next function to start the main consumer loop
  ;; See https://api.mambu.com/CNNmZrTm3GDuWN/index.html#events for params that can be passed
  ;;    batch_limit=n : Controls how many events to batch together under a single cursor
  ;;    stream_keep_alive_limit=n : How long to keep the stream alive with empty keep-alives.
  ;;       
  (consume-sse-stream url apiKey)

  ;; [4] Call next function to stop consume-sse-stream
  ;; NOTE: May take a little time. When complete you will see output "Stopping stream consumption" 
  (stop-client)

  ;; [5] Make some changes to a customer - To generate streaming events
  (modify-customer apiKey2 "726497757" "Tester" 1 30) ;; Change middleName to Tester<n> - apply a number of times based on last 2 params

;; 
  )