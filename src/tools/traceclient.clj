
;;; A port of my original version in Python - https://github.com/mkersh/MambuAPINotebook/blob/master/traceclient.py
(ns tools.traceclient
  (:require [http.api.json_helper :as api]
            [http.api.api_pipe :as steps]
            [clojure.data.json :as json]))

;; This next bit of magic is required to sign our API requests
(defn sha1-str [s]
  (->> (-> "sha1"
           java.security.MessageDigest/getInstance
           (.digest (.getBytes s)))
       (map #(.substring
              (Integer/toString
               (+ (bit-and % 0xff) 0x100) 16) 1))
       (apply str)))


;; Make a signature to sign the MPO/corezoid API request
;; Signature: hex( sha1({GMT_UNIXTIME} + {API_SECRET} + {CONTENT} + {API_SECRET}) )
(defn make_sign [secret, timestamp, content]
  (let [s (str timestamp secret content secret)
        signature (sha1-str s)]
    signature))

(comment 

(make_sign "secret" "timestamp" "Hello World")

;; Clojure version returns - b7a9e140fdb27c00fd432e0d27023f6482a37a3f
;; Python vesion returns   - b7a9e140fdb27c00fd432e0d27023f6482a37a3f
;; 
;; So it doesn't look like the signing bit is the reason for the Auth 401 error

)

(defn mpo-sync-trace-api [i]
  {:method api/POST
   :query-params {}
   :body {
        "timeout" 30
        "ops" [{
            "company_id" "i962764498"
            "conv_id" 27845
            "type" "create"
            "obj" "task"
            "data" {
                "index"  i
            }
        }]
    }
   :headers {"Content-Type" "application/json"}})


(defn getTraceItem [config i]
  (let [unixtime (quot (System/currentTimeMillis) 1000)
        mpoSyncUrl (:mpo-syncapi-url config)
        apiLogon (:apiLogin config)
        secret (:apiKey config)
        apiCall (mpo-sync-trace-api i)
        ;; The signature needs to be against the actual body that will be sent over-the-wire
        ;; So we need to expand the EDN structure into JSON before generating the signature
        signature (make_sign secret unixtime (json/write-str (:body apiCall)))
        url (str mpoSyncUrl "/api/1/json/" apiLogon "/" unixtime  "/"  signature)
        apiCall2 (fn [_] (assoc apiCall :url url))
        api_response (steps/apply-api apiCall2 {} :trace_details)]
    (api/get-attr (:trace_details api_response) ["ops" 0 "data" "TRACE"])))

(defn get-i-from-option [option count]
(condp = option
  "c" [-2 0]
  "0" [-1 0]
  [count count]))

;;; Functions for storing and reading config data from a file
(def test-config
  {:mpo-syncapi-url "https://mpo-multitenant-syncapi.mambuonline.com"
  :apiLogin "220"
  :apiKey "SecretKey data goes here in file"})

(defn save-config [filePath configMap]
  (spit filePath (with-out-str (pr configMap))))

(defn read-config [filePath]
  (read-string (slurp filePath)))

(comment
  (save-config "TRACECLIENT.env" test-config)
  (read-config "TRACECLIENT.env")

  (def testRes {"ops" [{"proc" "ok", "data" {"TRACE" []}}], 0 nil, "data" nil, "TRACE" nil})
  (api/get-attr testRes ["ops" 0 "data" "TRACE"] )
   )

(defn print-trace-items [trace-list current-i-pos]
  (if (not= (first trace-list) nil)
    (do
      (doseq [it trace-list]
        (println it))
      (+ (if (> current-i-pos 0) current-i-pos 0) (count trace-list)))
    current-i-pos))

(defn main []
  ;; Get config details from the TRACECLIENT.env
  ;; NOTE: Make sure this file doesn't get stored in GitHub because it contains an ApiKey   
  (let [config (read-config "TRACECLIENT.env")]
    ;; Repeat the next loop until the user presses the q key
    (loop [count1 -1
           option "start"
           [i count2] (get-i-from-option option count1)]
      (let [traceItem  (getTraceItem config i)
            nextCount (print-trace-items traceItem count2)]
        (prn "0 - Reset, c - Clear, q - quit program, Other Key - Read Next ")
        (let [option (read-line)]
          (if (= option "c")
            (do
              (prn "Clear the Trace buffer")
              (getTraceItem config -2)
              (getTraceItem config -2))
            nil)
          (if (not= option "q")
            ;; Recurse into loop above again
            (recur (inc count1) option (get-i-from-option option nextCount))
            nil))))))

(comment
  (main)

 (map prn [1 2 3])

(map prn ["A1" "A2" "A3" "A4" "A1" "A2" "A3" "A4"])

  (get-i-from-option "start" 0)

(let [config (read-config "TRACECLIENT.env")]
  (getTraceItem config -1)
  )
)