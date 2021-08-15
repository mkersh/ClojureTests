(ns http.api.mambu.demo.training.sse-client)

(require '[clj-http.client :as http])
(require '[clojure.core.async :as a])
(require '[clojure.string :as string])
(require '[clojure.java.io :as io])
(import '[java.io InputStream])

(def event-mask (re-pattern (str "(?s).+?\r\n\r\n")))

(defn- parse-event [raw-event]
  (->> (re-seq #"(.*): (.*)\n?" raw-event)
       (map #(drop 1 %))
       (group-by first)
       (reduce (fn [acc [k v]]
                 (assoc acc (keyword k) (string/join (map second v)))) {})))

(defn connect [url & [params]]
  (let [_ (prn "Before InputStream")
        event-stream ^InputStream (:body (http/get url (merge params {:as :stream})))
        _ (prn "After InputStream")
        events (a/chan (a/sliding-buffer 10) (map parse-event))]
    (a/thread
      (loop [data nil]
        (let [byte-array (byte-array (max 1 (.available event-stream)))
              _ (prn "Before .read")
              bytes-read (.read event-stream byte-array)
              _ (prn "After .read")
              ]

          (if (neg? bytes-read)

            (do (println "Input stream closed, exiting read-loop")
                (.close event-stream))

            (let [data (str data (slurp byte-array))]

              (if-let [es (not-empty (re-seq event-mask data))]
                (if (every? true? (map #(a/>!! events %) es))
                  (recur (string/replace data event-mask ""))
                  (do (println "Output stream closed, exiting read-loop")
                      (.close event-stream)))

                (recur data)))))))
    events))

(comment
(def gh-url "https://api.github.com/events")
(def gh-url2 "https://europeshowcase.sandbox.mambu.com/api/v1/subscriptions/5c75e51c-2b7d-49a6-b172-2e5437b48a41/events?batch_flush_timeout=5&batch_limit=1&commit_timeout=60")
(def headers {"Content-Type" "application/x-json-stream"
              "apikey" "gggg"})
(connect gh-url2 {:headers headers})

(* 2 2)

(let [url (new java.net.URL gh-url2)
      urlConn (.openConnection url)
      _ (.setRequestProperty urlConn "apikey" "iKfqhNlhBl7BZysHTPXWbqJvYs62tC05")
      streamID (.getHeaderField urlConn "X-Mambu-StreamId")
      inStream (.getInputStream urlConn)
      inStreamReader (new java.io.InputStreamReader inStream)
      bufferReader (new java.io.BufferedReader inStreamReader)
      ]
  (.readLine bufferReader))

;;
)