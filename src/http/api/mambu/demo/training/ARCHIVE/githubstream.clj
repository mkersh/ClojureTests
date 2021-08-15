
(ns http.api.mambu.demo.training.githubstream
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))

(def gh-url "https://api.github.com/events")
(def gh-url2 "https://europeshowcase.sandbox.mambu.com/api/v1/subscriptions/5c75e51c-2b7d-49a6-b172-2e5437b48a41/events")
(def chan (async/chan 100))

(async/go
  (loop [r (async/<! chan)]
    (when (not-empty r) (println (:type r)))
    (recur (async/<! chan))))

(def headers {"Content-Type" "application/x-json-stream"
             "apikey" "gggg"}
)
(defn read-gh-stream [url]
  (with-open [stream (-> url (http/get {:as :stream :query-params {"batch_flush_timeout" "5"
                                                                   "batch_limit" 1
                                                                   "commit_timeout" 10}
                                        :headers headers}) :body)]
    (let [lines (-> stream io/reader (json/parse-stream true))]
      (doseq [l lines]
        (async/go
          (async/>! chan l))))))

(comment
  (read-gh-stream gh-url2)

;; Not working may have to give https://github.com/dakrone/clj-http a go

;; This looks promising
;; https://gist.github.com/oliyh/2b9b9107e7e7e12d4a60e79a19d056ee

  (+ 1 2)

;;
  )