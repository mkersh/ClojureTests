;;; Experiments using core.async
;;; Clojure's implementation of Tony Hoares CSP (Communicating Sequential Processes) - https://en.wikipedia.org/wiki/Communicating_sequential_processes
;;; CSP original book: http://www.usingcsp.com/cspbook.pdf
;;; Going through https://github.com/clojure/core.async/blob/master/examples/walkthrough.clj
(ns concurrency.acore-sync
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))


(comment 

;;;; CHANNELS
  
;; Data is transmitted on queue-like channels. By default channels
;; are unbuffered (0-length) - they require producer and consumer to
;; rendezvous for the transfer of a value through the channel.
  
;; Use `chan` to make an unbuffered channel:
  (def my-channel1 (chan))

;; Pass a number to create a channel with a fixed buffer:
  (def my-buf-channel1 (chan 10))

;; If you run next statement in will block and you will have to restart the REPL!!!
  (>!! my-channel1 {:count 1}) ;; This write to an unbuffered chan will block main thread

;; We should however be able to write to the buffered chan
;; Well up to 10 items on it - before it blocks
(for [x (range 10)]
  (do (prn "Write x to channel")
      (>!! my-buf-channel1 {:id x})))

  (<!! my-buf-channel1)

;; Example of writing to a channel in a separate thread
  (let [c (chan)]
    (thread (>!! c "hello")) ;; Could use future here
    (prn "Got following from channel: " (<!! c))
    (close! c))



;;;; GO BLOCKS AND IOC THREADS

;; The `go` macro asynchronously executes its body in a special pool
;; of threads. Channel operations that would block will pause
;; execution instead, blocking no threads. This mechanism encapsulates
;; the inversion of control that is external in event/callback
;; systems. Inside `go` blocks, we use `>!` (put) and `<!` (take).

;; Here we convert our prior channel example to use go blocks:
(let [c (chan)]
  (go (>! c "hello"))
  (prn "Got following from channel: "(<!! (go (<! c))))
  (close! c))

;; Looking to see if I can simplify the (<!! (go (<! c)))
;; BUT it does look like the go block returns its own channel
  (let [c (chan)]
    (go (>! c "hello"))
    (prn "Got following from channel: " (go (<! c)))
    (close! c))


;; This works and I now understand the problem
;; Without waiting for the read go block to terminate the channel is closed before the read happens
(let [c (chan)]
  (go (>! c "hello"))
  (prn (<!! (go (prn "Result:" (<! c)) "return from go")))
  (close! c))

;; If we avoid closing the channel then all is good!!
(let [c (chan)]
  (go (>! c "hello"))
  (go (prn "Result:" (<! c))))



(let [c1 (chan)
      c2 (chan)]
  (go (while true
        (let [[v ch] (alts! [c1 c2])]
          (println "Read" v "from" ch))))
  (go (>! c1 "hi"))
  (go (>! c2 "there")))



  )


;;; How to park Go blocks
;;; https://stackoverflow.com/questions/46849064/clojure-async-go-how-to-park-blocking-code
;;; https://clojure.org/guides/core_async_go
;;; https://clojureverse.org/t/to-block-or-not-to-block-in-go-blocks/2104/10