;;; Experiments with the basic clojure concurrency mechanisms
;;; [A] Futures
;;; [B] Delays
;;; [C] Promises
;;; [D] Atoms
;;; [E] Refs
;;; [F] Dynamic Variables
;;; [G] Locking
(ns concurrency.con-basics)

;;; [A] ********** futures (aka threads)

(comment

;; [A.1] Simple test to create a new thread using (future ...)
(do
  (future (Thread/sleep 4000)
          (prn "Printing from within a future"))
  (prn "Main thread Finished")
  )


;; Force threads create under main thread to be terminated.
;; Careful using in your REPL though cos likely to break existing session
(shutdown-agents) 


;; [A.2] Get future reference and deref in main thread
(do
  (def f (future (Thread/sleep 4000)
                 (prn "Printing from within a future")
                 "result of the future"))

  (prn @f) ;; Main thread will wait here until the future has completed
  (prn "Main thread Finished")
  (prn @f) ;; subsequent derefs will return previous result
  (prn (deref f)))
)

;;; [B] ********** delays

(comment

;; [B.1] Create a delay that will only be executed when forced or deref'ed
(do
  (def d (delay (prn "Printing from within a delay")
         "delay result is 777"))
  (prn "Main thread Finished")
  (prn @d) ;; This will force the delay to be executed
  (prn @d) ;; Subsequent deref's will just return the previous results
  )

)


;;; [C] ********** promises

(comment

;; [C.1] Create a delay that will only be executed when forced or deref'ed
  
  (def my-promise (promise))
  (def my-promise2 (promise))

  (deliver my-promise (+ 2 2))

  @my-promise ;; Deref to get the value of a promise. Will wait if not yet computed
  
  ;; Use timeout version of deref to limit wait time
  (deref my-promise2 2000 "Deref timeout!!")
  )

;;; [D] ********** Atoms
;;; In clojure atoms are a key basic building block for managing state. 
;;; It allows you to name things whose state (i.e. value) will change over time

(comment 

;; Create an atom with an initial state value
(def my-atom (atom {:count 1}))

;; deref an atom to get its current state value
@my-atom

;; Change the value of the atom's state using swap!
;; NOTE: This does it in a controlled way that is safe if multiple threads are doing this at the same time
;; swap! guarentees that the update will only occur if the currrent-state did not change whilst the swap! function
;; was executing. If another thread was also performing a swap and completed first then the update would be retried
;; i.e. the swap function would be called again
(swap! my-atom
       (fn [current-state]
         (merge-with + current-state {:count 1 :appreciation-level 1})))

)

;; Clojure's prn is not thread-safe and if multiple threads are calling at the same time
;; results will be interleaved.
;; To avoid this lock the *out* stream that print uses
;; See also: https://stackoverflow.com/questions/18662301/make-clojures-println-thread-safe-in-the-same-way-as-in-java
(defn sync-prn [& toprint]
  (locking *out*
    (apply prn toprint)))


;; It is qute difficult from the above to understand/see what swap! is actually doing
;; Let's try and create an example that forces swap! to have to retry in a thread

(defn update-atom [a thread-name]
  (swap! a
         (fn [current-state]
           (sync-prn "update-atom: " thread-name) ;; If this gets prnting multiple times we know that the
                                             ;; previous attempt had to be retried
                                             ;; Use sync-prn rather than prn to make each call atomic                                             
           (Thread/sleep 2000) ;; Sleep to ensure that multiple threads are attempting swap! concurrently
           (let [count (:count current-state)
                 state1 (assoc current-state :count (+ count 1))
                 state2 (assoc state1 thread-name true)]
             state2))))


(defn atom-swap-test []
  (let [a (atom {:count 1})]
    (future (update-atom a "Thread-1"))  ;; 3 threads will all be trying to update the atom at the same time
    (future (update-atom a "Thread-2"))
    (future (update-atom a "Thread-3"))
    a))

(comment
  (def atom1 (atom-swap-test))
  @atom1
  )

  ;;; [E] ********** Refs
  ;;;   TODO
  

  ;;; [F] ********** Dynamic variables
  ;;;   TODO
  
  ;;; [G] Locking
  ;;; I have already justed locking above in sync-prn
  ;;; Just wanted to give another example here  
  
  ;; Rather than locking the *out* stream this creates a java Object in a closure and 
  ;; locks this everytime a print is performed.
  ;; The author claims it is better because you ae not locking a global object, which I agree with  
  (let [lock (Object.)]
    (defn sync-println [& args]
      (locking lock (apply println args))))

(comment 
(sync-println "this is a test" "yyy")

)