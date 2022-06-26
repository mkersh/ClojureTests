;;; Support functions for my project_euler
;;;
;;; What I am currently doing with future(s) does not work
;;; see https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
(ns project-euler.support)

;;; ***************************************************
;;; Code to redirect all output to the REPL
;;;
;; https://stackoverflow.com/questions/34576109/configure-clojure-logging-to-output-to-nrepl#38294275
;; run this code on the repl where you wish to see all output.
;; You will need to add the dependency [commons-io "2.4"] to your
;; leiningen dependencies.
(import 'org.apache.commons.io.output.WriterOutputStream)
(import 'java.io.PrintStream)

(defn setup-output-to-repl []
  ;; First, we redirect the raw stdout of the server to this repl
  (System/setOut (PrintStream. (WriterOutputStream. *out*)
                               true)) ;; Auto-flush the PrintStream

  ;; Next, we alter the root binding of *out* so that new threads
  ;; send their output to THIS repl rather than the original System/out.
  (alter-var-root #'*out* (fn [_] *out*))

  ;; Now the snippets should both send output to this repl:
  (.println System/out "Hello stdout.")
  (.start (Thread. #(println "Hello from a new thread."))))

(defonce _setupoutput (setup-output-to-repl))
_setupoutput

(defonce JOB-LIST (atom {}))

(defn start-task
  ([name f] (start-task name f []))
  ([name f args]
   (let [func (fn []
                (try
                  (let [answer (apply f args)
                        jobs @JOB-LIST
                        job-obj (get jobs name)
                        job-obj2 (assoc job-obj :result answer)
                        new-jobs (assoc @JOB-LIST name job-obj2)]
                    (reset! JOB-LIST new-jobs))
                  (catch InterruptedException e
                    (println (.getMessage e)))))
         thread1 (Thread. func)
         job-obj {:thread thread1 :result nil}
         new-jobs (assoc @JOB-LIST name job-obj)
         _ (reset! JOB-LIST new-jobs)]
     (.start thread1))))

(defn wait-task [name wait-time]
  (let [job-obj (get @JOB-LIST name)
      answer (:result job-obj)]
    (or answer ;; if answer ready jut return
        (if job-obj
            (do
              (Thread/sleep wait-time) ;; else wait for wait-time
              (or (:result (get @JOB-LIST name)) :wait-timeout-expired))
            (prn "wait-task: task no longer exists!!")
            ))))

(defn kill-task [name]
  (let [thread1 (:thread (get @JOB-LIST name))
        _  (if thread1
             ;; You are not meant to use .stop to kill a thread but I am here
             ;; recommendation is to use (.interrupt thread1) but this does not stop the thread
             ;; without the running codes perticipation. See https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
             ;; Running code has to check (Thread/interrupted) and close down gracefully
             (.stop thread1)
             (prn "kill-task: task no longer exists!!"))
        new-jobs-list (dissoc @JOB-LIST name)
        _ (reset! JOB-LIST new-jobs-list)]
    (prn (str "kill-task: " name " completed"))))

(defn test-func []
  (prn "Started test-func")
  (Thread/sleep 15000)
  (prn "Finished test-func")
  :test-func-finished
  )

(comment
  (start-task :job2 test-func [])
  (wait-task :job2 4000)
  (kill-task :job2)

  (start-task :job1 + [1 2 3])
  @JOB-LIST
  (wait-task :job1 4000)
  (kill-task :job1)


  (:thread nil)
;;
  )

