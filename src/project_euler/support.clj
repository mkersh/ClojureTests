;;; Support functions for my project_euler
;;;
;;; What I am currently doing with future(s) does not work
;;; see https://stackoverflow.com/questions/42700407/immediately-kill-a-running-future-thread
(ns project-euler.support)

(defonce JOB-LIST (atom {}))

(defn start-task [name f args]
  (let [answer (future (apply f args))
        jobs @JOB-LIST
        new-jobs (assoc jobs name answer)]
    (reset! JOB-LIST new-jobs)))

(defn wait-task [name wait-time]
  (deref (get @JOB-LIST name) wait-time :wait-timeout-expired))

(defn kill-task [name]
  (future-cancel (get @JOB-LIST name))
  (reset! JOB-LIST (dissoc @JOB-LIST name))
  )

(defn test-func []
  (prn "Started test-func")
  (Thread/sleep 15000)
  (prn "Finished test-func")
  :test-func-finished
  )

(defonce DBG (atom nil))

(defn start-thread [f args]
  (Thread.
   (fn []
    (println "Does this thread do anything!!!"))))

(defn start-thread2 [f args]
  (Thread.
   (fn []
     (try
       (apply f args)
       (reset! DBG "HERE!!")
       (catch InterruptedException e
         (println (.getMessage e)))))))


(comment
  (def t (start-thread test-func []))
  t
  (.start t)
  (.interrupt t)
  @DBG

  (start-task :job2 test-func [])
  (wait-task :job2 4000)
  (kill-task :job2)

  (start-task :job1 + [1 2 3])
  @JOB-LIST
  (wait-task :job1 4000)
  (kill-task :job1)
;;
  )