;;; Support functions for my project_euler
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


(comment
(start-task :job2 test-func [])
(wait-task :job2 4000)
(kill-task :job2)

(start-task :job1 + [1 2 3])
@JOB-LIST
(wait-task :job1 4000)
(kill-task :job1)
;;
)