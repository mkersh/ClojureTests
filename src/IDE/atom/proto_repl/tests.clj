

;; https://atom.io/packages/proto-repl
;;
;; Starting a headless REPL
;; lein repl :headless :host 0.0.0.0 :port 7888
(ns IDE.atom.proto-repl.tests)

(defn test-atom [p1 & rest]
    (prn "Yes this works " p1))

(test-atom 3) 
