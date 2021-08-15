(ns debug_tools.all
   (:require
    [debug_tools.immigrate :as ref1]
   )
)

(ref1/immigrate 'debug_tools.print_classpath)

;; (defn printClassPath []
;;   (ref1/printClassPath)
;;    )
;; Plan to use this to have a require that loads all the debug tools