(ns debug-tools.test
  (:require
  [debug_tools.all :as dt]
   ))

;; The next line seems to work after debug-tools.all has required ref1
(dt/printClassPath)