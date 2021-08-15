(ns tools.repl-navigate
  (:require [clojure.string :as str]))

(defn name-to-string [varname]
(let [namestr (name varname)
      replace1 (str "/" (str/replace namestr "." "/"))
      replace2 (str/replace replace1 "-" "_")]
      replace2))

(defn ns-swap [name]
  (load (name-to-string name))
  (in-ns name)
  (use 'tools.repl-navigate)
  )
