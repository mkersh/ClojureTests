(ns http.api.mambu.config_as_code.casc_helper
  (:require [clojure.java.io :as io]
            [clj-yaml.core :as yaml] ;; https://github.com/clj-commons/clj-yaml
            
            ))


(defn file->bytes [xin]
  (with-open [xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    ;;(.toByteArray xout)
    (.toString xout "UTF-8")))

(defn get-yaml-response [xin]
  (let
   [yaml-file-stream xin
    yaml-str (file->bytes yaml-file-stream)]
    yaml-str))

(defn write-yaml-file [xin fpath1]
  (let
   [yaml-file-stream xin
    yaml-str (file->bytes yaml-file-stream)
    _  (io/make-parents fpath1)
    _ (spit fpath1 yaml-str)]
    (prn "YAML output:" fpath1)))

(defn yaml-file-to-edn [yaml-path]
  (let [yaml-str (slurp yaml-path)
        edn-obj (yaml/parse-string yaml-str)]
    edn-obj))

(defn yaml-str-to-edn [yaml-str]
  (let [edn-obj (yaml/parse-string yaml-str)]
    edn-obj))

(defn edn-to-yaml-str [obj]
  (yaml/generate-string obj))

(comment
  ;; refer to public functions to remove kondo warnings above
  get-yaml-response
  write-yaml-file
  yaml-file-to-edn
  yaml-str-to-edn
  edn-to-yaml-str


  ;;
  )
