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

;; functions for attaching custom-fields to objects within a config-as-code
(defn get-custom-field-single-obj [fsid field-val-list]
  (let [field-val-objs (mapv (fn [[fieldid val]] {:customFieldId fieldid :value val}) field-val-list)]
    {:id fsid :standardCustomFieldValues field-val-objs})) 

(defn get-custom-field-table-obj []
  nil ;; TBD
  )

  (defn create-custom-fieldset-obj [obj-list]
    {:customFieldValueSets obj-list})


(comment
  ;; refer to public functions to remove kondo warnings above
  get-yaml-response
  write-yaml-file
  yaml-file-to-edn
  yaml-str-to-edn
  edn-to-yaml-str

  (create-custom-fieldset-obj (get-custom-field-single-obj "fieldsetid" [["f1id" 1] ["f2id" 2]]))
  ;;
  )
