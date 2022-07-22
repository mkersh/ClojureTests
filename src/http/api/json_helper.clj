;; Helper functions for calling JSON APIs using http://http-kit.github.io/
;; GitHub: https://github.com/mkersh/ClojureTests/tree/master/src/http/api/json_helper.clj 
;;
;; Useful helper functions:
;; [1] GET, POST, PATCH, DELETE, PUT
;;   Params: <url-endpoint> <options>
;;     <options> - See https://github.com/ring-clojure/ring 
;;   Return: Results of the API call
;;
;;   See also api_pipe/apply-api for a higher level way of calling APIs
;;   
;; [2] setenv/setenv-local - sets the value of {{*env*}} used in API endpoints e.g "{{*env*}}/clients/"
;; [3] find-path
;; [4] get-attr - to extract the results you want from a results
;;   This function is not needed use clojure standard get-in instead.
;; [5] extract-attrs - Extract attributes from an object or an array-of-objects 
;;

(ns http.api.json_helper
  (:require
   [clojure.data.json :as json]
   [org.httpkit.client :as client]
   [clojure.string :as str]
   [http.ENV :as env] ; This is the file to store env secrets in - make sure its in your .gitignore
   [clojure.pprint :as pp]))

(declare expandURL expand-options best-response request GET2)

(defn uuid [] (.toString (java.util.UUID/randomUUID)))

;;; ----------------------------------------------------------------------
;;; JSON - Helper Methods:
;;; 
;;; GET, POST, DELETE, PATCH, PUT
;;; 

;; --------------------------------------
;; Support 2 ways to define the ENV that will be used to expand {{*env*}} placeholders in API URL endpoints
;; (1) *ENV* - to be set using (binding *ENV* ...)
;; (2) (setenv ...) - which will set the ENV def
;;
;; (get-env-key) - returns the ENV key from either (1) or (2)
;;
;; To define the value to use for {{*env*}} use one of the following:
;;     [1] (api/setenv "env2") ;; set to use https://markkershaw.mambu.com
;;     [2] (binding [api/*ENV* "env2"]
;;            (print-client-page {:page-size 3, :page-num 0}))
;;
(def ^:dynamic *ENV* nil)
(declare get-env-domain)

(defmacro ignore-exceptions [& body]
  `(try ~@body (catch Exception e#)))

(defn setenv [envId]
  (def ENV envId)
  (ignore-exceptions (get-env-domain))
  )
(setenv "env1")

(defmacro setenv-local [envId & body]
  `(binding [*ENV* ~envId] ~@body)
)

(defn add-param [m p v]
  (if v (assoc m p v) m))

;; Get the environment to use 
(defn get-env-key []
  (if *ENV* *ENV* ENV))

(defn get-env-domain []
  (let [url (:url (get env/ENV-MAP (get-env-key)))
        uri (new java.net.URI url)
        domain (.getHost uri)]
    domain))



(get-env-domain)

;; ------------------------------------------

(defn set-show-only [b] ;; call this is just display the calls but not execute
  (def ^:dynamic *show-only* b))
(set-show-only false) 

(defn convertJsonFileToEdn [fn]
  (let [fileStr (slurp fn)]
    (json/read-str fileStr)))
    
(defn GET
  ([url] (GET2 url, {}))
  ([url, options] (GET2 url, options)))

(defn POST [url, options]
  (request url, client/post, options))

(defn PATCH [url, options]
  (request url, client/patch, options))

(defn DELETE [url, options]
  (request url, client/delete, options))

(defn PUT [url, options]
  (request url, client/put, options))

(defn PRN [str1 & options ]
  (if (:no-print (first options))
    nil
    (prn str1)
    ))

(defn- request [url, method, options0]
  ;;(prn "request START")

  (if (= *show-only* false)
    (let [url-expanded (expandURL url)
          options (expand-options options0 url)
          response @(method url-expanded options)
          status (:status response)]

    ;;(prn options)
    ;;(prn "request ENDxxx")
      (if (< status 300)
        ;;(PRN (str "Successful Call: " status) options0)
        nil
        (if (:throw-errors options)
          (throw (Exception. (str "ERROR Status: " status)))
          (prn "ERROR Status: " status)))
      (best-response response))

    ;; (= *show-only* true)
    ;; Just show the ring request that would be called
    (let [url-expanded (expandURL url)
          options (expand-options options0 url)
          ;; https://github.com/ring-clojure/ring/blob/master/SPEC 
          api-str  {:url url-expanded :ring-request options}]
      (prn "DEBUG ONLY - *show-only* flag enabled")
      (prn "To unset call (api/set-show-only false) ")
      api-str)))

(defn- GET2 [url, options]
  (request url, client/get, options))

;;; ----------------------------------------------------------------------
;;; Print and Extraction helper functions
;;; 
;;; 

(defn to-json [obj]
  (json/write-str obj))

(defn PRINT [http-kit-response & options]
  (if (:no-print (first options))
    http-kit-response
    (let [num-items (count http-kit-response)]
      (pp/pprint http-kit-response) ; This was pretty-printing the complete result
      (prn "Number of items:" num-items))))

(defn get-auth 
  ([] (get-auth ENV))
  ([envId]
    (:basic-auth (get env/ENV-MAP envId)))
  )

(defn get-auth2
  ([] (get-auth2 ENV))
  ([envId]
   (:ApiKey (get env/ENV-MAP envId))))

(defn get-num [strNum]
  (BigDecimal. strNum))

(defn round-num [num]
  (format "%.2f" (bigdec num)))

(defn get-env-item [envId]
  (get env/ENV-MAP envId))

(declare get-lists-from tidyup-results findPath-aux)

;; Besides the conversion to :DECIMAL this function is nor needed
;; Use the clojure standard get-in instead
(defn get-attr [obj nested-fields & format]
  (let [att-val (reduce #(get %1 %2) obj nested-fields)]
    (if (= (first format) :DECIMAL)
      (get-num att-val)
      att-val)))



(defn find-path 
  "Given a resultEdn find a match to matchStr
   Return the access-path to where the matchStr is in the resultEdn
   NOTE: Likely use is to add '@@' into a key/value of the resultEdn"
  [matchStr resultEdn]
  (assert (string? matchStr)) ; make sure a String has been passed
  (get-lists-from (tidyup-results (findPath-aux resultEdn matchStr []))) ; last parm builds up the access-path
  )

(declare get-obj-attrs)

(defn extract-attrs 
  "Extract the attributes defined in attrList from the objOrList passed in"
  [attrList objOrList]
  (let [objList ; make sure objOrList is a vector
        (if (vector? objOrList)
          objOrList
          (vector objOrList))]
    (map #(get-obj-attrs % attrList) objList)))

(defn docstring [symbol]
  (:doc (meta (resolve symbol))))

;;; ----------------------------------------------------------------------
;;; Internal helper functions
;;; 
;;; 

(defn- get-obj-attrs [obj attrList]
  (let [attrValList (map #(vector % (get obj %)) attrList)]
    (into {} attrValList)))

;;; Functions for examining JSON API results returned
(declare findPath-vector findPath-map search-leaf)
(defn- DEBUG [& args]
  (identity args) ; To prevent clj-kondo warning
  ;(apply println args)
  )

(defn- search-map-item [keyValueItem matchStr accessPathList]
  (DEBUG "Search Map Item:" keyValueItem)
  (let
   [[k item] keyValueItem
    newAccessPathList (conj accessPathList k)]
    (cond
      (vector? item)
      (concat (findPath-vector item matchStr newAccessPathList)
              (search-leaf k nil matchStr newAccessPathList))

      (map? item)
      (findPath-map item matchStr newAccessPathList)
      :else
      (search-leaf k item matchStr newAccessPathList))))

(defn- findPath-map [root matchStr accessPathList]
  (DEBUG "Searching in map")
  (map #(search-map-item % matchStr accessPathList)  root))


(defn- search-leaf [key item matchStr accessPathList]
  (DEBUG "Search Leaf Node: " item " access path: " accessPathList)
  (if (or (.contains (str key) matchStr)
          (.contains (str item) matchStr))
    (do
      (DEBUG "*****Match Found****** " item "AccessPath: " accessPathList)
      accessPathList)
    nil))

(defn- search-vec-item [indexedItem matchStr accessPathList]
  (DEBUG "Search Vector Item:" indexedItem)
  (let
   [[index item] indexedItem
    newAccessPathList (conj accessPathList index)]
    (cond
      (vector? item)
      (findPath-vector item matchStr newAccessPathList)

      (map? item)
      (findPath-map item matchStr newAccessPathList)
      :else
      (search-leaf nil item matchStr newAccessPathList))))


(defn- findPath-vector [root matchStr accessPathList]
  (let
   [enumList (map-indexed vector root)]
    (DEBUG "Searching in Vector")
    (DEBUG enumList)
    (map #(search-vec-item % matchStr accessPathList)  enumList)))

(defn- findPath-aux [root matchStr accessPathList]
  (DEBUG "root= " root)
  (cond
    (vector? root)
    (findPath-vector root matchStr accessPathList)

    (map? root)
    (findPath-map root matchStr accessPathList)
    :else
    (search-leaf nil root matchStr accessPathList)))


(declare get-lists-from)
(defn get-lists-items [res item]
  (cond
    (vector? item)
    (conj res item)
    (seq? item)
    (concat res (get-lists-from item))
    :else
    nil))

(defn get-lists-from [seq1]
  (reduce get-lists-items [] seq1))


(declare tidyup-results)
(defn tidyup-item [res item]
  (DEBUG "considering: " item (type item))
  (cond
    (or (vector? item) (seq? item))
    (do
      (DEBUG "vector or list")
      (let [recRes (tidyup-results item)]
        (if (or (nil? recRes) (empty? recRes))
          res
          (concat res [recRes]))))
    :else
    (do
      (DEBUG ":else " item)
      (if (nil? item)
        res
        (conj res item)))))

(defn tidyup-results [lst]
  (let [res (reduce tidyup-item [] lst)]
    (DEBUG "START tidyup-results " lst)
    (DEBUG "TIDY: " res)
    res))

(defn- get-envID [_ placeHolder]
  (let [;; Need to strip the {{ and }} from the placeholder before doing the lookup
        p2 (str/replace placeHolder "{{" "")
        p3 (str/replace p2 "}}" "")
        p4 (if (= p3 "*env*") (get-env-key) p3)]
    p4))

(defn- replacePlaceholder [currentStr placeHolder]
  (let [;; Need to strip the {{ and }} from the placeholder before doing the lookup
        p2 (str/replace placeHolder "{{" "")
        p3 (str/replace p2 "}}" "")
        p4 (if (= p3 "*env*") (get-env-key) p3)
        ;;placeHolderValue (:url (get env/ENV-MAP p3))
        placeHolderValue (:url (get env/ENV-MAP p4))
        ]
    (str/replace currentStr placeHolder placeHolderValue)))

;; Support the expansion of placeholder in URLs 
;; e.g "{{*env*}}/branches" which will have the {{*env*}} placeholders replaced
(defn- expandURL [url]
  (let [placeholderRegExp #"\{\{[^\}]*\}\}"
        placeholderList (re-seq placeholderRegExp url)
        placeholderSet (set placeholderList)]
    (reduce replacePlaceholder url placeholderSet)))

(defn- get-env-from-URL [url]
  (let [placeholderRegExp #"\{\{[^\}]*\}\}"
        placeholderList (re-seq placeholderRegExp url)
        placeholderSet (set placeholderList)]
    (reduce get-envID url placeholderSet)))

(defn return-byte-stream [body]
  body)

(defn return-result [body]
  (try
     (json/read-str body)
     (catch Exception _ (return-byte-stream body))))

;; Extract the best response from resp
(defn- best-response [resp]
  (let [body (:body resp)
        status (:status resp)]
    (if (= body "")
      (str "SUCCESS: " status) ;; Could return resp for full response but makes it noisy
      (return-result body))))

(defn add-apikey-header [options envId]
  (let [headers (:headers options)
        ext-headers (assoc headers "ApiKey" (get-auth2 envId))]
    (assoc options :headers ext-headers)))

(defn add-auth-header
  "Add an authentication header to the request. 
  Get this using (get-auth)"
  [options url]
  (let [envId (get-env-from-URL url)]
    (if (:basic-auth options)
      options
      (if (get-auth2 envId)
      ;; Use ApiKey method as the preferrence (if supplied) else basic-auth
        (add-apikey-header options envId)
      ;;(assoc options :basic-auth (get-auth))
        (assoc options :basic-auth (get-auth envId))))))

;; Convert options parameters from EDN to JSON
(defn- expand-options [options url]
  ;;(prn "OPTIONS: " options)
  (let [options1 (add-auth-header options url)
        body (:body options1)]
    (if (or (map? body) (vector? body))
      (assoc options1 :body (json/write-str body)) ; Convert body to JSON string if needed
      options1)))

;;(def nCinoConnectorJson (convertJsonFileToEdn "/Users/mkersh/Downloads/folder_684_1599545125.json"))

(comment

(def req1 {:xbasic-auth ["apiUser" "k6RxsaedvbPEg6cjNmXqvxaW32"], :headers {"Accept" "application/vnd.mambu.v2+json"}, :query-params {"detailsLevel" "FULL"}})

(add-auth-header req1 "{{*env*}}/clients/")

(get-env-from-URL "{{*env*}}/clients/")

(conj [:basic-auth 123] req1)

(setenv "env3b")
(get-auth2)
(add-param {} "param1" nil)

;;
)