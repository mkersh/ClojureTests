;;; Support functions for calling JSON API calls 
;;; 
;;; [1] (process-collection <steps-collection>)
;;;    Takes a collections that defines a number of API related steps to perform.
;;;    See example-collection
;;;
;;; [2] (apply-api <api-template> <context>)
;;;    Expand the <api-template> using the <context> and call the resulting API structure.
;;;    See json-api-example
;;;    See https://github.com/ring-clojure/ring for details of the <api-template>
;;;    Add the results from the API call to the <context> and return.
;;;       - Results are by default added using :last-call attribute or under attribute identified in :saveas
;;; 
(ns http.api.api_pipe
  (:require [http.api.json_helper :as api]
            [clojure.pprint :as pp]))


(defn show-only-val [context, nm, val]
  (let [show-only? (:show-only context)]
    (if show-only?  (str "{{" (name nm) "}}")
        val)))

(defn save-part-to-context [part-path name]
  (fn [context _] ; 2nd param is the step object
    (let [last (:last-call context)
          part (api/get-attr last (show-only-val context name part-path))]
      (assoc context name (show-only-val context name part)))))

(defn save-value-to-context [val name]
  (fn [context _] ; 2nd param is the step object
    (assoc context name (show-only-val context name val))))

(defn save-context-value-to-context [old-name new-name]
  (fn [context _] ; 2nd param is the step object
    (assoc context new-name (show-only-val context name (get context old-name)))))


(defn save-last-to-context [name]
  (fn [context _] ; 2nd param is the step object
    (assoc context name (:last-call context))))

(defn append-last-to-context [item-id name]
  (fn [context _] ; 2nd param is the step object
    (let [previous-val (get context name)]
      (assoc context name (conj previous-val {item-id (:last-call context)})))))

;; merge with save-last-to-context when have time
(defn save-last-to-context2 [item-id name]
  (fn [context _] ; 2nd param is the step object
    (assoc context name {item-id (:last-call context)})))


(defn print-context [label]
  (fn [context _] ; 2nd param is the step object
    (do (prn label)
        (pp/pprint context))
    context))


;;; *******************************************************
;;; Example datastructures that this file handles

;; An individual API request
;; Part of a parent collection See example-collection below
(defn json-api-example
  [context]
    {:url (str "{{*env*}}/clients/" (:custid context))
     :method api/GET
     :headers {"Accept" "application/vnd.mambu.v2+json"}
     :query-params {}})

;; A collection of steps     
(def example-collection
  {:context {:accID 1, :custName "Smith"}
   :steps [{:pre-filter nil
            :request json-api-example
             ;;:post-filter (save-last-to-context :cust-list)}
            :post-filter [(save-part-to-context [0 "id"] :last-id)
                          (save-part-to-context [0 "birthDate"] :last-date)
                           ;(save-last-to-context :cust-list)
                          ]}

           {:pre-filter nil
            :request (fn [_] ; again remember to wrap requests as functions
                       {:method (fn [_,_] (prn "Step 2"))})
            :post-filter nil}
           {:request (fn [_]
                       {:method (fn [_,_] (prn "Step 3"))})}]})

(defn method-name [fn]
  (cond
    (= fn api/GET) "GET"
    (= fn api/POST) "POST"
    (= fn api/DELETE) "DELETE"
    (= fn api/PUT) "PUT"
    (= fn api/PATCH) "PATCH"
    :else "UNKNOWN-METHOD"))

(defn show-only-method [context req]
  (let [show-only (:show-only context)]
    (if show-only
      (assoc req :method (method-name (:method req)))
      req)))

(defn process-api-request [context request]
  (let [request0 (show-only-method context (request context)) ; expand the request using the context
        request1 (assoc request0 :throw-errors (:throw-errors context))
        url (:url request0)
        api-method (:method request0)]
    (if (:show-only context)
      (do (prn "DEBUG:")
          (pp/pprint request0))
      (api-method url request1))))

(defn process-filter [filterFn context step]
  (if (vector? filterFn)
    (reduce #(process-filter %2 %1 step) context filterFn)
    (filterFn context step)))

(defn opt-print-verbose [context step]
  (let [label (:label step)
        verbose? (:verbose context)]
    (if (and verbose? label)
      (prn label)
      nil)))

(defn next-step [context step]
  (opt-print-verbose context step)
  (if (:ignore-rest context) context
      (let [pre-filter (:pre-filter step)
            new-context1 (if pre-filter
                           (process-filter pre-filter context step)
                           context)
            request (:request step)
            request-results (if request (process-api-request new-context1 request) nil)
            new-context2 (assoc new-context1 :last-call request-results)
            post-filter (:post-filter step)]
        (if post-filter
          (process-filter post-filter new-context2 step)
          new-context2))))

(defn- find-jump-pos [id-val steps-list]
  (+ 1 (count
        (take-while #(not (= (:id %1) id-val)) steps-list))))

(find-jump-pos :jump-here [1 {:id :jump-here} 3])

(defn jump-to-step [col steps-list]
  (let [jumpto (:jump-to-step col)
        jumpId (if (vector? jumpto)
                 (first jumpto)
                 jumpto)
        oneOnly (if (vector? jumpto)
                  (= (second jumpto) :one-only)
                  false)]
    (if jumpto
      (let [start-from (drop (- (find-jump-pos jumpId steps-list) 1) steps-list)]
        (if oneOnly (take 1 start-from) start-from))
      steps-list)))

;; Test jump-to-step
;;(jump-to-step {:jump-to-step [:jump-here :one-only]} [1 2 {:id :jump-here} 3 4 5])

(defn process-collection [col]
  (reduce next-step (:context col) (jump-to-step col (:steps col))))

;; Next function is an easy way to call individual API calls
(defn- apply-api1 [api-obj context]
  (let [steps {:context context
               :steps [{:request api-obj}]}
        context2 (process-collection steps)]
    context2))

(defn- apply-api2 [api-obj context save-results-name]
  (let [steps {:context context
               :steps [{:request api-obj
                        :post-filter (save-last-to-context save-results-name)}]}
        context2 (process-collection steps)]
    context2))

;; Function for applying an API to a context
;; Results of the API are added to the context and returned
(defn apply-api
  ([api-obj context]
   (if-let [saveas (:saveas context)]
     (apply-api2 api-obj context saveas)
     (apply-api1 api-obj context)))
  ([api-obj context save-results-name]
   (apply-api2 api-obj context save-results-name)))

;; convenience function for calling a single api-def
(defn call-api
  ([api context attr-to-return]
   (let [res (call-api api context)
         val (get res attr-to-return)]
     val))
  ([api context]
   (let  [res (apply-api api context)]
     (:last-call res))))


(comment
  
  (api/setenv "env2")
  (json-api-example {:custid "019327031"})

  ;; process-api-request is low-level function for calling APIs
  ;; Better method is apply-api (which adds results to the context)
  (process-api-request (:context example-collection) json-api-example)

  (apply-api json-api-example {:custid "019327031"})

  (process-collection example-collection)

  (reduce prn "start" [1 2 3]))