(ns http.api.mambu.product-type
  (:require [http.api.json_helper :as api]))


(defn get-loan-product [id & opt-overrides]
  (let [moreOpts (first opt-overrides)
        detailLevel (or (:details-level moreOpts) "FULL")
        optdefs {:headers {"Accept" "application/vnd.mambu.v2+json"}
                 :query-params {"detailsLevel" detailLevel}}
        options (merge optdefs moreOpts)
        url (str "{{*env*}}/loanproducts/" id)]
    (api/PRINT (api/GET url options) moreOpts)))