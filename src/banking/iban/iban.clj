(ns banking.iban.iban
   (:require
    [big-ivan.bic :as bic]
    [big-ivan.iban :as iban])
  )



(bic/bic? "not-a-bic") 
(bic/bic? "DEUTDEFF") 

(def some-value "DEUTDEFF")
(if-let [b (bic/bic? some-value)]
  (bic/country-code b)
  nil)

(bic/bic "DEUT" "DE" "FF" "007")

(iban/iban? :not-an-iban) 
(iban/iban? "SA0380000000608010167519") 

(iban/bban "SA0380000000608010167519") 

(iban/add-spaces "SA0380000000608010167519")


;; Generate an IBAN from country-code and BBAN
;; 
 
(iban/iban "RO" "BTRLLGBO89B2P72HNFGM")

(iban/iban "GB" "NWBK60161331926822")


(iban/bic  "RO26BTRLLGBO89B2P72HNFGM")

(* 54 60)
