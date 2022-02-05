;;; Functions to create a capability-taxonomy folder structure
;;;

(ns tools.capability-category.captax
  (:require
   [clojure.string :as str]))

(defn reform-cap-str [cap-parts]
  (let [first-part (first cap-parts)
        cap-parts1 (if (= first-part "") (next cap-parts) cap-parts)]
    (reduce (fn [p it]
              (str p "/" it))
            "" cap-parts1)))

(defn path-str-to-list [cap-dir]
  (str/split cap-dir #"\/"))

(defn get-full-path [first-part rest-parts last-cap-dir]
  (prn "Rest parts:" rest-parts)
  (let [rest-str (reform-cap-str rest-parts)]
    (condp = first-part
      "" (do (prn "root path") rest-str)
      "." (do (prn "use current path") (str last-cap-dir rest-str))
      ".."
      (let [parts (path-str-to-list last-cap-dir)
            parts1 (reverse parts)
            num-parts (count parts1) ;; if 2 then parent-str = "/"
            parts2 (next parts1) ;; Only remove if more than one real-part
            parts3 (reverse parts2)
            parent-str (if (= num-parts 2) "" (reform-cap-str parts3))
            _ (prn "Res = " parts3)]
        (prn "use parent path")
        (str parent-str rest-str)))))


(defn create-cap-dir [last-cap-dir cap-dir]
  (let [parts (str/split cap-dir #"\/")
        first-part (first parts)
        rest-parts (next parts)
        cap-dir-full (get-full-path first-part  rest-parts last-cap-dir)
        ]
  (prn "Creating Dir")
  (prn "Last Dir" last-cap-dir)
  (prn "New Dir" cap-dir-full)
  cap-dir-full ;; This becomes the last-cap-dir for the next item in the reduce
  ))

;; Create a directory structure from the cap-list.
;;     cap-list items are strings identifying a capability.
;;     cap-item can start with "/", "." or ".."
(defn create-taxonomy [cap-list]
  (reduce create-cap-dir "" cap-list))


(comment

(create-cap-dir "/channel" "./self-service")
(reform-cap-str ["gg" "channel"])

(create-taxonomy ["/channel"])
(create-taxonomy ["/channel" "/onboarding-origination" "/customer"])



(str/split "../customer/f2/f3" #"\/")
(next [1 2 3])

(reform-cap-str ["1" "2" "3" "4"])
(get-full-path ".." ["1" "2" "3" "4"] "old")

;; Testing that we can move between str and list versions of a path
(let [parts (path-str-to-list "/1/2/3")]
  (reform-cap-str (next parts)))

(create-cap-dir "/channel" "../self-service")
;;
)