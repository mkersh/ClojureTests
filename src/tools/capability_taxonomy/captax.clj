;;; Functions to create a capability-taxonomy folder structure
;;;

(ns tools.capability_taxonomy.captax
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]))

;; This is the directory where the capability-hierarchy will be created
;; 
(defonce CAPTAX-DIR (atom "/Users/mkersh/captax/captax01"))

(defn reform-cap-str [cap-parts]
  (let [first-part (first cap-parts)
        cap-parts1 (if (= first-part "") (next cap-parts) cap-parts)]
    (reduce (fn [p it]
              (str p "/" it))
            "" cap-parts1)))

(defn path-str-to-list [cap-dir]
  (str/split cap-dir #"\/"))

(defn get-full-path [first-part rest-parts last-cap-dir]
  (let [rest-str (reform-cap-str rest-parts)]
    (condp = first-part
      ""  rest-str
      "." (str last-cap-dir rest-str)
      ".."
      (let [parts (path-str-to-list last-cap-dir)
            parts1 (reverse parts)
            num-parts (count parts1) ;; if 2 then parent-str = "/"
            parts2 (next parts1) ;; Only remove if more than one real-part
            parts3 (reverse parts2)
            parent-str (if (= num-parts 2) "" (reform-cap-str parts3))]
        (str parent-str rest-str)))))

(defn file-exists? [fp]
  (.exists (io/file fp)))

(defn create-folders-and-files [cap-dir-full]
  (prn "create-folders-and-files")
  (let [full-path (str @CAPTAX-DIR cap-dir-full)
        dummy-file (str full-path "/dummy.txt")
        from-fp  "src/tools/capability_taxonomy/TEMPLATE-README.md"
        to-fp (str full-path "/README.md")]
    (io/make-parents dummy-file)
    (when (not (file-exists? to-fp))
      (sh/sh "cp" from-fp to-fp))
    
    ))

(defn create-cap-dir [last-cap-dir cap-dir]
  (let [parts (str/split cap-dir #"\/")
        first-part (first parts)
        rest-parts (next parts)
        cap-dir-full (get-full-path first-part  rest-parts last-cap-dir)
        ]
  (prn cap-dir-full)
  (create-folders-and-files cap-dir-full)
  cap-dir-full ;; This becomes the last-cap-dir for the next item in the reduce
  ))

;; Create a directory structure from the cap-list.
;;     cap-list items are strings identifying a capability.
;;     cap-item can start with "/", "." or ".."
(defn create-taxonomy [cap-list]
  (reduce create-cap-dir "" cap-list))

(defn delete-directory-recursive
  "Recursively delete a directory."
  [^java.io.File file]
  ;; when `file` is a directory, list its entries and call this
  ;; function with each entry. can't `recur` here as it's not a tail
  ;; position, sadly. could cause a stack overflow for many entries?
  (when (.isDirectory file)
    (doseq [file-in-dir (.listFiles file)]
      (delete-directory-recursive file-in-dir)))
  ;; delete the file or directory. if it it's a file, it's easily
  ;; deletable. if it's a directory, we already have deleted all its
  ;; contents with the code above (remember?)
  (io/delete-file file))

(defn delete-dir [dir-path]
  (try (delete-directory-recursive (clojure.java.io/file dir-path))
       (catch Exception _ "Nothing to DELETE")))

(defn create-taxonomy-from-file [filepath dest-root]
  (let [file-str (slurp filepath)
        cap-list (str/split-lines file-str)]
  (create-taxonomy cap-list)
  ))



(comment

;; [1] create a capability-taxonomy from a file definition
;;
(create-taxonomy-from-file (str @CAPTAX-DIR ".txt") @CAPTAX-DIR)

;; [2] Delete an existing capability-taxonomy directory
;;
(delete-dir @CAPTAX-DIR)

;; Functions used whilst testing
(create-cap-dir "/channel/dd/ee/ff/gg/hh" "../self-service")
(reform-cap-str ["gg" "channel"])
(slurp "src/tools/capability_taxonomy/TEMPLATE-README.md")

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