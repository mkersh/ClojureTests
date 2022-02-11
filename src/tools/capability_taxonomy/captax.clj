;;; Functions to create a capability-taxonomy folder structure
;;;

(ns tools.capability_taxonomy.captax
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]))

;; This is the directory where the capability-hierarchy will be created
;; 
(defonce CAPTAX-DIR (atom "/Users/mkersh/captax/CAPTAX-Examples/captax01"))

(defonce CAPTAX-LIST (atom []))

;; Will contain options to filter ECM generation
(defonce ECM-GEN-OPTIONS (atom {:remove-prefix [#"/component"]}))
(defonce GAS-LIST (atom [])) ;; save {:row :label} into this list
(defonce ECM-ROW (atom 1)) ;; This will be updated with the row of the CSV

(defn reform-cap-str [cap-parts]
  (let [first-part (first cap-parts)
        cap-parts1 (if (= first-part "") (next cap-parts) cap-parts)]
    (reduce (fn [p it]
              (str p "/" it))
            "" cap-parts1)))

(defn reform-cap-str2 [cap-parts]
  (let [first-part (first cap-parts)
        cap-parts1 (if (= first-part "") (next cap-parts) cap-parts)]
    (reduce (fn [p it]
              (str p "." it))
            "" cap-parts1)))

(defn path-str-to-list [cap-dir]
  (str/split cap-dir #"\/"))

(defonce MEM-MAP (atom {}))

(defn expand-cap [mem-id cap-it]
  (let [mem-map @MEM-MAP
        parent-cap (get mem-map mem-id)]
  (str parent-cap cap-it)))

(defn remember-cap [mem-id cap-it last-cap-dir]
  (reset! MEM-MAP (assoc @MEM-MAP mem-id (if (= cap-it "") last-cap-dir cap-it)))
  cap-it)


(defn get-full-path [first-part rest-parts last-cap-dir]
  (let [rest-str (reform-cap-str rest-parts)]
    (condp = first-part
      ""  rest-str
      "[M1]"  (remember-cap "M1" rest-str last-cap-dir)
      "[M2]"  (remember-cap "M2" rest-str last-cap-dir)
      "[M3]"  (remember-cap "M3" rest-str last-cap-dir)
      "[M4]"  (remember-cap "M4" rest-str last-cap-dir)
      "@M1"  (expand-cap "M1" rest-str)
      "@M2"  (expand-cap "M2" rest-str)
      "@M3"  (expand-cap "M3" rest-str)
      "@M4"  (expand-cap "M4" rest-str)
      "." (str last-cap-dir rest-str)
      ".." (let [parts (path-str-to-list last-cap-dir)
                 parts1 (reverse parts)
                 num-parts (count parts1) ;; if 2 then parent-str = "/"
                 parts2 (next parts1) ;; Only remove if more than one real-part
                 parts3 (reverse parts2)
                 parent-str (if (= num-parts 2) "" (reform-cap-str parts3))]
             (str parent-str rest-str))
      (throw (Exception. (str "Unknown prefix " first-part " - " rest-str))))))

(defn file-exists? [fp]
  (.exists (io/file fp)))

(defn label-str-to-list [cap-dir]
  (str/split cap-dir #"\."))

(defn parent-label [label level]
  (let [label-parts (label-str-to-list label)]
    (take (- level 1) label-parts)))


;; Next function groups rows using the following rules:
;; (1) Label with >= level are candicates for the group
;; (2) Label(s) should share the same parent to (- level 1)
;;
(defn group-level [level row-label-list body-str]
  ;; Loop through row-label-list and group all rows, with label >= level, that share the same parent 
  (let [groupit-obj
        (reduce (fn [context row-item]
                  (let [row-start (:row-start context)
                        row (:row row-item)
                        label (:label row-item)
                        prev-parent (:parent context)
                        parent (parent-label label level)
                        label-depth (+ (count parent) 1)
                        res-buffer (:res-buffer context)
                        _ (prn "group-level: level=" level " label " label " parent= " parent " prev-parent= " prev-parent " label-depth= " label-depth)
                        ]
                    (if (= prev-parent "")
                      ;; 01 - option
                      (do
                        (prn "option-1")
                        {:res-buffer res-buffer :level level :parent parent :row-start nil})
                      (if (and (= parent prev-parent) (>= label-depth level))
                      ;; 02 - option
                        (do
                          (prn "option-2")
                          {:res-buffer res-buffer :level level :parent parent :row-start (or row-start row) :row-end row})
                        (if row-start
                        ;; 03 - option
                          (let [_ (prn "option-3")
                                range-str (str "'" row-start ":" (- row 1) "'")
                                gr-str (str "groupIt(" range-str ");\n")
                                new-buffer (str res-buffer gr-str)]
                            {:res-buffer new-buffer :parent parent :row-start nil :row-end nil})
                          {:res-buffer res-buffer :parent parent :row-start nil :row-end nil})))))
                {:res-buffer "" :parent nil :row-start nil}
                row-label-list)
        row-start (:row-start groupit-obj)  
        row-end (:row-end groupit-obj)   
        body1 (:res-buffer groupit-obj) 
        last-gr (if row-end
                  (let [range-str (str "'" row-start ":" row-end "'")
                        gr-str (str "groupIt(" range-str ");\n")]
                        gr-str
                        ) 
                  nil)
        body2 (str body1 last-gr)
        new-body (str body-str body2)]
    new-body)
  )


;; build up a string of "groupIt(range-str);" calls for rows in the ECM
;; that need to be grouped together
;; 
(defn build-ecm-patch-body [row-label-list group-max-depth]
  (loop [i 2
         body-str ""]
    (let [body2 (group-level i row-label-list body-str)]
      (if (> i group-max-depth)
        body2
        (recur (inc i) body2)))))


(comment
  (build-ecm-patch-body (take 212 @GAS-LIST) 2)
  (parent-label "1.2.1" 3) 
(count @GAS-LIST)
(take 212 @GAS-LIST)
(str nil "shsh")
(str/replace "aaa@@BODYbbbb" "@@BODY" "This is my body")
;;
  )

;; @GAS-LIST contains all the info needed to create the ECM patch file
;; This patch file needs to generate instructions to group rows
(defn create-ecm-patch-script []
  (let [full-path (str @CAPTAX-DIR "/..")
        dummy-file (str full-path "/dummy.txt")
        patch-body (build-ecm-patch-body @GAS-LIST (:group-max-depth @ECM-GEN-OPTIONS))
        from-fp  "src/tools/capability_taxonomy/ecm-patch-script-template.txt"
        template-str (slurp from-fp)
        patch-file-str (str/replace template-str "@@BODY" patch-body)
        to-fp (str full-path "/ecm-patch-script.gs")]
    (io/make-parents dummy-file)
    (spit to-fp patch-file-str)))

(defn create-folders-and-files [cap-dir-full]
  (let [full-path (str @CAPTAX-DIR cap-dir-full)
        dummy-file (str full-path "/dummy.txt")
        from-fp  "src/tools/capability_taxonomy/TEMPLATE-README.md"
        to-fp (str full-path "/README.md")]
    (io/make-parents dummy-file)
    (when (not (file-exists? to-fp))
      (sh/sh "cp" from-fp to-fp))
    
    ))

(defn create-cap-dir [last-cap-dir cap-dir]
  (let [parts (str/split (str/trim cap-dir) #"\/")
        first-part (first parts)
        rest-parts (next parts)
        cap-dir-full (get-full-path first-part  rest-parts last-cap-dir)
        ]
  (if cap-dir-full
    (do (prn cap-dir-full)
        (create-folders-and-files cap-dir-full)
        (reset! CAPTAX-LIST (conj @CAPTAX-LIST cap-dir-full))
        cap-dir-full ;; This becomes the last-cap-dir for the next item in the reduce
        )
    last-cap-dir)
  ))

;; Create a directory structure from the cap-list.
;;     cap-list items are strings identifying a capability.
;;     cap-item can start with "/", "." or ".."
;;
(defn create-taxonomy [cap-list]
  (reset! CAPTAX-LIST []) 
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

(defn reset-counters [context cnt]
  (let [context1 (if (< cnt 2) (assoc context 2 0) context)
        context2 (if (< cnt 3) (assoc context1 3 0) context1)
        context3 (if (< cnt 4) (assoc context2 4 0) context2)
        context4 (if (< cnt 5) (assoc context3 5 0) context3)
        context5 (if (< cnt 6) (assoc context4 6 0) context4)
        context6 (if (< cnt 7) (assoc context5 7 0) context5)]
        context6
        ))
(defn filter-cap-label [label-str ecm-gen-options]
  (let [remove-list (:remove-prefix ecm-gen-options)]
    (reduce (fn [res it]
              (str/replace res it "")) label-str remove-list)))

;; The item to add to the ECM CSV file is line-item
;; What we do in this function though is gather information needed for create-ecm-patch-script
(defn save-ecm-line [label-str line-item]
  (swap! ECM-ROW inc) ;; increment ECM-ROW
  (reset! GAS-LIST (conj @GAS-LIST {:row @ECM-ROW :label label-str}))
  (println line-item))

(comment 
(reset! ECM-ROW 1)
(swap! ECM-ROW inc)
(save-ecm-line "1" "line1")
(save-ecm-line "1.1" "line2")
@GAS-LIST
)

(defn save-cap-item [context cap-item]
  (let [cap-item2 (filter-cap-label cap-item @ECM-GEN-OPTIONS)
        parts (str/split (str/trim cap-item2) #"\/")
        parts1 (next parts)
        parts2 (reverse parts1)
        cnt (count parts2)
        it (first parts2)
        context1 (reset-counters context cnt)
        context2 (if (get context1 cnt) (update context1 cnt inc) (assoc context1 cnt 1))
        label-list (map
                    (fn [pos]
                      (get context2 (+ pos 1))) (range 0 cnt))
        label-str (if (empty? label-list) nil (subs (reform-cap-str2 label-list) 1))]
    (when label-str (save-ecm-line label-str (str label-str "," it "," "," "," "," "," "," )))
    context2))

(defn generate-ECM-file [cap-list save-root]
  (reduce save-cap-item {} cap-list))

(defn create-ECM-from-file [filepath dest-root ecm-root]
  (let [_ (create-taxonomy-from-file filepath dest-root)
        ecm-path (str ecm-root "/ECM.csv")]
    (reset! ECM-ROW 1) ;; Reset the Row counter we are on
    (reset! GAS-LIST []) ;; Reset GAS-LIST
    (io/make-parents ecm-path)
    ;; built the @CAPTAX-LIST when calling create-taxonomy-from-file above
    (with-open [out-data (io/writer ecm-path)]
      (binding [*out* out-data]
        (println "ID, Component, Marketplace, Other, Option Chosen, Connector(s), Comments, Re-use v Build v Buy v SaaS,")
        (generate-ECM-file @CAPTAX-LIST ecm-root)))
    (create-ecm-patch-script)))

(comment

(range 0 2)

(reset! CAPTAX-DIR "/Users/mkersh/captax/CAPTAX-Examples/captax01")

;; [1] create a capability-taxonomy from a file definition
;;
(create-taxonomy-from-file (str @CAPTAX-DIR ".txt") @CAPTAX-DIR)

;; [2] Delete an existing capability-taxonomy directory
;;
(delete-dir @CAPTAX-DIR)

;; [3] Create an ECM CSV file
(create-ECM-from-file (str @CAPTAX-DIR ".txt") @CAPTAX-DIR (str @CAPTAX-DIR "/.."))
;; [3b] Change the default gen options 
(reset! ECM-GEN-OPTIONS {:remove-prefix [#"/component"] :group-max-depth 7})

(conj @CAPTAX-LIST 3)

;; Functions used whilst testing
(create-cap-dir "" "[M1]/root-cap")
(create-cap-dir "" "@M1/child")
(create-cap-dir "hello there" "[M2]")
(create-cap-dir "hello there" "@M2/jjj")

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



(filter-cap-label "ababababab" {:remove-prefix [#"b" #"a"]})
(filter-cap-label "/customer/reg-reporting" {:remove-prefix [#"/customer"]})
;;
)
