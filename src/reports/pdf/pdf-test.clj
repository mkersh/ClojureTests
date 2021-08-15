(ns reports.pdf.pdf-test
  (:require [clj-pdf.core :as pdf]))

(defn get-header-size [level]
(condp = level
  1 30
  2 25
  3 20
  4 15
  5 10)
)
(defn main-heading [level title]
  (let [size (get-header-size level)]
    [:heading {:style {:size size :align :left}} title]))
(defn right-heading [title]
[:heading {:style {:size 30 :color [100 40 150] :align :right}} title])
(defn heading5 [title]
  (main-heading 5 title))
(defn heading4 [title]
  (main-heading 4 title))
(defn heading3 [title]
  (main-heading 3 title))
(defn heading2 [title]
  (main-heading 2 title))
(defn heading1 [title]
  (main-heading 1 title))

(defn section [title & body]
  [:section (heading2 title) body])

(defn chapter [title & body]
  [:chapter (heading1 title) body])

(pdf/pdf
 [{}
  (chapter "Headers")
  (heading1 "Header 1")
  (heading2 "Header 2")
  (heading3 "Header 3")
  (heading4 "Header 4")
  (heading5 "Header 5")


  (chapter "Chapters")
  [:paragraph "A chapter starts a new page and puts a header at the top of the page"]
  (chapter "Chapters + Sections"
           (section "section 1"
                    [:paragraph "A chapter can have a section within it"])
           (section "section 2"))

  (chapter "Another chapter" (section "section 1") (section "section 2"))

  (chapter "Lists")
  (heading2 "Lettered Items")
  [:list {:lettered true}
   [:chunk {:style :bold} "a bold item"]
   "another item"
   "yet another item"
   "and another"
   (for [x (range 10)]
     (str "hello " x))]

  (heading2 "Numbered Items")
  [:list {:numbered true}
   (for [x (range 10)]
     (str "hello " x))]

  (chapter "PDF Table")
  (heading2 "Simple Table")
  ; if the widths vector that normally would be after the metadata map is nil, the
; pdf-table's column widths will be automatically figured out (evenly spaced)
  [:pdf-table
   {:width-percent 100
    :spacing-before 10}
   nil
   ["a" "b" "c"]
   ["1" "2" "3"]
   ["i" "ii" "iii"]]

  (heading2 "Table with Header")
 ; table with 2 header rows, 3 regular content rows
  [:pdf-table
   {:header [[[:pdf-cell {:colspan 2}
               [:paragraph {:align :center :style :bold} "Customer Orders"]]]
             [[:phrase {:style :bold} "Name"]
              [:phrase {:style :bold} "Order Amount"]]]
    :spacing-before 10}
   [50 50]
   ["Joe" "$20.00"]
   ["Bob" "$7.50"]
   ["Mary" "$18.90"]]


  (chapter "multi-column")
  [:multi-column
   {:columns 3}

   [:list {:numbered true}
    (for [i (range 100)]
      (str "Linefff " i))]]

  (chapter "Paragraph fun!!")
  [:paragraph {:indent 50 :color [0 255 221]}
   [:spacer 2]
   [:phrase {:style :bold :size 18 :family :helvetica} "Hello Clojure!"]]

  [:spacer 2]
  [:anchor {:target "http://google.com"} "google"]

  [:spacer 2]
  [:anchor {:style {:size 15} :leading 20 :id "targetanchor"} "some anchor"]

  [:spacer 2]
  [:anchor {:target "#targetanchor"} "this anchor points to some anchor"]

  [:spacer 2]
  [:anchor [:phrase {:style :bold} "some anchor phrase"]]

  [:spacer 2]
  [:anchor "plain anchor"]]
 "doc.pdf")