;;; https://github.com/mjul/docjure
;;; Experimented with this Aug 2020
(ns tools.excel.excel
  (:require
   [dk.ative.docjure.spreadsheet :as ss]))


;; Load a spreadsheet and read the first two columns from the
;; price list sheet:
(->> (ss/load-workbook "src/tools/excel/spreadsheet.xlsx")
     (ss/select-sheet "Test")
     (ss/select-columns {:A :name, :B :price}))


(ss/read-cell
 (->> (ss/load-workbook "src/tools/excel/spreadsheet.xlsx")
      (ss/select-sheet "Test")
      (ss/select-cell "A1")))

(->> (ss/load-workbook "src/tools/excel/spreadsheet.xlsx")
     (ss/select-sheet "Test")
     ss/row-seq
     (remove nil?)
     (map ss/cell-seq)
     (map #(map ss/read-cell %)))

(let [workbook (ss/load-workbook "src/tools/excel/spreadsheet.xlsx")
      sheet    (ss/select-sheet "Test" workbook)
      row      (ss/row-seq sheet)
      row2     (remove nil? row)
      cell1    (map ss/cell-seq row2)
      cell2    (map #(map ss/read-cell %) cell1)]
  cell2
  )

(defn load-spreadsheet [fname]
  (let [workbook (ss/load-workbook fname)
      ;sheet    (ss/.getSheetAt workbook 0)
      ; Above works but following is better
        sheet    (first (ss/sheet-seq workbook))
        row      (ss/row-seq sheet)
        row2     (remove nil? row)
        cells1    (map ss/cell-seq row2)
        cells2    (map #(map ss/read-cell %) cells1)]
    cells2))

(defn load-sheet [sheet]
  (println "SHEET" (ss/sheet-name sheet))
  (let [d1   (println "load-sheet 11")
        row      (ss/row-seq sheet)
        d2   (println "load-sheet 22")
        row2     (remove nil? row)
        d3   (println "load-sheet 33")
        cells1    (map ss/cell-seq row2)
        d4   (println "load-sheet 44")
        cells2    (map #(map ss/read-cell %) cells1)
        d5   (println "load-sheet 55")]
    cells2))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn load-spreadsheet2 [fname]
  (let [workbook (ss/load-workbook (expand-home fname))
        d1 (println "here1")
        sheets    (ss/sheet-seq workbook)
        d2 (println "here2" (ss/sheet-name (first sheets)))]
    (println "about to iterate through sheets")
    ;(load-sheet (first sheets))))
    (map load-sheet sheets)))

(load-spreadsheet2 "src/tools/excel/spreadsheet.xlsx")

(load-spreadsheet2 "/Users/mkersh/JupyterNotebooks/ClojureTests/src/tools/excel/spreadsheet.xlsx")



(load-spreadsheet2 "~/JupyterNotebooks/ClojureTests/src/tools/excel/spreadsheet.xlsx")


;; This is my RFP spreadsheet but it does not load because of:
;; https://github.com/mjul/docjure/issues/92
;; Guess I will have to go straight to using google docs API and read
;; spreadsheets from there. BUT that's a project for another day
(load-spreadsheet2 "~/Downloads/test2.xlsx")