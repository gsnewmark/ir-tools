(ns ir-tools.api.incidence-matrix
  "Functions to generate a term-document incidence matrix."
  (:require [ir-tools.api.common :as common])
  (:import [java.util BitSet]))


;; Term-document matrix. Represented by a sorted map - keys are words,
;; values - list of booleans (whether the word is in n-th document)
(def matrix (atom (sorted-map)))

;; A map with pairs filename - docID.
(def doc-ids (atom {}))

;; Forward declarations

(declare add-term-to-matrix matrix-row-to-str)

;; Public API

(defn fill-matrix-from-file
  "Adds all words from a file with a given filename to an incidence matrix
referenced by a m-ref, uses a map with document name - document id pairs
referenced by a d-ref (must be generated before adding terms). Returns a
map with current incidence matrix (:results), document ids (:doc-ids),
number of words (:tokens-count) and file's size (:size)."
  [m-ref d-ref filename]
  (let [r (common/process-file #(str %) filename)]
    (doseq [term (:results r)] (add-term-to-matrix m-ref d-ref term filename))
    (assoc r :results @m-ref :doc-ids @d-ref)))

(defn write-matrix-to-file
  "Writes an incidence matrix (from m-ref) to a file with a given
filename (using the document ids map referenced by d-ref) in a format
'term - 1/0-vector', where 1/0-vector is a vector with 1 on a positions that
correspond to a document ids where word is present and 0 - where word is not
present."
  [m-ref d-ref filename]
  (let [strings (map (partial matrix-row-to-str (count @d-ref)) @m-ref)]
    (common/write-collection-to-file strings filename)))

(defn add-term-to-matrix
  "Adds a given term to an incidence matrix (referenced by a m-ref)
and sets a corresponding file's index to true (using the doc-ids map
referenced by a d-ref)."
  [m-ref d-ref term file]
  (let [ids @d-ref
        doc-id (get ids file)]
    (when-not (contains? @m-ref term)
      ;; When a term not in a matrix, add it and assign its value to an
      ;; BitSet with length equal to a number of input files and all
      ;; elements of which are set to false.
      (swap! m-ref assoc term (BitSet. (count ids))))
    (.set (get @m-ref term) doc-id)))

;; Private API

(defn- matrix-row-to-str
  "Forms a string representation of a matrix row."
  [num-of-docs row]
  (let [[term bitset] row
        s (apply
           str (interpose " "
                          (for [i (range num-of-docs)]
                            (if (.get bitset i) 1 0))))]
    (format "%s - %s" term s)))
