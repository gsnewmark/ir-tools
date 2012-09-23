(ns ir-tools.api.inverted-index
  "Functions to generate an inverted index."
  (:require [clojure.string :as cstr]
            [ir-tools.api.common :as common]))


;; ## Data Structures

;; Inverted index. Represented by a sorted map - keys are words,
;; values - sorted set of document ids.
(def index (atom (sorted-map)))

;; A map with pairs filename - docID.
(def doc-ids (atom {}))

;; ## Forward declarations
(declare add-term-to-index index-entry-to-str deserealize-index-string)

;; ## Public API

(defn fill-index-from-file
  "Adds all words from a file with a given filename to an inverted index
referenced by a i-ref, uses a map with document name - document id pairs
referenced by a d-ref (must be generated before adding terms). Returns a
map with current inverted index (:results), document ids (:doc-ids),
number of words (:tokens-count) and file's size (:size)."
  [i-ref d-ref filename]
  (let [r (common/process-file filename)]
    (doseq [term (:results r)] (add-term-to-index i-ref d-ref term filename))
    (assoc r :results @i-ref :doc-ids @d-ref)))

(defn write-index-to-file
  "Writes an inverted index (from i-ref) to a file with a given
filename (using the document ids map referenced by d-ref) in a format
'term - docs', where docs is a list of document ids of documents where
the given term is present."
  [i-ref filename]
  (let [strings (map index-entry-to-str @i-ref)]
    (common/write-collection-to-file strings filename)))

(defn read-index-from-file
  "Given a file produced by write-index-to-file, restores initial index."
  [filename]
  (common/read-datastructure-from-file filename (sorted-map)
                                       deserealize-index-string))

(defn read-index-doc-ids-from-file
  "Given an index file produced by write-index-to-file and an doc ids file
produced by a write-doc-ids-to-file, restores these both datastructures."
  [index-file doc-ids-file]
  [(read-index-from-file index-file)
   (common/read-doc-ids-from-file doc-ids-file)])

(defn add-term-to-index
  "Adds a given term to an inverted index (referenced by a i-ref)
and adds a corresponding file's id to this term's set (using the
doc-ids map referenced by a d-ref)."
  [i-ref d-ref term file]
  (let [ids @d-ref
        doc-id (get ids file)]
    (when-not (contains? @i-ref term)
      (swap! i-ref assoc term (sorted-set)))
    (swap! i-ref update-in [term] conj doc-id)))

;; ## Private API

(defn- index-entry-to-str
  "Forms a string representation of an index entry."
  [entry]
  (let [[term id-set] entry
        s (apply
           str (interpose " " id-set))]
    (format "%s - %s" term s)))

(defn- deserealize-index-string
  "Deserializes an index row string from a file."
  [string]
  (let [[term indices] (cstr/split string #" \- ")
        indices (map #(Integer/parseInt %) (cstr/split indices #" "))]
    [term (apply sorted-set indices)]))
