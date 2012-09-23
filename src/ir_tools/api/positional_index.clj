(ns ir-tools.api.positional-index
  "Functions to generate a positional index."
  (:require [ir-tools.api.common :as common]))


;; ## Data Structures

;; Positional index - a word with it's positions in a documents.
;; Represented by a sorted map with a term as a key and another
;; map as a value. Value map has keys :freq with term's frequency
;; and keys that correspond to documents ids with sets of positions
;; as values.
;; Example entry: {"zip" {4 {:pos #{32654}, :count 1}, :freq 1}}
(def positional-index (atom (sorted-map)))

;; A map with pairs filename - docID.
(def doc-ids (atom {}))

;; ## Forward Declarations
(declare generate-term-position add-term-to-positional-index)

;; ## Public API

(defn fill-positional-index-from-file
  "Adds all pairs of words from a file with a given filename to an
biword index referenced by a b-ref, uses a map with document name -
document id pairs referenced by a d-ref (must be generated before adding
terms). Returns a map with current positional index (:results),
 document ids (:doc-ids), number of words (:tokens-count) and
file's size (:size)."
  [p-ref d-ref filename]
  (let [r (common/process-file generate-term-position filename)]
    (doseq [pair (:results r)]
      (add-term-to-positional-index p-ref d-ref pair filename))
    (assoc r :results @p-ref :doc-ids @d-ref)))

(defn add-term-to-positional-index
  "Adds a given term and it's positions to a positional
index (referenced by a p-ref) or updates its positions."
  [p-ref d-ref [term {:keys [count] :as v}] file]
  (let [ids @d-ref
        doc-id (get ids file)]
    (when-not (contains? @p-ref term)
      (swap! p-ref assoc term {:freq 0}))
    (swap! p-ref
           #(update-in
             (update-in % [term :freq] + count)
             [term] assoc doc-id v))))

;; ## Private API

(defn- generate-term-position
  "Generates a map with terms as keys and another map as value. Second map
has a key :pos - set of term's positions in a document and :count - how
many times the term is found in a document."
  [tokens-seq]
  (apply
   merge
   (map #(let [[k v] %] {k {:pos v :count (count v)}})
        (reduce (partial merge-with into) {}
                (map-indexed #(hash-map %2 (sorted-set %1)) tokens-seq)))))
