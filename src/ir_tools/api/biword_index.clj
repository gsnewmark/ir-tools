(ns ir-tools.api.biword-index
  "Functions to generate a biword index."
  (:require [ir-tools.api.common :as common]
            [ir-tools.api.inverted-index :as ind]))


;; ## Data Structures

;; Biword index - contains all pairs of consecutive words.
;; Represented by a sorted map - keys are pairs, values - sorted sets of
;; document ids of documents that contain this pair.
(def biword-index (atom (sorted-map)))

;; A map with pairs filename - docID.
(def doc-ids (atom {}))

;; ## Forward Declarations
(declare generate-pairs-set)

;; ## Public API

(defn fill-biword-index-from-file
  "Adds all pairs of words from a file with a given filename to an
biword index referenced by a b-ref, uses a map with document name -
document id pairs referenced by a d-ref (must be generated before adding
terms). Returns a map with current biword index (:results),
 document ids (:doc-ids), number of words (:tokens-count) and
file's size (:size)."
  [b-ref d-ref filename]
  (let [r (common/process-file generate-pairs-set filename)]
    (doseq [pair (:results r)]
      (ind/add-term-to-index b-ref d-ref pair filename))
    (assoc r :results @b-ref :doc-ids @d-ref)))

;; ## Private API

(defn generate-pairs-set
  "Generates a sorted set of pairs from a given seq."
  [tokens-seq]
  (into (sorted-set)
        (map #(apply str (interpose " " %)) (partition 2 1 tokens-seq))))
