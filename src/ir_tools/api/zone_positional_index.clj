(ns ir-tools.api.zone-positional-index
  "Functions to generate a positional index of a file with zones."
  (:require [ir-tools.api [common :as common]
                          [positional-index :as p-index]
                          [fb2-tools :as fb2-tools]]))


;; ## Forward Declarations

(declare)

;; ## Data Structures

;; Each zone is an element of a map - its name is key, and atom with actual
;; index is a value.
(def zone-positional-index (atom {}))

;; A map with pairs filename - docID.
(def doc-ids (atom {}))

;; ## Public API

(defn fill-zoned-positional-index-from-file
  "Adds all words from a file with a given filename along with position to a
zoned positional index referenced by a z-ref, uses a map with document name -
document id pairs referenced by a d-ref (must be generated before adding
terms). Returns a map with current zoned index (:results),
 document ids (:doc-ids), number of words (:tokens-count)."
  [z-ref d-ref filename]
  (let [r (com/process-file generate-term-position filename)]
    (doseq [pair (:results r)]
      (add-term-to-positional-index p-ref d-ref pair filename))
    (assoc r :results @p-ref :doc-ids @d-ref)))

;; ## Private API

(defn- add-zone
  "Adds a zone to a zone index."
  [i-ref zone-name]
  (swap! i-ref assoc zone-name (atom (sorted-map))))
