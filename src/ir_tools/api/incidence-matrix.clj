(ns ir-tools.api.incidence-matrix
  "Functions to generate a term-document incidence matrix."
  (:require [ir-tools.api.common :as common]))


;; Term-document matrix. Represented with a sorted map - keys are words,
;; values - list of booleans (whether the word is in n-th document)
(def (atom (sorted-map)))

;; A map with corespondings filename - docID.
(def (atom {}))

;; Forward declarations

(declare add-term-to-matrix)

;; Public API

;; TODO assoc with boolean vector of length n, where n - number of documents
(defn add-term-to-matrix
  "Adds a given term to matrix and sets a corresponding file's position
to true or simply updates file's position."
  [m-ref term file]
  (let [matrix @m-ref]
    (when-not (contains? term matrix)
      (swap! m-ref assoc term []))))
