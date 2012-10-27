(ns ir-tools.api.zone-positional-index
  "Functions to generate a positional index of a file with zones and query
against it."
  (:require [ir-tools.api [positional-index :as p-index]
                          [positional-query :as p-query]
                          [fb2-tools :as fb2-tools]]
            [clojure.string :as cstr]))


;; ## Forward declarations

(declare calc-score sort-results add-zone)

;; ## Data Structures

;; Each zone is an element of a map - its name is key, and atom with actual
;; index is a value.
(def zone-positional-index (atom {}))

;; A map with pairs filename - docID.
(def doc-ids (atom {}))

;; Zones' weights.
(def weights {:author 0.4 :title 0.4 :body 0.2})

;; ## Public API

(defn process-query
  "Given a string representation of a query perform it against the given
positional index and document ids list. Also, sorts results according to
their weight."
  [query index doc-ids]
  (let [r  (sort-results
            (reduce merge
                    (map
                     #(hash-map
                       (first %)
                       (p-query/process-query query (deref (second %))
                                              doc-ids))
                     index))
            weights)
        lf (fn [i] (for [[t n] doc-ids :when (= i n)] t))]
    (cstr/join "\n"
               (flatten (map #(vector (first %) (map lf (second %))) r)))))

(defn fill-zone-positional-index-from-file
  "Adds all words from a file with a given filename along with position to a
zoned positional index referenced by a z-ref, uses a map with document name -
document id pairs referenced by a d-ref (must be generated before adding
terms). Returns a map with current zoned index (:results),
document ids (:doc-ids), file's size (:size)."
  [z-ref d-ref filename]
  (let [r     (fb2-tools/process-file filename)
        zones (keys (dissoc r :size))]
    (doseq [z zones]
      (add-zone z-ref z)
      (p-index/fill-positional-index-from-string (z @z-ref) d-ref
                                                 (z r) filename))
    {:size (:size r) :doc-ids @d-ref :results @z-ref}))

;; ## Private API

(defn- add-zone
  "Adds a zone to a zone index."
  [i-ref zone-name]
  (when-not (contains? @i-ref zone-name)
    (swap! i-ref assoc zone-name (atom (sorted-map)))))

(defn- sort-results
  "Calculate a weighted zone score for a query and results."
  [results weights]
  (let [indices (reduce into (vals results))
        scores  (into (sorted-map)
                      (map #(hash-map % (calc-score results weights %))
                           indices))]
    (reverse (map #(let [[s l] %] [s (map first l)])
                  (into (sorted-map) (group-by second scores))))))

(defn calc-score
  "Calculates a weighted score for a given weights, query results and doc-id."
  [results weights ind]
  (apply + 0 (for [[zone inds] results :when (inds ind)] (zone weights))))
