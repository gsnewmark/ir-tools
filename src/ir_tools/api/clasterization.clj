(ns ir-tools.api.clasterization
  "Functions to make a vector-space model based on a positional index and
clasterize index.")


;; ## Forward declarations

(declare calculate-weight)

;; ## Data Structures

;; Vector space model - map with documents as a keys and another map as a
;; value - {:word weight}.
;; {:doc-id {:word weight}}
(def vector-space-model (atom {}))

;; ## Public API

(defn positional-index->vector-space
  "Transforms a given positional index to a vector space model."
  [pos-index n]
  (reduce (partial merge-with merge)
          (map (partial calculate-weight n) pos-index)))

;; ## Private API

(defn- calculate-weight
  "Calculates a weight for a words based on a entry from a positional index."
  [n index-entry]
  (let [[word stats] index-entry
        docs         (remove #(= % :freq) (keys stats))
        df           (count docs)
        idf          (Math/log10 (/ n df))]
    (into {} (map #(vector % {word (* (:count (get stats %)) idf)}) docs))))
