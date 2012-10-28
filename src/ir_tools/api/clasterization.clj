(ns ir-tools.api.clasterization
  "Functions to make a vector-space model based on a positional index and
clasterize index.")


;; ## Forward declarations

(declare calculate-weight calculate-length)

;; ## Data Structures

;; Vector space model - map with documents as a keys and another map as a
;; value - {:word weight}.
;; {:doc-id {:word weight}}
(def vector-space-model (atom {}))

;; ## Public API

(defn create-vector-space-model
  "Creates a vector space model (saves to an atom referenced by a v-ref) of a
positional index (referenced by a p-ref) of a given documents (referenced by a
d-ref)."
  [v-ref p-ref d-ref]
  (reset! v-ref (positional-index->vector-space @p-ref (count @d-ref))))

(defn positional-index->vector-space
  "Transforms a given positional index to a vector space model."
  [pos-index n]
  (into
   {}
   (map #(let [[key val] %] [key (assoc val :length (calculate-length %))])
        (reduce (partial merge-with merge)
                (map (partial calculate-weight n) pos-index)))))

;; ## Private API

(defn- calculate-weight
  "Calculates a weight for a words based on a entry from a positional index."
  [n index-entry]
  (let [[word stats] index-entry
        docs         (remove #(= % :freq) (keys stats))
        df           (count docs)
        idf          (Math/log10 (/ n df))]
    (into {} (map #(vector % {word (* (:count (get stats %)) idf)}) docs))))

(defn- calculate-length
  "Calculates a length of a document in a vector space model."
  [doc]
  (let [[doc-id words] doc
        weights (vals words)]
    (Math/sqrt (apply + (map #(* % %) weights)))))
