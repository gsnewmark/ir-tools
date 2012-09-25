(ns ir-tools.api.positional-query
  "Functions to perform queries against the positional index."
  (:require [clojure [string :as cstr]
                     [set :as cset]]
            [ir-tools.api [common :as common]
                          [query :as query]]))



;; ## Forward declarations

(declare get-ids-for-phrase get-ids-of-word)

;; ## Public API

(defn process-query
  "Given a string representation of a query perform it against the given
positional index and document ids list."
  [query index doc-ids]
  (let [doc-ids-set (into #{} (vals doc-ids))
        [phrases operations]
        (let [r (cstr/split query #"(?<=AND|OR)|(?=AND|OR)")]
          [(map cstr/trim (take-nth 2 r))
           (map #(keyword (cstr/lower-case %)) (take-nth 2 (rest r)))])
        phrases-sets (map #(get-ids-for-phrase % index doc-ids-set)
                          phrases)]
    (loop [ws phrases-sets ops operations]
         (if (empty? ops)
           (first ws)
           (let [n (query/perform-operation (first ws) (first ops)
                                            (second ws))]
             (recur (cons n (drop 2 ws)) (rest ops)))))))

;; ## Private API

(defn get-ids-for-phrase
  "Returns a set with document ids for a given phrase based on all ids and
positional index."
  [phrase index doc-ids-set]
  (let [splitted-words (cstr/split phrase #" ")
        [not? words] (if (= "NOT" (first splitted-words))
                       [true (rest splitted-words)]
                       [false splitted-words])
        words (map common/normalize-token words)
        ;; ids of all docs where all word of phrase are present (regardless of
        ;; position)
        initial-ids-pile (apply cset/intersection
                                (map (partial get-ids-of-word index) words))
        ;; TODO don't check for all consecutive words if one is already
        ;; in a 'incorrect' position
        ids (set
             (remove nil?
                     (for [doc-id initial-ids-pile]
                       (let [pos (map
                                  #(:pos (get (get index %) doc-id))
                                  words)
                             r (reduce
                                #(cset/intersection (set (map inc %1)) %2)
                                pos)]
                         (when-not (empty? r)
                           doc-id)))))]
    (if not? (cset/difference doc-ids-set ids) ids)))

(defn- get-ids-of-word
  "Returns a ids of a normalized word from an positional index."
  [index word]
  (->> (get index word {})
      keys
      (remove #{:freq})
      set))

