(ns ir-tools.api.wildcard-query
  "Functions to interpret user's queries with a wildcards. Wildcards that
are supported: * - any number of any symbols. Based on a permuterm index."
  (:require [clojure [string :as cstr]
                     [set :as cset]]
            [ir-tools.api [common :as common]
                          [query :as query]]))


;; ## Forward declarations

(declare get-ids-for-word get-words-for-wildcard rotate-wildcard
         filter-words)

;; ## Public API

(defn process-query
  "Given a word with a wildcard, index, doc ids and an auxiliary index find
documents where the given word is present."
  [query index aux-index doc-ids]
  (let [doc-ids-set (into #{} (vals doc-ids))
        words (get-words-for-wildcard query aux-index)
        words-sets (map #(vector % (get-ids-for-word % index)) words)]
    (into (sorted-map) words-sets)))

;; ## Private API

(defn- get-ids-for-word
  "Returns a set with ids for a given possible word."
  [word index]
  (get index word))

(defn- get-words-for-wildcard
  "Returns all words that match a given wildcard query."
  [query aux-index]
  (let [[rotated-term mid] (rotate-wildcard query)
        rotations (keys aux-index)
        ;; TODO somehow optimize this
        ;(take-while #(>= 0 (.compareTo % rotated-term)) (keys aux-index))
        possible-rots (filter #(.startsWith % rotated-term) rotations)
        possible-words (map #(get aux-index %) possible-rots)
        words (filter-words possible-words mid)]
    words))

(defn- rotate-wildcard
  "Rotates a query so that a wildcard is placed in the end. Also returns a
second part which contains any substrings that are inside the phrase (if
any)."
  [^String query]
  (let [wildcard-index (inc (.indexOf query "*"))
        prefix (apply str (take (dec wildcard-index) query))
        sufix (apply str (drop wildcard-index query))
        i (inc (.lastIndexOf sufix "*"))
        mid (cstr/split (apply str (take i sufix)) #"\*")
        sufix (apply str (drop i sufix))]
    [(str sufix "$" prefix) mid]))

;; TODO preserve sequence of parts (whats before, whats after) if more than
;; one element in parts
(defn- filter-words
  "Returns words that contain all parts"
  [words parts]
  (letfn [(reducer [x y] (filter #(<= 0 (.indexOf % y)) x))]
    (reduce reducer words parts)))
