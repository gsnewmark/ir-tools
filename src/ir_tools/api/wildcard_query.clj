(ns ir-tools.api.wildcard-query
  "Functions to interpret user's queries with a wildcards. Wildcards that
are supported: * - any number of any symbols. Based on a permuterm index."
  (:require [clojure [string :as cstr]
                     [set :as cset]]
            [ir-tools.api [common :as common]
                          [query :as query]]))


;; ## Forward declarations

(declare get-ids-for-words get-words-for-wildcard rotate-wildcard
         filter-words)

;; ## Public API

(defn process-query
  "Given a word with a wildcard, index, doc ids and an auxiliary index find
documents where the given word is present."
  [query index doc-ids aux-index]
  (let [doc-ids-set (into #{} (vals doc-ids))
        words (get-words-for-wildcard query)
        words-sets (map
                       #(get-ids-for-word % index doc-ids-set aux-index)
                       words)]
    (reduce into #{} words-sets)))

;; ## Private API

(defn- get-ids-for-words
  "Returns a set with ids for a given possible words."
  [words index doc-ids-set aux-index]
  [])

(defn- get-words-for-wildcard
  "Returns all words that match a given wildcard query."
  [query aux-index]
  (let [[rotated-term mid] (rotate-wildcard query)
        rotations (keys aux-index)
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

;; TODO preserve sequence of parts (whats before, whats after)
(defn- filter-words
  "Returns words that contain all parts"
  [words parts]
  (letfn [(reducer [x y] (filter #(<= 0 (.indexOf % y)) x))]
    (reduce reducer words parts)))
