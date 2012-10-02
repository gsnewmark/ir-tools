(ns ir-tools.api.wildcard-query
  "Functions to interpret user's queries with a wildcards. Wildcards that
are supported: * - any number of any symbols. Search is based on a permuterm
index or a 3-gram index."
  (:require [clojure [string :as cstr]
                     [set :as cset]]
            [ir-tools.api.aux-indices.three-gram-index :as gram-api]))


;; ## Forward declarations

(declare get-val-for-word get-words-for-wildcard-permuterm rotate-wildcard
         filter-words check-word get-query-grams get-words-for-wildcard-gram
         process-query)

;; ## Public API

(defn process-query-permuterm
  [query index aux-index]
  (process-query query index aux-index get-words-for-wildcard-permuterm))

(defn process-query-gram
  [query index aux-index]
  (process-query query index aux-index get-words-for-wildcard-gram))

(defn process-query
  "Given a word with a wildcard, index and an auxiliary index find
documents where the given word is present. Optional last argument specifies a
function that finds terms that correspond to a given query from an given
aux (permuterm, 3-gram) index."
  ([query index aux-index]
     (process-query query index aux-index get-words-for-wildcard-permuterm))
  ([query index aux-index find-words]
     (let [words (find-words query aux-index)
           words-sets (map #(vector % (get-val-for-word % index)) words)]
       (into (sorted-map) words-sets))))

;; ## Shared Private API

(defn- get-val-for-word
  "Returns a value for a given possible word from a given map."
  [word index]
  (get index word))

;; ## Private API for permuterm-based searching

(defn- get-words-for-wildcard-permuterm
  "Returns all words that match a given wildcard query based on a permuterm
index."
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
  (filter #(check-word parts %) words))

(defn- check-word
  "Checks whether the given word has all parts in a given order."
  [parts word]
  (if (reduce #(if %1
                 (let [p (.indexOf %1 %2)]
                   (if (<= 0 p)
                     (.substring %1 (+ p (count %2)))
                     false)
                   )
                 false)
              word parts)
    true
    false))

;; ## Private API for 3-gram-based search

(defn- get-words-for-wildcard-gram
  "Returns all words that match a given wildcard query based on a 3-gram
index."
  [query aux-index]
  (let [grams (get-query-grams query)
        words (apply cset/intersection
                     (map #(get-val-for-word % aux-index) grams))]
    words))

(defn- get-query-grams
  "Returns all 3-grams of a given string with wildcards."
  [^String s]
  ;; TODO correctly work with "red*" "*red"xs
  (let [word (if (.startsWith s "*") s (str "$" s))
        word (if (.endsWith word "*") word (str word "$"))
        parts (remove empty? (cstr/split word #"\*"))]
    (flatten (map #(gram-api/generate-n-grams % 3 false) parts))))
