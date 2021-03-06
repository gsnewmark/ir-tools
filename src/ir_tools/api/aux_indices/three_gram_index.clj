(ns ir-tools.api.aux-indices.three-gram-index
  "Functions to generate a 3-gram index of a given inverted index."
  (:require [clojure.string :as cstr]
            [ir-tools.api [inverted-index :as i-api]]))


;; ## Data Structures

;; Sorted map with a 3-grams of words as keys and a set of 'normal' terms
;; as a value.
(def three-gram-index (atom (sorted-map)))

;; ## Forward declarations

(declare generate-n-grams add-term-to-gram-index)

;; ## Public API

(defn inverted-index->gram-index
  "Add all words from a given inverted index to a given gram index."
  [i-ref g-ref]
  (let [terms (keys @i-ref)]
    (doseq [t terms]
      (add-term-to-gram-index g-ref t))))

(defn add-term-to-gram-index
  "Adds all 3-grams of a given term to a given 3-gram index."
  [g-ref term]
  (let [grams (generate-n-grams term 3)]
    (doseq [g grams]
      (when-not (contains? @g-ref g)
        (swap! g-ref assoc g (sorted-set)))
      (swap! g-ref update-in [g] conj term))))

(defn write-gram-index-to-file
  "Writes a n-gram index (from g-ref) to a file with a given
filename in a format 'gram - term'."
  [g-ref filename]
  (i-api/write-index-to-file g-ref filename))

(defn read-gram-index-from-file
  "Given a file with n-gram index, create this index in a memory."
  [filename]
  (i-api/read-index-from-file filename
                              #(let [[g terms] (cstr/split % #" - ")
                                     terms (cstr/split terms #" ")]
                                 [g (apply sorted-set terms)])))

(defn wrap-word
  "Adds a bounds symbol at words bounds."
  [word]
  (str "$" word "$"))

(defn generate-n-grams
  "Generate all n-grams for a given word and a given n. Optional third
argument specifies whether to add $ to beginning and end of a word."
  ([word n]
     (generate-n-grams word n true))
  ([word n add]
     (let [word (if add (wrap-word word) word)]
       (if (<= n (count word))
         (doall (map (partial apply str) (partition n 1 word)))
         word))))
