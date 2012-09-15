(ns ir-tools.api.dictionary
  "Functions to generate a dictionary (sorted list of unique files) of
a text."
  (:require [ir-tools.api.common :as common]))


;; Dictionary with words. Represented with a sorted set - automatically
;; removes duplicates if any and sorts it.
(def dictionary (atom (sorted-set)))

;; Forward declarations

(declare add-word-to-dict)

;; Public API

(defn fill-dict-from-file
  "Adds all words from a file with a given filename to a dictionary
referenced by a given dict-ref. Returns a vector with number of words
in a given file as first element and its size in bytes as second."
  [dict-ref filename]
  (let [r (common/process-file #(str %) filename)]
    (doseq [w (:results r)] (add-word-to-dict dict-ref w))
    r))

(defn write-dict-to-file
  "Writes current contents of a dictionary referenced by a given reference
to a file with a given filename (previous contents of file is erased)."
  [dict-ref filename]
  (common/write-collection-to-file @dict-ref filename))

(defn add-word-to-dict
  "Adds a given word to a dictionary represented by a given ref."
  [dict-ref word]
  (swap! dict-ref conj word))

(defn dict-size
  "Returns current number of words in a dictionary referenced by a
given ref."
  [dict-ref]
  (count @dict-ref))
