(ns ir-tools.api.dictionary-compressor
  "A functions to compress a dictionary.
http://nlp.stanford.edu/IR-book/html/htmledition/blocked-storage-1.html"
  (:require [clojure.string :as cstr]))


;; ## Forward declarations

(declare block->string find-prefix)

;; ## Public API

(defn compress-dictionary
  "Compresses a given dictionary using blocked front coding."
  [block-size dictionary]
  (let [blocks (partition block-size block-size '() dictionary)]
    (apply str (map block->string blocks))))

;; ## Private API

(defn block->string
  "Transforms a block with words into a string representation."
  [block]
  (let [prefix (find-prefix block)
        ind (count prefix)
        block (map #(str (.substring % 0 ind) "|" (.substring % ind)) block)]
    (apply str (map #(str (count %) %) block))))

(defn find-prefix
  "Finds same prefix for a block of words."
  [block]
  (let [block (sort-by count block)]
    (loop [prefix (first block)]
      (if (every? #(.startsWith % prefix) block)
        prefix
        (recur (apply str (butlast prefix)))))))
