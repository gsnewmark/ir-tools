(ns ir-tools.api.index-compressor
  "A functions to compress an inverted index.
http://nlp.stanford.edu/IR-book/html/htmledition/postings-file-compression-1.html
http://nlp.stanford.edu/IR-book/html/htmledition/gamma-codes-1.html"
  (:require [clojure.string :as cstr]
            [ir-tools.api.dictionary-compressor :as dcomp])
  (:import [java.util BitSet]))


;; ## Forward declarations

(declare get-gamma-code seq->gamma-codes get-unary-code compress-entry)

;; ## Public API

(defn compress-index
  "Compresses a given inverted index."
  [block-size index]
  (let [comp-dict (dcomp/compress-dictionary block-size (keys index))]
    (into (sorted-map) (map compress-entry index))))

;; ## Private API

(defn- compress-entry
  "Compresses a given index entry."
  [[word ids]]
  [word (seq->gamma-codes ids)])

(defn- seq->gamma-codes
  "Transforms a given sequence of numbers into a sequence of gamma code
using the first element as a 'root'."
  [num-seq]
  (let [first-elem (first num-seq)
        gaps       (map #(apply - (reverse %)) (partition 2 1 num-seq))]
    (map get-gamma-code (cons first-elem gaps))))

(defn- get-gamma-code
  "Returns a gamma code for a given number."
  [num]
  (let [binary        (Integer/toBinaryString num)
        offset        (apply str (rest binary))
        length-unary  (get-unary-code (count offset))
        gamma-str     (str length-unary offset)
        bit-set       (BitSet. (count gamma-str))
        gamma-str     (reverse gamma-str)]
    (dotimes [i (count gamma-str)]
      (when (= \1 (nth gamma-str i))
        (.set bit-set i)))
    bit-set))

(defn- get-unary-code
  "Returns an unary code for a given number."
  [num]
  (str (apply str (repeat num 1)) 0))
