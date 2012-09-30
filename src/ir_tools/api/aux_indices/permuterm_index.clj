(ns ir-tools.api.aux-indices.permuterm-index
  "Functions to generate a permuterm index of a given inverted index."
  (:require [clojure.string :as cstr]
            [ir-tools.api [inverted-index :as i-api]]))


;; ## Data Structures

;; Sorted map with a permutation/rotation as a key and a 'normal' term as
;; a value.
(def permuterm-index (atom (sorted-map)))

;; ## Forward declarations

(declare word-rotations add-term-to-permuterm-index)

;; ## Public API

(defn inverted-index->permuterm-index
  "Add all words from a given inverted index to a given permuterm index."
  [i-ref p-ref]
  (let [terms (keys @i-ref)]
    (doseq [t terms]
      (add-term-to-permuterm-index p-ref t))))

(defn add-term-to-permuterm-index
  "Adds all rotations of a given term to a given permuterm index."
  [p-ref term]
  (let [rotations (word-rotations term)]
    (doseq [r rotations]
      (swap! permuterm-index assoc r term))))

(defn write-permuterm-index-to-file
  "Writes a permuterm index (from p-ref) to a file with a given
filename in a format 'rotation - term'."
  [p-ref filename]
  (i-api/write-index-to-file p-ref filename
                             #(let [[f s] %] (format "%s - %s" f s))))

(defn read-permuterm-index-from-file
  "Given a file with permuterm index, create this index in a memory."
  [filename]
  (i-api/read-index-from-file filename #(cstr/split % #" - ")))

;; ## Private API

(defn- word-rotations
  "Generate all rotations of a given word."
  [^String word]
  (let [word-length (count word)
        word (str word "$")]
    (doall (for [i (range (inc word-length))]
             (str (apply str (drop i word)) (apply str (take i word)))))))
