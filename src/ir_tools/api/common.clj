(ns ir-tools.api.common
  "Functions that a used by other API parts."
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]))


;; Forward declarations

(declare tokenize-string normalize-token process-string merger fold-into-vec
         write-collection-to-file)

;; String processing

(defn process-string-seq
  "Performs a given operation over all tokens from elements of a given
string sequence. Returns a map with operation's aggregated results (:results)
and number of normalized tokens in a given sequence (:tokens-count)."
  [op seq]
  ;; TODO use fold-into-vec and fold
  (let [processed-strings (map (partial process-string op) seq)
        result (reduce (partial merge-with merger) processed-strings)]
    result))

(defn process-string
  "Performs a given operation over all tokens of a given string.
Returns a map with an operation's result set (:results) as well as
overall number of normalized tokens in a string (:tokens-count)."
  [op ^String string]
  (let [tokens (tokenize-string string)]
    {:results (into (sorted-set) (map op tokens))
     :tokens-count (count tokens)}))

(defn tokenize-string
  "Retrieves all tokens (words) from a string."
  [^String s]
  (remove empty? (map normalize-token (cstr/split s #"\s+"))))

(defn normalize-token
  "Transforms a token (word) to a chosen canonical form."
  [^String token]
  (-> token
      ;; Removes all symbols except characters, digits, underscores,
      ;; apostrophes and dashes from a token.
      (cstr/replace #"[^\w-']" "")
      ;; Removes dashes, underscores, apostrophes at the beginning/end of
      ;; a token.
      (cstr/replace #"(^[\-_']+)|([\-_']+$)" "")
      (cstr/lower-case)))

;; Document ids

;; TODO maybe cast files to vec or change this completely
(defn fill-doc-ids
  "Fills a document ids map (references by a d-ref) with a given list
of files."
  [d-ref files]
  (doseq [i (range (count files))] (swap! d-ref assoc (nth files i) i)))

(defn write-doc-ids-to-file
  "Writes a document ids (from a d-ref) to a file with a given filename in a
format 'filename - file id'."
  [d-ref filename]
  (let [ids (map #(format "%s - %s" (first %) (second %)) @d-ref)]
    (write-collection-to-file ids filename)))

;; File interaction

(defn process-file
  "Process a sequence of strings extracted from a file with a given
filename using a given operation. Returns a map with result of
operation (:results), overall count of tokens (:tokens-count) and
a size of processed file in bytes (:size)."
  [op filename]
  (with-open [rdr (io/reader filename)]
    (assoc (process-string-seq op (line-seq rdr))
      :size (.length (io/file filename)))))

(defn write-collection-to-file
  "Writes a given data collection to a file with a given name. Each element
is written in a separate line."
  [col name]
  (spit name (cstr/join "\n" col)))

;; Private API

(defn- merger
  "Function for merge-with for process-string-seq - we have only
collections and number values, so it can be this simple."
  [v1 v2]
  (if (coll? v1)
    (into v1 v2)
    (+ v1 v2)))

(comment
  (defn- fold-into-vec
   "Provided a reducer, concatenate into a vector.
Note: same as (into [] coll), but parallel."
   [coll]
   (r/fold (r/monoid into vector) conj coll))
  )
