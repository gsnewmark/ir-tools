(ns ir-tools.api.common
  "Functions that a used by other API parts."
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]))


;; ## Forward declarations

(declare tokenize-string normalize-token process-string merger
         write-collection-to-file read-datastructure-from-file
         deserealize-doc-ids-string)

;; ## String processing

(defn process-string
  "Tokenizes a given string and transforms it using a given operation.
Returns a map with an operation's result (:results) as well as
overall number of normalized tokens in a string (:tokens-count)."
  ([^String string]
     (process-string #(into (sorted-set) %) string))
  ([op ^String string]
     (let [tokens (tokenize-string string)]
       {:results (doall (op tokens))
        :tokens-count (count tokens)})))

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

;; ## Document ids

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

(defn read-doc-ids-from-file
  "Given a file produced by write-doc-ids-to-file, restores initial doc ids."
  [filename]
  (read-datastructure-from-file filename {} deserealize-doc-ids-string))

;; ## File interaction

(defn process-file
  "Process a tokens from string extracted from a file with a given filename
using a given operation. If operations isn't given - creates a
sorted set of all tokens. Returns a map with result of
operation (:results), overall count of tokens (:tokens-count) and a size of
processed file in bytes (:size)."
  ([filename]
     (process-file #(into (sorted-set) %) filename))
  ([op filename]
     (let [string-contents (slurp filename)]
       (assoc (process-string op string-contents)
         :size (.length (io/file filename))))))

(defn read-datastructure-from-file
  "Reads a particular datastructure from a file, each element is formed
using the given operation."
  [filename ds op]
  (with-open [rdr (io/reader filename)]
    (into ds (map op (line-seq rdr)))))

(defn write-collection-to-file
  "Writes a given data collection to a file with a given name. Each element
is written in a separate line."
  [col name]
  (with-open [wrtr (io/writer name)]
    (doseq [s col]
      (.write wrtr (str s))
      (.newLine wrtr))))

;; ## Private API

(defn- merger
  "Function for merge-with for process-string-seq - we have only
collections and number values, so it can be this simple."
  [v1 v2]
  (if (coll? v1)
    (into v1 v2)
    (+ v1 v2)))

(defn- deserealize-doc-ids-string
  "Deserialize a doc ids row from a string."
  [string]
  (let [[file id] (cstr/split string #" \- ")]
    [file (Integer/parseInt id)]))
