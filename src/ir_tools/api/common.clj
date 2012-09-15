(ns ir-tools.api.common
  "Functions that a used by other API parts."
  (:require [clojure.string :as cstr]
            [clojure.java.io :as io]))


;; Forward declarations

(declare tokenize-string normalize-token process-string)

;; Common API

(defn process-string-seq
  "Performs a given operation over all tokens from elements of a given
string sequence. Returns a list with operation's results and number of
normalized tokens in a given sequence."
  [op seq]
  (let [processed-strings (map (partial process-string op) seq)]
    [(map first processed-strings) (apply + (map second processed-strings))]))

(defn process-string
  "Performs a given operation over all tokens of a given string.
Returns a set with normalized tokens as well as overall number of
normalized tokens in a string."
  [op string]
  (let [tokens (tokenize-string string)]
    [(doall (map op tokens)) (count tokens)]))

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

;; File interaction

(defn process-file
  "Process a sequence of strings extracted from a file with a given
filename using a given operation. Returns a result of operation and
a size of processed file."
  [op filename]
  (with-open [rdr (io/reader filename)]
    [(process-string-seq op (line-seq rdr))
     (.length (io/file filename))]))

(defn write-collection-to-file
  "Writes a given data collection to a file with a given name. Each element
is written in a separate line."
  [col name]
  (spit name (cstr/join "\n" col)))


























