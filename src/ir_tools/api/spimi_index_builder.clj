(ns ir-tools.api.spimi-index-builder
  "Generates an (inverted) index for a collection of documents using
the SPIMI algorithm (http://nlp.stanford.edu/IR-book/html/htmledition/single-pass-in-memory-indexing-1.html)."
  (:require [clojure.java.io :as io]
            [ir-tools.api.inverted-index :as index-api]
            [ir-tools.api.common :as common-api])
  (:import [java.io File]))

;; ## Forward Declarations

(declare get-file-names get-available-memory has-memory?
         write-temp-index-to-file join-files!)

;; ## Public API

;; Builds current index until it can't fit in a memory, then writes it to a
;; file, cleans memory and starts building a new index. When all documents
;; are processed - merge all created files into one big index.

(defn process-collection
  "Adds all terms from a given collection (name of a directory with files)
to one inverted index stored in a file with name res."
  [col res]
  (let [filenames (get-file-names col)
        i (atom 1)]
    (common-api/fill-doc-ids index-api/doc-ids filenames)
    (doseq [file filenames]
      (when-not (has-memory? (.length (File. file)))
        (write-temp-index-to-file i))
      (index-api/fill-index-from-file index-api/index
                                      index-api/doc-ids file))
    (write-temp-index-to-file i)
    (join-files! @i res)
    nil))

;; ## Private API

(defn- join-files!
  "Join all temporary index files (with names 1, 2, 3...) into one big index
stored in a file with a name result."
  [next-num result]
  (let [temp-names (map str (range 1 next-num))
        readers (map io/reader temp-names)
        res (io/writer result)]
    (try
      (loop [lines (map #(vector % (.readLine %)) readers)]
        (let [;; Remove empty readers
              non-nil (remove (comp nil? second) lines)
              ;; Transform read strings to data structures
              evaled (map #(vector (first %) (read-string (second %)))
                          non-nil)]
          (when-not (empty? evaled)
            (let [;; Find 'minimal' word among current lines
                  min-word (first (sort (map (comp first second) evaled)))
                  ;; Find set with docIDs for a current 'minimal' word
                  to-write-val (reduce
                                into
                                ;; Extract sets with docIDs
                                (map (comp second second)
                                     ;; Find elements with 'minimal' word
                                     (filter #(= min-word (first (second %)))
                                             evaled)))
                  ;; Fully formed vector to write into file
                  to-write [min-word to-write-val]
                  recur-func (fn [[reader [word pos]]]
                               (if (= word min-word)
                                 [reader (.readLine reader)]
                                 [reader (str [word pos])]))]
              (.write res (str to-write))
              (.newLine res)
              (recur (map recur-func evaled))))))
      (finally
        (.flush res)
        (doseq [r (conj readers res)] (.close r))))))

(defn- write-temp-index-to-file
  [i]
  (common-api/write-collection-to-file @index-api/index (str @i))
  (swap! i inc)
  (swap! index-api/index (fn [_] (sorted-map))))

;; TODO make recursive
(defn- get-file-names
  "Retrieves names of all files in a given directory (not recursive)."
  [dir]
  (remove nil?
          (map #(when-not (.isDirectory %) (.getAbsolutePath %))
               (.listFiles (File. dir)))))

(defn- get-available-memory
  "Returns an amount of available free memory."
  []
  (let [r (Runtime/getRuntime)
        free-mem (. r freeMemory)
        max-mem (. r maxMemory)
        alloc-mem (. r totalMemory)]
    (+ free-mem (- max-mem alloc-mem))))

(defn- has-memory?
  "Checks whether the program has enough memory to continue working.
Arguments specifies how much memory will be used on a next step and
what percent of a memory should remain free."
  ([memory-to-use]
     (has-memory? memory-to-use 0.1))
  ([memory-to-use free-percent]
     (let [r (Runtime/getRuntime)
           total-mem (. r maxMemory)
           available-mem (get-available-memory)]
       ;(println available-mem)
       (and (or (> available-mem (* 2 memory-to-use))
                (> memory-to-use total-mem))
            (>= (/ available-mem total-mem) free-percent)))))
