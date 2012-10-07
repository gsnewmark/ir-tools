(ns ir-tools.api.spimi-index-builder
  "Generates an (inverted) index for a collection of documents using
the SPIMI algorithm (http://nlp.stanford.edu/IR-book/html/htmledition/single-pass-in-memory-indexing-1.html)."
  (:require [clojure.java.io :as io]
            [ir-tools.api.inverted-index :as index-api]
            [ir-tools.api.common :as common-api])
  (:import [java.io File]))

;; ## Forward Declarations

(declare get-file-names get-available-memory has-memory?
         write-temp-index-to-file)

;; ## Public API

;; Builds current index until it can't fit in a memory, then writes it to a
;; file, cleans memory and starts building a new index. When all documents
;; are processed - merge all created files into one big index.

(def i (atom 1))
(defn process-collection
  "Adds all terms from a given collection (name of a directory with files)
to one inverted index."
  [col]
  (let [filenames (get-file-names col)]
    (common-api/fill-doc-ids index-api/doc-ids filenames)
    (doseq [file filenames]
      (when-not (has-memory? (.length (File. file)))
        (write-temp-index-to-file))
      (index-api/fill-index-from-file index-api/index
                                      index-api/doc-ids file))
    (write-temp-index-to-file)
    (swap! i (fn [_] 1))
    nil))

;; ## Private API

(defn- join-files
  "Join all temporary index files (with names 1, 2, 3...) into one big index."
  [last-name]
)

(defn- write-temp-index-to-file
  []
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
