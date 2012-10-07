(ns ir-tools.api.spimi-index-builder
  "Generates an (inverted) index for a collection of documents using
the SPIMI algorithm (http://nlp.stanford.edu/IR-book/html/htmledition/single-pass-in-memory-indexing-1.html)."
  (:require [ir-tools.api.inverted-index :as index-api]
            [ir-tools.api.common :as common-api])
  (:import [java.io File]))

;; ## Forward Declarations

(declare get-file-names get-available-memory has-memory?)

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
      (when-not (has-memory?)
        (spit (str @i) @index-api/index)
        (swap! i inc)
        (swap! index-api/index (fn [_] (sorted-map))))
      (index-api/fill-index-from-file index-api/index
                                      index-api/doc-ids file))
    (spit (str @i) @index-api/index)
    (swap i 1)))

;; ## Private API

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

;; TODO some better algorithm to find whether enough memory (not based
;; on a 0.15 constant percent, but maybe on actual file/index sizes).
(defn- has-memory?
  "Checks whether the program has enough memory to continue working."
  []
  (let [r (Runtime/getRuntime)
        total-mem (. r maxMemory)
        available-mem (get-available-memory)]
    (println total-mem available-mem)
    (<= 0.15 (double (/ available-mem total-mem)))))
