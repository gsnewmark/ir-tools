(ns ir-tools.cli.aux-indices.binary-tree
  "CLI for 3-gram index API."
  (:require [ir-tools.api [inverted-index :as i-api]
                          [common :as common-api]]
            [ir-tools.api.aux-indices.binary-tree :as api]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-binary-tree
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        sources (rest (rest args))]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (common-api/fill-doc-ids i-api/doc-ids sources)
         (let [;; Fill an inverted index with words from all files and get
               ;; numbers of words in those files as well as their sizes
               ;; in bytes.
               sizes (doall
                      (map
                       (partial i-api/fill-index-from-file
                                i-api/index i-api/doc-ids)
                       sources))
               ;; Fill a binary tree from an inverted index.
               tree (api/inverted-index->tree
                        i-api/index api/terms-tree api/inverted-terms-tree)
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (common-api/write-doc-ids-to-file i-api/doc-ids file-to-write-ids)
           (i-api/write-index-to-file i-api/index file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in an index:" (count @i-api/index)))))
      (println (str "You must provide at least 3 arguments - where to "
                    "write an inverted index, where to write a "
                    "document - id matrix and sources of words.")))))
