(ns ir-tools.cli.inverted-index
  "CLI for inverted index API."
  (:require [ir-tools.api [inverted-index :as api]
                          [common :as common-api]]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-inverted-index
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        sources (rest (rest args))]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (common-api/fill-doc-ids api/doc-ids sources)
         (let [;; Fill an inverted index with words from all files and get
               ;; numbers of words in those files as well as their sizes
               ;; in bytes.
               sizes (doall
                      (map
                       (partial api/fill-index-from-file
                                api/index api/doc-ids)
                       sources))
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (common-api/write-doc-ids-to-file api/doc-ids file-to-write-ids)
           (api/write-index-to-file api/index file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in an index:" (count @api/index)))))
      (println (str "You must provide at least 3 arguments - where to "
                    "write an inverted index, where to write a "
                    "document - id matrix and sources of words.")))))
