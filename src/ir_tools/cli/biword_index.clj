(ns ir-tools.cli.biword-index
  "CLI for biword index API."
  (:require [ir-tools.api [inverted-index :as api]
                          [biword-index :as biword-api]
                          [common :as common-api]]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-biword-index
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        sources (rest (rest args))]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (common-api/fill-doc-ids biword-api/doc-ids sources)
         (let [;; Fill a biword index with pairs from all files and get
               ;; numbers of words in those files as well as their sizes
               ;; in bytes.
               sizes (doall
                      (map
                       (partial biword-api/fill-biword-index-from-file
                                biword-api/biword-index biword-api/doc-ids)
                       sources))
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (common-api/write-doc-ids-to-file
            biword-api/doc-ids file-to-write-ids)
           (api/write-index-to-file biword-api/biword-index file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in an index:"
                    (count @biword-api/biword-index)))))
      (println (str "You must provide at least 3 arguments - where to "
                    "write a biword index, where to write a "
                    "document - id matrix and sources of words.")))))
