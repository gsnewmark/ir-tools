(ns ir-tools.cli.positional-index
  "CLI for positional index API."
  (:require [ir-tools.api [inverted-index :as api]
                          [positional-index :as positional-api]
                          [common :as common-api]]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-positional-index
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        sources (rest (rest args))]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (common-api/fill-doc-ids positional-api/doc-ids sources)
         (let [;; Fill a positional index with words from all files and get
               ;; numbers of words in those files as well as their sizes
               ;; in bytes.
               sizes (doall
                      (map
                       (partial positional-api/fill-positional-index-from-file
                                positional-api/positional-index
                                positional-api/doc-ids)
                       sources))
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (common-api/write-doc-ids-to-file
            positional-api/doc-ids file-to-write-ids)
           (positional-api/write-positional-index-to-file
            positional-api/positional-index
            file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in an index:"
                    (count @positional-api/positional-index)))))
      (println (str "You must provide at least 3 arguments - where to "
                    "write a positional index, where to write a "
                    "document - id matrix and sources of words.")))))
