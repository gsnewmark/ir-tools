(ns ir-tools.cli.aux-indices.three-gram-index
  "CLI for 3-gram index API."
  (:require [ir-tools.api [inverted-index :as i-api]
                          [common :as common-api]]
            [ir-tools.api.aux-indices.three-gram-index :as api]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-three-gram-index
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        file-to-write-grams (first (rest (rest args)))
        sources (rest (rest (rest args)))]
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
               ;; Fill a 3-gram index from an inverted index.
               g-index (api/inverted-index->gram-index
                        i-api/index api/three-gram-index)
               gram-size (count @api/three-gram-index)
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (common-api/write-doc-ids-to-file i-api/doc-ids file-to-write-ids)
           (i-api/write-index-to-file i-api/index file-to-write)
           (api/write-gram-index-to-file api/three-gram-index
                                         file-to-write-grams)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in an index:" (count @i-api/index))
           (println "Amount of grams of terms:" gram-size))))
      (println (str "You must provide at least 4 arguments - where to "
                    "write an inverted index, where to write a "
                    "document - id matrix, where to write a 3-gram "
                    "index and sources of words.")))))
