(ns ir-tools.cli.aux-indices.permuterm-index
  "CLI for permuterm index API."
  (:require [ir-tools.api [inverted-index :as i-api]
                          [common :as common-api]]
            [ir-tools.api.aux-indices.permuterm-index :as api]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-permuterm-index
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        file-to-write-permuterms (first (rest (rest args)))
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
               ;; Fill a permuterm index from an inverted index.
               p-index (api/inverted-index->permuterm-index
                        i-api/index api/permuterm-index)
               permuterm-size (count @api/permuterm-index)
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (common-api/write-doc-ids-to-file i-api/doc-ids file-to-write-ids)
           (i-api/write-index-to-file i-api/index file-to-write)
           (api/write-permuterm-index-to-file api/permuterm-index
                                              file-to-write-permuterms)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in an index:" (count @i-api/index))
           (println "Amount of rotations of terms:" permuterm-size))))
      (println (str "You must provide at least 4 arguments - where to "
                    "write an inverted index, where to write a "
                    "document - id matrix, where to write a permuterm "
                    "index and sources of words.")))))
