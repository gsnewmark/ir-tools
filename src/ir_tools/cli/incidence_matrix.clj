(ns ir-tools.cli.incidence-matrix
  "CLI for incidence matrix API."
  (:require [ir-tools.api.incidence-matrix :as api]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-incidence-matrix
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        sources (rest (rest args))]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (api/fill-doc-ids api/doc-ids sources)
         (let [;; Fill a matrix with words from all files and get numbers
               ;; of words in those files as well as their sizes in bytes.
               sizes (doall
                      (map
                       (partial api/fill-matrix-from-file
                                api/matrix api/doc-ids)
                       sources))
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           (api/write-doc-ids-to-file api/doc-ids file-to-write-ids)
           (api/write-matrix-to-file api/matrix api/doc-ids file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of terms in a matrix:" (count @api/matrix)))))
      (println (str "You must provide at least 3 arguments - where to "
                    "write an incidence matrix, where to write a "
                    "document - id matrix and sources of words.")))))
