(ns ir-tools.cli.spimi-index-builder
  "CLI for SPIMI index buidlder."
  (:require [ir-tools.api.spimi-index-builder :as spimi-api]))


;; ## Public API

(defn generate-index
  [& args]
  (let [file-to-write (first args)
        file-to-write-ids (first (rest args))
        source (first (rest (rest args)))]
    (if (and file-to-write file-to-write-ids source)
      (time
       (spimi-api/process-collection source file-to-write file-to-write-ids))
      (println (str "You must provide 3 arguments - where to write an index, "
                    "where to write a document - id matrix and a "
                    "directory with source files.")))))
