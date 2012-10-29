(ns ir-tools.cli.clasterized-vsm
  "CLI for clasterized API."
  (:require [ir-tools.api [positional-index :as api]
                          [vector-space-model :as vsm-api]
                          [clasterization :as clasterization-api]
                          [common :as common-api]]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-clasterized-vsm
  [& args]
  (let [file-to-write     (first args)
        file-to-write-ids (first (rest args))
        sources-dir       (first (rest (rest args)))
        sources           (common-api/get-file-names sources-dir)]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (common-api/fill-doc-ids api/doc-ids sources)
         (let [sizes
               (doall
                (map
                 (partial
                  api/fill-positional-index-from-file
                  api/positional-index
                  api/doc-ids)
                 sources))
               total-size (apply + (map :size sizes))
               vsm (vsm-api/create-vector-space-model
                    vsm-api/vector-space-model
                    api/positional-index
                    api/doc-ids)
               clasterized-vsm (clasterization-api/create-clasterized-vsm
                                clasterization-api/clasterized-vsm
                                vsm-api/vector-space-model)]
           (common-api/write-doc-ids-to-file api/doc-ids file-to-write-ids)
           (common-api/write-collection-to-file clasterized-vsm file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total size of all files (in bytes):" total-size))))
      (println (str "You must provide 3 arguments - where to "
                    "write a clasterized index, where to write a "
                    "document - id matrix and directory with "
                    "sources of words.")))))
