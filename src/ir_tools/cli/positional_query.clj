(ns ir-tools.cli.positional-query
  "CLI for positional query API."
  (:require [ir-tools.api [positional-index :as index-api]
                          [positional-query :as query-api]]))


;; Public API

(defn run-query
  [& args]
  (let [index-file (first args)
        doc-ids-file (first (rest args))
        queries (rest (rest args))]
    (if-not (empty? queries)
      (let [[index doc-ids]
            (index-api/read-positional-index-doc-ids-from-file
             index-file doc-ids-file)
            query-results
            (map #(query-api/process-query % index doc-ids) queries)]
        (doseq [result query-results] (println result)))
      (println (str "You must provide at least 3 arguments - file with "
                    "a positional index dump, file with doc ids dump, and "
                    "one or more queries.")))))
