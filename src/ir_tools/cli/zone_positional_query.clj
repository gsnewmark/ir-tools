(ns ir-tools.cli.zone-positional-query
  "CLI for zone positional query API."
  (:require [ir-tools.api [zone-positional-index :as index-api]
                          [common :as c]]))


;; Public API

(defn run-query
  [& args]
  (let [index-file (first args)
        doc-ids-file (first (rest args))
        queries (rest (rest args))]
    (if-not (empty? queries)
      (let [index (c/load-from-file index-file)
            doc-ids (c/load-from-file doc-ids-file)
            query-results
            (map #(index-api/process-query % @index @doc-ids) queries)]
        (doseq [result query-results] (println result)))
      (println (str "You must provide at least 3 arguments - file with "
                    "a positional index dump, file with doc ids dump, and "
                    "one or more queries.")))))
