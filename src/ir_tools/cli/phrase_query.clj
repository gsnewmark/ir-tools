(ns ir-tools.cli.phrase-query
  "CLI for phrase query API."
  (:require [ir-tools.api [inverted-index :as index-api]
                          [query :as query-api]]))


;; Public API

(defn run-query
  [& args]
  (let [index-file (first args)
        biword-index-file (first (rest args))
        doc-ids-file (first (rest (rest args)))
        queries (rest (rest (rest args)))]
    (if-not (empty? queries)
      (let [[index doc-ids]
            (index-api/read-index-doc-ids-from-file index-file doc-ids-file)
            biword-index (index-api/read-index-from-file biword-index-file)
            query-results
            (map
             #(query-api/process-query % index biword-index doc-ids)
             queries)]
        (doseq [result query-results] (println result)))
      (println (str "You must provide at least 4 arguments - file with "
                    "an index dump, file with a biword index dump, file "
                    "with doc ids dump, and one or more queries.")))))
