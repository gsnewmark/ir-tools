(ns ir-tools.cli.wildcard-query
  "CLI for wildcard query API."
  (:require [ir-tools.api [inverted-index :as index-api]
                          [wildcard-query :as query-api]]
            [ir-tools.api.aux-indices.permuterm-index :as permuterm-api]))


;; Public API

(defn run-query
  [& args]
  (let [index-file (first args)
        aux-index-file (first (rest args))
        doc-ids-file (first (rest (rest args)))
        queries (rest (rest (rest args)))]
    (if-not (empty? queries)
      (let [[index doc-ids]
            (index-api/read-index-doc-ids-from-file index-file doc-ids-file)
            permuterm-index (permuterm-api/read-permuterm-index-from-file
                             aux-index-file)
            query-results
            (map
             #(query-api/process-query % index permuterm-index)
             queries)]
        (doseq [result query-results] (println result)))
      (println (str "You must provide at least 4 arguments - file with "
                    "an index dump, file with a permuterm index dump, file "
                    "with doc ids dump, and one or more queries.")))))
