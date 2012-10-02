(ns ir-tools.cli.wildcard-query
  "CLI for wildcard query API."
  (:require [ir-tools.api [inverted-index :as index-api]
                          [wildcard-query :as query-api]]
            [ir-tools.api.aux-indices [permuterm-index :as permuterm-api]
                                      [three-gram-index :as gram-api]]))


;; Public API

(defn run-query
  [& args]
  (let [mode (first args)
        index-file (first (rest args))
        aux-index-file (first (rest (rest args)))
        doc-ids-file (first (rest (rest (rest args))))
        queries (rest (rest (rest (rest args))))]
    (if-not (empty? queries)
      (let [[index doc-ids]
            (index-api/read-index-doc-ids-from-file index-file doc-ids-file)
            [process-query aux-index]
            (cond
             (= mode "p")
             [query-api/process-query-permuterm
              (permuterm-api/read-permuterm-index-from-file aux-index-file)]
             (= mode "g")
             [query-api/process-query-gram
              (gram-api/read-gram-index-from-file aux-index-file)])
            query-results
            (map
             #(process-query % index aux-index)
             queries)]
        (doseq [result query-results] (println result)))
      (println (str "You must provide at least 5 arguments - a mode "
                    "selector ('p' for permuterm, 'g' for 3-gram), file with "
                    "an index dump, file with a permuterm index dump, file "
                    "with doc ids dump, and one or more queries.")))))
