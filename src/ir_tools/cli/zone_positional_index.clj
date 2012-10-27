(ns ir-tools.cli.zone-positional-index
  "CLI for zone positional index API."
  (:require [ir-tools.api [inverted-index :as api]
                          [zone-positional-index :as zone-positional-api]
                          [common :as common-api]]
            [ir-tools.cli.common :as common]))


;; Public API

(defn build-zone-positional-index
  [& args]
  (let [file-to-write     (first args)
        file-to-write-ids (first (rest args))
        sources-dir       (first (rest (rest args)))
        sources           (common-api/get-file-names sources-dir)]
    (if-not (empty? sources)
      (time
       (do
         ;; Create ids for documents.
         (common-api/fill-doc-ids zone-positional-api/doc-ids sources)
         (let [;; Fill a zone positional index with words from all files and
               ;; get numbers of words in those files as well as their sizes
               ;; in bytes.
               sizes
               (doall
                (map
                 (partial
                  zone-positional-api/fill-zone-positional-index-from-file
                  zone-positional-api/zone-positional-index
                  zone-positional-api/doc-ids)
                 sources))
               total-size (apply + (map :size sizes))]
           (common-api/save-to-file
            zone-positional-api/doc-ids file-to-write-ids)
           (common-api/save-to-file
            zone-positional-api/zone-positional-index
            file-to-write)
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total size of all files (in bytes):" total-size))))
      (println (str "You must provide 3 arguments - where to "
                    "write a zone positional index, where to write a "
                    "document - id matrix and directory with "
                    "sources of words.")))))
