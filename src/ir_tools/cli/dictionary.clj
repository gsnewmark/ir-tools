(ns ir-tools.cli.dictionary
  "CLI for dictionary API."
  (:require [ir-tools.api.dictionary :as dict-api]
            [ir-tools.cli.common :as common]))


;; Declarations

(declare print-file-info)

;; Public API

(defn build-dictionary
  "Receives two or more arguments - file name to write dictionary to as
first argument and a number of file names to read words from as rest of
the arguments."
  [& args]
  (let [file-to-write (first args)
        sources (rest args)]
    (if-not (empty? sources)
      (time
       (do
         (let [;; Fill a dictionary with words from all files and get numbers
               ;; of words in those files as well as their sizes in bytes.
               sizes (doall
                      (map
                       (partial dict-api/fill-dict-from-file
                                dict-api/dictionary)
                       sources))
               total-words (apply + (map :tokens-count sizes))
               total-size (apply + (map :size sizes))]
           ;; Print some info about files.
           (dorun (map common/print-file-info sources sizes))
           ;; Write a dictionary to a file.
           (dict-api/write-dict-to-file dict-api/dictionary file-to-write)
           ;; Print current dictionary size, total words count and total size.
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           (println "Amount of words in a dictionary:"
                    (dict-api/dict-size dict-api/dictionary)))))
      (println (str "You must provide at least 2 arguments - where to "
                    "write a dictionary and source of words.")))))
