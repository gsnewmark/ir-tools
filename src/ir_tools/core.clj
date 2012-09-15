(ns ir-tools.core
  "A command line interface for the tools."
  (:require [ir-tools.api.dictionary-builder :as dict-api])
  (:gen-class))


;; Declarations
(declare print-file-info)

(defn -main
  "Receives two or more arguments - file name to write dictionary to as
first argument and a number of file names to read words from as rest of
the arguments."
  [& args]
  (let [file-to-write (first args)
        sources (rest args)]
    (if-not (empty? sources)
      (time
       (do
         (let [ ;; Fill a dictionary with words from all files and get numbers
               ;; of words in those files as well as their sizes in bytes.
               sizes (doall
                      (map
                       (partial dict-api/fill-dict-from-file
                                dict-api/dictionary)
                       sources))
               total-words (apply + (map #(second (first %)) sizes))
               total-size (apply + (map second sizes))]
           ;; Print some info about files.
           (dorun (map print-file-info sources sizes))
           ;; Write a dictionary to a file.
           (dict-api/write-dict-to-file dict-api/dictionary file-to-write)
           ;; Print current dictionary size, total words count and total size.
           (println (apply str (repeat 80 "-")))
           (println)
           (println "Total amount of words in all files:" total-words)
           (println "Total size of all files (in bytes):" total-size)
           ;; TODO write size
           (println "Amount of words in a dictionary:"))))
      (println (str "You must provide at least 2 arguments - where to "
                    "write a dictionary and source of words.")))))

(defn print-file-info
  "Prints file's name, number of words in it and its size."
  [filename [[_ words-number] size]]
  (println "File:" filename)
  (println "Amount of words:" words-number)
  (println "Size of file (in bytes):" size)
  (println))
