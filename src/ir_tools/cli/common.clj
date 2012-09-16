(ns ir-tools.cli.common
  "Common CLI functions.")


(defn print-file-info
  "Prints file's name, number of words in it and its size."
  [filename {:keys [tokens-count size]}]
  (println "File:" filename)
  (println "Amount of words:" tokens-count)
  (println "Size of file (in bytes):" size)
  (println))
