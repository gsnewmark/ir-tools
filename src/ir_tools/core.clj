(ns ir-tools.core
  "A command line interface for the tools."
  (:require [ir-tools.cli.dictionary :as dict-cli])
  (:gen-class))


(defn -main
  "Finds out what tool to call and calls it."
  [& args]
  (apply dict-cli/build-dictionary args))
