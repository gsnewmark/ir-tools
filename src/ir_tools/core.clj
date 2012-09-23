(ns ir-tools.core
  "A command line interface for the tools."
  (:require [ir-tools.cli [dictionary :as dict-cli]
                          [incidence-matrix :as imatrix-cli]
                          [inverted-index :as index-cli]
                          [query :as query-cli]])
  (:gen-class))


;; TODO add ability to choose a tool
(defn -main
  "Finds out what tool to call and calls it."
  [& args]
  (apply imatrix-cli/build-incidence-matrix args)
  ;(apply index-cli/build-inverted-index args)
  ;(apply query-cli/run-query args)
  )
