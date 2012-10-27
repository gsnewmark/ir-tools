(ns ir-tools.core
  "A command line interface for the tools."
  (:require [ir-tools.cli [dictionary :as dict-cli]
                          [incidence-matrix :as imatrix-cli]
                          [inverted-index :as index-cli]
                          [positional-index :as positional-cli]
                          [biword-index :as biword-cli]
                          [query :as query-cli]
                          [phrase-query :as biword-query-cli]
                          [wildcard-query :as wildcard-query-cli]
                          [positional-query :as positional-query-cli]
                          [zone-positional-query :as zone-query-cli]
                          [spimi-index-builder :as spimi-cli]
                          [zone-positional-index :as zone-cli]]
            [ir-tools.cli.aux-indices
             [permuterm-index :as permuterm-index-cli]
             [three-gram-index :as three-gram-index-cli]
             [binary-tree :as binary-tree-cli]])
  (:gen-class))


;; TODO add ability to choose a tool
(defn -main
  "Finds out what tool to call and calls it."
  [& args]
  ;(apply imatrix-cli/build-incidence-matrix args)
  ;(apply index-cli/build-inverted-index args)
  ;(apply biword-cli/build-biword-index args)
  ;(apply positional-cli/build-positional-index args)
  ;(apply query-cli/run-query args)
  ;(apply biword-query-cli/run-query args)
  ;(apply positional-query-cli/run-query args)
  ;(apply permuterm-index-cli/build-permuterm-index args)
  ;(apply three-gram-index-cli/build-three-gram-index args)
  ;(apply binary-tree-cli/build-binary-tree args)
  ;(apply wildcard-query-cli/run-query args)
  ;(apply spimi-cli/generate-index args)
  ;(apply zone-cli/build-zone-positional-index args)
  (apply zone-query-cli/run-query args)
  )
