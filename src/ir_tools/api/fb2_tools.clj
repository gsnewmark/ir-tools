(ns ir-tools.api.fb2-tools
  "Functions to correctly parse FB2 files (including zoning)."
  (:require [clojure.xml :as cxml]))


;; ## Forward declarations

(declare)

;; ## Public API

;; ## Private API

(defn- extract-info
  "Extracts required title info (author, title, annotation) and actual body
text."
  [fb2-xml]
  (let [fb2-map (cxml/parse fb2-xml)]
    fb2-map))

(defn- get-text-attr
  [elt filter-seq]
  (let [path (interleave (repeat :content) filter-seq)
        tag (last path)
        path (butlast path)]
    (for [e _ :when (= tag (:tag e))] e)))
