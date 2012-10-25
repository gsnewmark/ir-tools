(ns ir-tools.api.fb2-tools
  "Functions to correctly parse FB2 files (including zoning)."
  (:require [clojure.xml :as cxml]))


;; ## Forward declarations

(declare process-one-level)

;; ## Public API

;; ## Private API

(defn- extract-info
  "Extracts required title info (author, title) and actual body
text."
  [fb2-xml]
  (let [fb2-map (cxml/parse fb2-xml)
        name (first
              (get-text-attr
               fb2-map [:description :title-info :author :first-name]))
        surname (first
                 (get-text-attr
                  fb2-map [:description :title-info :author :last-name]))
        author (str name " " surname)
        title (first
               (get-text-attr fb2-map [:description :title-info :book-title]))
        ;; TODO grab only text from body
        body (get-text-attr fb2-map [:body])]
    {:author author :title title :body body}))

(defn- get-text-attr
  "Gets a given part of a map produced by cxml/parse (elt) specified by a list
of tags (filter-seq)."
  [elt filter-seq]
  (let [path (conj (into [] (interleave (repeat :content) filter-seq))
                   :content)]
    (reduce process-one-level [elt] path)))

(defn- process-one-level
  "Retrieves a content of one 'level' of a map produced by a cxml/parse or
gets values of all element with specified tag."
  [elt tag]
  (if-not (= tag :content)
    (for [e elt :when (= tag (:tag e))] e)
    (apply concat (map tag elt))))
