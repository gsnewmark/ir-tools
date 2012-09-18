(ns ir-tools.api.query
  "Functions to interpret user's queries. A query is defined as a sequence
of words (terms) connected by AND, OR and optionally negated by NOT. For
example, 'foo AND bar OR NOT baz' - finds all documents that have either
both 'foo' and 'bar' or don't have 'baz'."
  (:require [clojure [string :as cstr]
                     [set :as cset]]
            [ir-tools.api.common :as common]))


;; Forward declarations

(declare divide-query get-ids-for-word perform-operation)

;; Public API

;; TODO change loop-recur to something more idiomatic
(defn process-query
  "Given a query (string), an index and doc ids perform a query over
an index and return results (if any)."
  [query index doc-ids]
  (let [doc-ids-set (into #{} (vals doc-ids))
        [words operations] (divide-query query)
        words-sets (map #(get-ids-for-word % index doc-ids-set) words)]
    (loop [ws words-sets ops operations]
      (if (empty? ops)
        (first ws)
        (let [n (perform-operation (first ws) (first ops) (second ws))]
          (recur (cons n (drop 2 ws)) (rest ops)))))))

;; Private API

;; TODO maybe NOT must be a part of operation not word
(defn- divide-query
  "Given a query represented by a string divide it to a sequence of terms
and a sequence of connectors."
  [query]
  (let [r (map cstr/trim
               (cstr/split query #"(?<=AND|OR)|(?=AND|OR)"))
        words (take-nth 2 r)
        operations (map #(keyword (cstr/lower-case %)) (take-nth 2 (rest r)))]
    [words operations]))

(defn- get-ids-for-word
  "Returns a set with ids for a given word based on a map of all ids and
index."
  [word index doc-ids-set]
  (let [splitted-word (cstr/split word #" ")
        term (common/normalize-token (last splitted-word))
        maybe-not (first splitted-word)
        not (= "NOT" maybe-not)
        ids (get index term)]
    (if not (cset/difference doc-ids-set ids) ids)))

(defn- perform-operation
  "Performs a given operation (:or, :and) on a given sets of ids."
  [set1 op set2]
  (cond
   (= op :and) (cset/intersection set1 set2)
   (= op :or) (cset/union set1 set2)
   :else nil))
