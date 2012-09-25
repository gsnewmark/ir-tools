(ns ir-tools.api.query
  "Functions to interpret user's queries. A query is defined as a sequence
of words (terms) connected by AND, OR and optionally negated by NOT. For
example, 'foo AND bar OR NOT baz' - finds all documents that have either
both 'foo' and 'bar' or don't have 'baz'."
  (:require [clojure [string :as cstr]
                     [set :as cset]]
            [ir-tools.api.common :as common]))


;; ## Forward declarations

(declare divide-query get-ids-for-word perform-operation shrink-phrase)

;; ## Public API

;; TODO change loop-recur to something more idiomatic
(defn process-query
  "Given a query (string), an index and doc ids perform a query over
an index and return results (if any). Optionally you can pass a reference
to a biword index to make the phrases query possible."
  ([query index doc-ids]
     (process-query query index nil doc-ids))
  ([query index biword-index doc-ids]
     (let [doc-ids-set (into #{} (vals doc-ids))
           phrase? (if biword-index true false)
           max-words (if biword-index 2 1)
           [words operations] (divide-query query phrase? max-words)
           words-sets (map
                       #(get-ids-for-word % index biword-index doc-ids-set)
                       words)]
       (loop [ws words-sets ops operations]
         (if (empty? ops)
           (first ws)
           (let [n (perform-operation (first ws) (first ops) (second ws))]
             (recur (cons n (drop 2 ws)) (rest ops))))))))

;; TODO refactor this
;; ## Private API

;; TODO maybe NOT must be a part of operation not word
(defn- divide-query
  "Given a query represented by a string divide it to a sequence of terms
and a sequence of connectors."
  [query phrase? max-words]
  (let [r (flatten (map #(shrink-phrase (cstr/trim %) max-words)
                        (cstr/split query #"(?<=AND|OR)|(?=AND|OR)")))
        words (take-nth 2 r)
        operations (map #(keyword (cstr/lower-case %)) (take-nth 2 (rest r)))]
    [words operations]))

(defn- get-ids-for-word
  "Returns a set with ids for a given word based on a map of all ids and
index, biword-index (possibly nil)."
  [word index biword-index doc-ids-set]
  (let [splitted-word (cstr/split word #" ")
        maybe-not (first splitted-word)
        not (= "NOT" maybe-not)
        words (if not (rest splitted-word) splitted-word)
        [term phrase?] (if (empty? (rest words))
                         [(common/normalize-token (first splitted-word))
                          false]
                         [(apply
                           str
                           (interpose " "
                                      (map common/normalize-token words)))
                          true])
        ind (if (and phrase? biword-index) biword-index index)
        ids (get ind term #{})]
    (if not (cset/difference doc-ids-set ids) ids)))

(defn- perform-operation
  "Performs a given operation (:or, :and) on a given sets of ids."
  [set1 op set2]
  (cond
   (= op :and) (cset/intersection set1 set2)
   (= op :or) (cset/union set1 set2)
   :else nil))

(defn- shrink-phrase
  "Shrinks a phrase to a seq of phrases of max-words lengths interposed
with ANDs."
  [phrase max-words]
  (let [words (cstr/split phrase #" ")
        not (= "NOT" (first words))
        op (if not "OR" "AND")
        prefix (if not "NOT " "")
        words (if not (rest words) words)]
    (if (> (count words) max-words)
      (interpose op
                 (map #(apply str prefix (interpose " " %))
                      (partition max-words 1 words)))
      (list phrase))))
