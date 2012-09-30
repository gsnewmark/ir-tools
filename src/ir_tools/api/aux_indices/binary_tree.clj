(ns ir-tools.api.aux-indices.binary-tree
  "Functions to generate a binary tree of words."
  (:require [clojure.string :as cstr]
            [ir-tools.api.common :as common]))


;; ## Forward declarations

(declare create-tree add-node add-term-to-tree)

;; ## Data Structures

;; Describes a basic binary tree node with a value, left and right sons as
;; well as a link to parent of a given node.
(defrecord TreeNode [key left right])

;; Binary trees with terms and reverted terms.
(def terms-tree (atom nil))
(def inverted-terms-tree (atom nil))

;; ## Public API

(defn inverted-index->tree
  "Add all words from a given inverted index to a given binary tree and
binary tree with inverted terms."
  [i-ref t-ref it-ref]
  (let [terms (keys @i-ref)]
    (doseq [t terms]
      (add-term-to-tree t-ref it-ref t))))

(defn add-term-to-tree
  "Adds a term to a binary tree and inverted binary tree."
  [tree-ref itree-ref term]
  (swap! tree-ref add-node term)
  (swap! itree-ref add-node (cstr/reverse term)))

;; ## Private API

(defn- create-tree
  "Constructor to create a binary tree with a given element as an initial
root."
  [root-element]
  (->TreeNode root-element nil nil))

(defn- add-node
  "Adds a node at a proper position of a given binary tree."
  [tree node]
  (cond
   (nil? tree)
    (create-tree node)
   (< (.compareTo node (:key tree)) 0)
    (->TreeNode (:key tree) (add-node (:left tree) node) (:right tree))
   (> (.compareTo node (:key tree)) 0)
    (->TreeNode (:key tree) (:left tree) (add-node (:right tree) node))
   :else tree))
