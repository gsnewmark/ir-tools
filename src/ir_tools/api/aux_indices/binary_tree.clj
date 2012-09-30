(ns ir-tools.api.aux-indices.binary-tree
  "Functions to generate a binary tree of words."
  (:require [clojure.string :as cstr]
            [ir-tools.api.common :as common]))


;; ## Forward declarations

(declare)

;; ## Public API

;; Describes a basic binary tree node with a value, left and right sons as
;; well as a link to parent of a given node.
(defrecord TreeNode [key left right])

(defn create-tree
  "Constructor to create a binary tree with a given element as an initial
root."
  [root-element]
  (->TreeNode root-element nil nil))

(defn add-node
  "Adds a node at a proper position of a given binary tree."
  [node tree]
  (cond
   (nil? tree)
    (create-tree node)
   (< (.compareTo node (:key tree)) 0)
    (->TreeNode (:key tree) (add-node node (:left tree)) (:right tree))
   (> (.compareTo node (:key tree)) 0)
    (->TreeNode (:key tree) (:left tree) (add-node node (:right tree)))
   :else tree))

;; ## Private API
