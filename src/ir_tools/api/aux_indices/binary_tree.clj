(ns ir-tools.api.aux-indices.binary-tree
  "Functions to generate a binary tree of words."
  (:require [clojure.string :as cstr]
            [ir-tools.api.common :as common]))


;; ## Forward declarations

(declare)

;; ## Public API

;; Describes a basic binary tree node with a value, left and right sons as
;; well as a link to parent of a given node.
(defrecord TreeNode [key left right parent])

(defn create-tree
  "Constructor to create a binary tree with a given element as an initial
root."
  [root-element]
  (->TreeNode root-element nil nil nil))

;; ## Private API
