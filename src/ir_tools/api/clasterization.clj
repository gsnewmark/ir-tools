(ns ir-tools.api.clasterization
  "Functions to clasterize positional index."
  (:require [ir-tools.api.vector-space-model :as vsm]))


;; ## Forward declarations

(declare find-max-in-map clasterize-vector-space-model)

;; ## Data Structures

(def clasterized-vsm (atom {}))

;; ## Public API

(defn create-clasterized-vsm
  "Stores a clasterized representation of a vector space model (in a atom
referenced by a cvsm-ref) stored is a vsm-ref."
  [cvsm-ref vsm-ref]
  (let [cvsm-doc (clasterize-vector-space-model @vsm-ref)
        cvsm     (into
                  {}
                  (map
                   (fn [a]
                     (let [[k v] a]
                       (vector k (into
                                  {}
                                  (map #(vector % (get @vsm-ref %)) v)))))
                   cvsm-doc))]
    (reset! cvsm-ref cvsm)))

(defn clasterize-vector-space-model
  "Clasterizes a given vector space model."
  [vsm]
  (let [leaders-length (Math/sqrt (count vsm))
        doc-ids        (shuffle (keys vsm))
        leaders        (take leaders-length doc-ids)
        followers      (drop leaders-length doc-ids)]
    ;(println "Leaders: " leaders)
    ;(println "Followers: " followers)
    (into
     (into {} (map #(vector % (list %)) leaders))
     (map
      #(let [[k v] %] [k (conj (map first v) k)])
      (group-by second
                (into
                 {}
                 (map (fn
                        [f]
                        {f (first
                            (find-max-in-map
                             (into
                              {}
                              (map #(hash-map
                                     %
                                     (vsm/similarity-quot [f (get vsm f)]
                                                          [% (get vsm %)]))
                                   leaders))))})
                      followers)))))))

;; ## Private API

(defn- find-max-in-map
  [map]
  (let [m (reduce max (vals map))]
    (first (filter #(= m (second %)) map))))
