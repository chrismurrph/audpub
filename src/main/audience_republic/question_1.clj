(ns audience-republic.question-1
  "Graph walking functions that can read directed graphs that now have weights associated with the edges"
  (:require
    [audience-republic.graph :as gr]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    )
  (:import (clojure.lang PersistentQueue)))

(def G
  "Canonical form now using. Easier to update and order of the target nodes is not important so using a map
  represents that fact better. It is a ::gr/graph"
  {:1 (into {} [[:2 1] [:3 2]])
   :2 (into {} [[:4 4]])
   :3 (into {} [[:4 2]])
   :4 (into {} [])})

(defn reverse-graph-map-entry
  "one->many map-entries so mapcat against it. Note that these tuples cannot be used to create a map as
  there will be duplicate keys. Will need to group-by and merge-entry-value before `into {}`"
  [[source targets]]
  (assert ((some-fn vector? map?) targets) ["v s/be a vector of tuples or a map" source targets])
  (mapv (fn [tuple]
          (assert (= 2 (count tuple)) ["S/be target and weight" tuple])
          (let [[target weight] tuple]
            [target [[source weight]]]))
        targets))

(defn merge-grouped-by-entry-value
  "Needed as part of reversing a graph. See usage in test"
  [needs-merged]
  (->> needs-merged
       second
       (map second)
       (mapcat identity)
       vec))

(>defn reverse-graph
  "Changes the direction of the arrows, in terms of the drawing of a graph that the data structure represents.
  Used for being able to find a weight between two vertices across the wrong (target -> source) direction"
  [graph]
  [::gr/graph => ::gr/graph]
  (->> graph
       (mapcat reverse-graph-map-entry)
       (group-by first)
       (map (juxt first merge-grouped-by-entry-value))
       (map (fn [[k v]]
              [k (into {} v)]))
       (into {})))

(>defn pair->weight
  "Given two nodes (alias vertices), if they are connected and there is a weight then return that weight,
  otherwise nil. The [source-vertex target-vertex] pair needs to be connected in the right direction to produce a
  non-nil result"
  [graph [source-vertex target-vertex]]
  [::gr/graph ::gr/pair => (? int?)]
  (when graph
    (let [v (get graph source-vertex)]
      (when v
        (->> v
             (filter (comp #{target-vertex} first))
             first
             second)))))

(>defn lookup-weight-f
  "If the graph has a weight between the nodes of the pair it will be picked up, no matter whether the graph's arrow's
  direction is with the request or against it. 'against' means a false is returned in the first position of the
  ::gr/traversal.
  When there is no weight a nil is put"
  ([graph pair reversed-graph]
   [::gr/graph ::gr/pair ::gr/graph => (? ::gr/traversal)]
   (if-let [weight (pair->weight graph pair)]
     [true weight]
     (when-let [reversed-weight (pair->weight reversed-graph pair)]
       [false reversed-weight])))
  ([graph pair]
   [::gr/graph ::gr/pair => (? ::gr/traversal)]
   (lookup-weight-f graph pair nil)))

(>defn traverse-graph-dfs
  "Debuggable version of seq-graph, as it doesn't use lazy sequences"
  [g s]
  [::gr/graph ::gr/vertex => ::gr/graph-traversal]
  (let [first-node [s]
        reversed-graph (reverse-graph g)]
    (loop [last-vertex nil
           ongoing-traversal []
           explored-vertices #{(first first-node)}
           frontier [first-node]]
      (if (empty? frontier)
        (next ongoing-traversal)
        (let [[vertex] (peek frontier)
              edge-traversal [last-vertex (lookup-weight-f g [last-vertex vertex] reversed-graph) vertex]
              targets (get g vertex)
              neighbours-vertices (map first targets)]
          (recur
            vertex
            (conj ongoing-traversal edge-traversal)
            (into explored-vertices neighbours-vertices)
            (into (pop frontier) (remove #(explored-vertices (first %)) targets))))))))

(defn seq-graph-hof
  "Flattens a graph using an order of traversal that is determined by the data structure given: d.
  g is for the graph and s for the traversal starting point. To flatten a graph you *have* to give
  it an ordering strategy - either breadth first or depth first. A queue makes it breadth first. A vector depth first.
  Sometimes the traversal order goes against the 'weight arrows'. The returned data structure incorporates this fact"
  [d]
  (fn [g s]
    (let [reversed-graph (reverse-graph g)]
      (next ((fn rec-seq [last-vertex explored-vertices frontier]
               (lazy-seq
                 (when (seq frontier)
                   (let [[vertex] (peek frontier)
                         edge-traversal [last-vertex (lookup-weight-f g [last-vertex vertex] reversed-graph) vertex]
                         targets (get g vertex)
                         neighbours-vertices (map first targets)]
                     (cons edge-traversal
                           (rec-seq
                             vertex
                             (into explored-vertices neighbours-vertices)
                             (into (pop frontier) (remove #(explored-vertices (first %)) targets))))))))
             nil #{s} (conj d [s]))))))

(def seq-graph-dfs (seq-graph-hof []))
(def seq-graph-bfs (seq-graph-hof (PersistentQueue/EMPTY)))

(comment
  (traverse-graph-dfs G :1)
  (seq-graph-dfs G :1)
  (seq-graph-bfs G :1)
  )