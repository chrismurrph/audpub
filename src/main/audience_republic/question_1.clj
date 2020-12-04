(ns audience-republic.question-1
  "Graph walking functions that can read directed graphs that now have weights associated with the edges"
  (:require
    [audience-republic.graph :as gr]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    )
  (:import (clojure.lang PersistentQueue)))

(def Ga
  "LEGACY: Directed graph structure"
  {:1 [:2 :3]
   :2 [:4]
   :3 [:4]
   :4 []})

(def Gb
  "LEGACY: Directed graph where every edge has a weight"
  {:1 ['(:2 1) '(:3 2)]
   :2 ['(:4 4)]
   :3 ['(:4 2)]
   :4 []})

(def Gc
  "LEGACY: Same but with the idiomatic way of representing tuples"
  {:1 [[:2 1] [:3 2]]
   :2 [[:4 4]]
   :3 [[:4 2]]
   :4 []})

(def G
  "Canonical form now using. Easier to update and order of the target nodes is not important so using a map
  represents that fact better. It is a ::gr/graph"
  {:1 (into {} [[:2 1] [:3 2]])
   :2 (into {} [[:4 4]])
   :3 (into {} [[:4 2]])
   :4 (into {} [])})

(defn reverse-graph-map-entry
  "Produces lots of map-entries so mapcat against it. Note that these tuples cannot be used to create a map as
  there will be duplicate keys. Will need to group-by and merge-entry-value before `into {}`"
  [[source v]]
  (assert ((some-fn vector? map?) v) ["v s/be a vector of tuples or a map" source v])
  (mapv (fn [tuple]
          (assert (= 2 (count tuple)) ["S/be target and weight" tuple])
          (let [[target weight] tuple]
            [target [[source weight]]]))
        v))

(def needs-merged [:4 [[:4 [[:2 4]]] [:4 [[:3 2]]]]])

(defn merge-entry-value
  "Needed as part of reversing a graph. See usage in test"
  [needs-merged]
  (->> needs-merged
       second
       (map second)
       (mapcat identity)
       vec))

(>defn reverse-graph
  "Changes the direction of the arrows, in terms of the drawing of a graph that the data structure represents.
  Only used for being able to find a weight between two vertices across the wrong direction"
  [graph]
  [::gr/graph => ::gr/graph]
  (->> graph
       (mapcat reverse-graph-map-entry)
       (group-by first)
       (map (juxt first merge-entry-value))
       (map (fn [[k v]]
              [k (into {} v)]))
       (into {})))

(>defn pair->weight
  "Given two nodes (alias vertices), if they are connected and there is a weight then return that weight,
  otherwise nil. This is directional"
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
  "If reversed-graph parameter is not nil then if you go from one node to another the weight will be used no matter which
  node it points to. This is not really consistent with the question which showed G as a directed
  graph and then said to show weights between the nodes: 'from the start to end vertex'. When there is no weight a
  nil is put in centre position"
  ([graph pair reversed-graph]
   [::gr/graph ::gr/pair ::gr/graph => (? ::gr/weight)]
   (if-let [weight (pair->weight graph pair)]
     weight
     (when-let [reversed-weight (pair->weight reversed-graph pair)]
       reversed-weight)))
  ([graph pair]
   [::gr/graph ::gr/pair => ::gr/weight]
   (lookup-weight-f graph pair nil)))

(>defn traverse-graph-dfs
  "Debuggable version of seq-graph, as it doesn't use lazy sequences"
  [g s ignore-direction?]
  [::gr/graph ::gr/vertex boolean? => ::gr/traversal]
  (let [first-node [s nil nil]
        reversed-graph (when ignore-direction? (reverse-graph g))]
    (dev/log-off "g" g)
    (loop [last-vertex nil
           ongoing-traversal []
           explored #{(first first-node)}
           frontier [first-node]]
      (dev/log-off ongoing-traversal explored frontier)
      (if (empty? frontier)
        (next ongoing-traversal)
        (let [[vertex weight :as v] (peek frontier)
              weighted-edge [last-vertex (lookup-weight-f g [last-vertex vertex] reversed-graph) vertex]
              targets (get g vertex)
              neighbours-vertices (map first targets)]
          (dev/log-off "vertex neighbours/targets" vertex targets)
          (recur
            vertex
            (conj ongoing-traversal weighted-edge)
            (into explored neighbours-vertices)
            (into (pop frontier) (remove #(explored (first %)) targets))))))))

(defn seq-graph-hof
  "Flattens a graph using an order of traversal that is determined by the data structure given: d.
  g is for the graph and s for the traversal starting point. To flatten a graph you *have* to give
  it an ordering strategy - either breadth first or depth first. A queue makes it breadth first. A vector depth first.
  Sometimes the traversal order goes against the 'weight arrows'. When it does by default nil is put down for the weight.
  Weights are directional. To relax this restriction set ignore-direction? to true. The 'take home' here is that this is
  a traversal, *not* making a copy. If you think it is a pity to lose the weight information set ignore-direction? to true"
  [d ignore-direction?]
  (fn [g s]
    (let [reversed-graph (when ignore-direction? (reverse-graph g))]
      (next ((fn rec-seq [last-vertex explored frontier]
               (lazy-seq
                 (if (empty? frontier)
                   nil
                   (let [[vertex weight :as v] (peek frontier)
                         weighted-edge [last-vertex (lookup-weight-f g [last-vertex vertex] reversed-graph) vertex]
                         targets (get g vertex)
                         neighbours-vertices (map first targets)]
                     (cons weighted-edge (rec-seq
                                         vertex
                                         (into explored neighbours-vertices)
                                         (into (pop frontier) (remove #(explored (first %)) targets))))))))
             nil #{s} (conj d [s nil nil]))))))

(def seq-graph-dfs (seq-graph-hof [] true))
(def seq-graph-bfs (seq-graph-hof (PersistentQueue/EMPTY) true))

(comment
  (traverse-graph-dfs G :1 true)
  (seq-graph-dfs G :1)
  (seq-graph-bfs G :1)
  )