(ns audience-republic.graph
  "Clojure specs used by graph orientated functions, as well as graph orientated functions that are not metrics"
  (:require
    [audience-republic.example-data :as example]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [clojure.spec.alpha :as s]
    ))

;;
;; A node on a graph
;;
(s/def ::vertex keyword?)
(s/def ::vertices (s/coll-of ::vertex))

;;
;; The cost/penalty/distance for going from one vertex to another
;;
(s/def ::weight int?)

;;
;; Arrows between the nodes are edges
;;
(s/def ::edge (s/tuple ::vertex ::weight ::vertex))
(s/def ::edges (s/coll-of ::edge))

;;
;; An edge is on a graph, whereas a pair is just [::vertex ::vertex]. The first a source and the second
;; a target, even if just potentially
;;
(s/def ::pair (s/tuple (s/nilable ::vertex) (s/nilable ::vertex)))

;;
;; If a traversal across an edge goes against the edge's direction we record a false. Thus with no information
;; being lost, it will be possible to convert a ::graph-traversal back into a ::graph.
;;
(s/def ::same-as-arrow-direction? boolean?)

(s/def ::traversal (s/tuple ::same-as-arrow-direction? (s/nilable ::weight)))

;;
;; Traversal from one node to another
;;
(s/def ::edge-traversal (s/tuple ::vertex ::traversal ::vertex))

;;
;; A record of a journey through many vertices of a graph
;;
(s/def ::graph-traversal (s/coll-of ::edge-traversal))

;;
;; What does it cost to get to a vertex? (Presumably from a source vertex)
;;
(s/def ::target (s/tuple ::vertex ::weight))

(s/def ::targets (s/coll-of ::target))

;;
;; We say that each vertex of a graph has many targets even thou we don't directly use the target spec here
;; (i.e. a tuple (::target) is equivalent to a map-entry (what have here under s/map-of))
;;
(s/def ::graph (s/map-of ::vertex (s/map-of ::vertex ::weight)))

(s/def ::fail-type keyword?)
(s/def ::message string?)
(s/def ::failure (s/keys :req-un [::fail-type ::message]))

;;
;; If can't generate a graph then a failure is returned, which contains the reason.
;;
(s/def ::failure-or-graph (s/or :failure ::failure :graph ::graph))

;;
;; Important is a list, because we take from the beginning
;;
(s/def ::shuffled-vertices (s/coll-of ::vertex :kind list))

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
  [::graph => ::graph]
  (->> graph
       (mapcat reverse-graph-map-entry)
       (group-by first)
       (map (juxt first merge-grouped-by-entry-value))
       (map (fn [[k v]]
              [k (into {} v)]))
       (into {})))

(>defn nodes
  [g]
  [::graph => (s/coll-of ::vertex :kind set)]
  (-> g keys set))

(>defn pair-edges
  "All the edges on a graph, without weight"
  [g]
  [::graph => (s/coll-of ::pair :kind set)]
  (reduce
    (fn [acc [source-node v]]
      (into acc (map (fn [target-node]
                       [source-node target-node])
                     (keys v))))
    #{}
    g))

(>defn traversable-nodes
  [g node]
  [::graph ::vertex => (s/coll-of ::vertex :kind set)]
  (assert g ["No graph in traversable-nodes of" node])
  (set (or (-> g node keys) #{})))

#_(defn traversable-edges [g node]
  (->> (traversable-nodes g node)
       (map (fn [neighbour]
              [node neighbour]))
       set))

(>defn non-traversable-nodes
  [reversed-g node]
  [::graph ::vertex => (s/coll-of ::vertex :kind set)]
  (set (or (-> reversed-g node keys) #{})))

(defn adjacent-nodes
  [g reversed-g node]
  [::graph ::graph ::vertex => (s/coll-of ::vertex :kind set)]
  (set (concat (traversable-nodes g node) (non-traversable-nodes reversed-g node))))

(defn adjacent-edges
  [g reversed-g node]
  [::graph ::graph ::vertex => (s/coll-of ::edge :kind set)]
  (-> (concat (->> (traversable-nodes g node)
                   (map (fn [neighbour]
                          [node neighbour])))
              (->> (non-traversable-nodes reversed-g node)
                   (map (fn [neighbour]
                          [neighbour node]))))
      set))
