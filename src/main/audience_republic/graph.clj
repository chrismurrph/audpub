(ns audience-republic.graph
  "Clojure specs used by graph orientated functions"
  (:require
    [clojure.spec.alpha :as s]))

;;
;; A node on a graph
;;
(s/def ::vertex keyword?)

;;
;; Arrows between the nodes are edges
;;
(s/def ::edge (s/tuple ::vertex ::vertex))
(s/def ::edges (s/coll-of ::edge))

;;
;; An edge is on a graph, whereas a pair is just [::vertex ::vertex]. The first a source and the second
;; a target, even if just potentially
;;
(s/def ::pair (s/tuple (s/nilable ::vertex) (s/nilable ::vertex)))

;;
;; The cost/penalty/distance for going from one vertex to another
;;
(s/def ::weight int?)

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

(s/def ::vertices (s/coll-of ::vertex))

;;
;; Important is a list, because we take from the beginning
;;
(s/def ::shuffled-vertices (s/coll-of ::vertex :kind list))
