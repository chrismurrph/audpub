(ns audience-republic.graph
  "Clojure specs used by graph orientated functions"
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::vertex keyword?)

(s/def ::edge (s/tuple ::vertex ::vertex))

(s/def ::edges (s/coll-of ::edge))

(s/def ::weight int?)

(s/def ::weighted-edge (s/tuple ::vertex ::weight ::vertex))

(s/def ::target (s/tuple ::vertex ::weight))

(s/def ::targets (s/coll-of ::target))

;;
;; We say that each vertex of a graph has targets (i.e. a tuple is equivalent to a map-entry)
;;
(s/def ::graph (s/map-of ::vertex (s/map-of ::vertex ::weight)))

(s/def ::failure map?)

(s/def ::failure-or-graph (s/or ::failure ::graph))

(s/def ::vertices (s/coll-of ::vertex))

(s/def ::shuffled-vertices (s/coll-of ::vertex :kind list))

