(ns audience-republic.question-3
  "Shortest path graph traversal algorithm (Dijkstra)"
  (:require
    [audience-republic.question-2 :as question-2]
    [audience-republic.example-data :as example]
    [audience-republic.metrics :as metrics]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    ))

(defn view-unreachable-nodes-graph []
  (dev/pp 30 example/unreachable-nodes-graph))

(defn view-connected-graph []
  (dev/pp 30 example/connected-graph-1))

(defn run-example-dijkstra []
  (dev/pp (metrics/dijkstra example/unreachable-nodes-graph :1)))

(defn should-be-able-to-write []
  (let [random-graph (question-2/G 10 10)
        first-key (-> random-graph keys rand-nth)
        last-key (-> random-graph keys rand-nth)
        path (metrics/D random-graph first-key last-key)]
    (dev/pp 40 random-graph)
    (if path
      (dev/log-on "Easiest way to get from" first-key "to" last-key "is via path:" path)
      (dev/log-on "No possible path from" first-key "to" last-key))
    ))

(comment
  (should-be-able-to-write)
  (metrics/shortest-path example/connected-graph-1 :1 :12)
  (metrics/shortest-path example/unreachable-nodes-graph :1 :2)
  (metrics/shortest-path example/connected-graph-1 :1 :2)
  )

