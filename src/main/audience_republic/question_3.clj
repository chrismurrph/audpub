(ns audience-republic.question-3
  "Shortest path graph traversal algorithm (Dijkstra)"
  (:require
    [audience-republic.question-2 :as question-2]
    [audience-republic.example :as example]
    [audience-republic.metrics :as metrics]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [clojure.test :refer :all])
  )

(defn view-disconnected-graph []
  (dev/pp 30 example/disconnected-graph))

(defn view-connected-graph []
  (dev/pp 30 example/connected-graph-1))

(defn run-example-dijkstra []
  (metrics/dijkstra example/disconnected-graph :1))

(defn should-be-able-to-do []
  (let [random-graph (question-2/G 10 9)
        first-key (-> random-graph keys rand-nth)
        last-key (-> random-graph keys rand-nth)]
    (dev/pp random-graph)
    (dev/log-on "Easiest way to get from" first-key "to" last-key ":" (metrics/D random-graph first-key last-key))
    (dev/log-off "path diameter" (metrics/path-diameter random-graph))
    (dev/log-off "connected?" (metrics/don-t-use-connected? random-graph))
    ))

(comment
  (should-be-able-to-do)
  (shortest-path connected-graph :1 :12)
  (shortest-path disconnected-graph :1 :2)
  (shortest-path connected-graph :1 :2)
  )

