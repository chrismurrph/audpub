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

(deftest test-update-costs
  (let [start-map {:weight ##Inf :path []}
        costs-m {:11 start-map :10 start-map :4 start-map :7 start-map :1 (assoc start-map :weight 0)
                 :8  start-map :9 start-map :2 start-map :5 start-map :3 start-map}
        unvisited #{:11 :10 :4 :7 :8 :9 :2 :5 :3}]
    (is (= 5 (-> (metrics/update-weights-and-path example/disconnected-graph costs-m unvisited :1 false) :3 :weight)))))

(defn run-example-dijkstra []
  (metrics/dijkstra example/disconnected-graph :1))

(deftest shortest-from-1-to-6
  (is (= [:3 :5 :7 :6] (metrics/shortest-path example/connected-graph-1 :1 :6))))

(deftest shortest-from-1-to-12
  (is (= [:3 :8 :9 :11 :12] (metrics/shortest-path example/connected-graph-1 :1 :12))))

(deftest shortest-from-1-to-2
  (is (= [:3 :4 :2] (metrics/shortest-path example/connected-graph-1 :1 :2))))

(deftest no-path-when-one-does-not-exist
  (is (some? (metrics/shortest-path example/connected-graph-1 :1 :2))))

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
  (run-tests)
  (shortest-path connected-graph :1 :12)
  (shortest-path disconnected-graph :1 :2)
  (shortest-path connected-graph :1 :2)
  )

