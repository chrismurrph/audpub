(ns audience-republic.question-4
  "Some metrics that make use of the Dijkstra shortest distance algorithm"
  (:require
    [audience-republic.example :as example]
    [audience-republic.metrics :as metrics]
    [clojure.test :refer :all]
    ))

(deftest longest-from-1-to-2
  (is (= [:3 :4 :2] (metrics/longest-path example/connected-graph-1 :1 :2))))

(deftest longest-from-1-to-12
  (is (= [:3 :4 :7 :8 :9 :10 :12] (metrics/longest-path example/connected-graph-1 :1 :12))))

(deftest batch-longest-from-1-to-12
  (let [longest-path (metrics/batch-longest-path-hof example/connected-graph-1 :1)]
    (is (= [:3 :4 :7 :8 :9 :10 :12] (longest-path :12)))))

(comment
  (metrics/eccentricity example/connected-graph-1 :1)
  (metrics/radius example/connected-graph-1)
  (metrics/diameter example/connected-graph-1)
  (run-tests)
  (metrics/shortest-path example/connected-graph-1 :1 :12)
  (metrics/longest-path example/connected-graph-1 :1 :12)
  )

