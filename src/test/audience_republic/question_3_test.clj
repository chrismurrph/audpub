(ns audience-republic.question-3-test
  (:require
    [audience-republic.metrics :as metrics]
    [audience-republic.example-data :as example]
    [clojure.test :refer :all]
    ))

(deftest test-update-costs
  (let [start-map {:weight ##Inf :path []}
        costs-m {:11 start-map :10 start-map :4 start-map :7 start-map :1 (assoc start-map :weight 0)
                 :8  start-map :9 start-map :2 start-map :5 start-map :3 start-map}
        unvisited #{:11 :10 :4 :7 :8 :9 :2 :5 :3}]
    (is (= 5 (-> (metrics/update-weights-and-path example/unreachable-nodes-graph costs-m unvisited :1 false) :3 :weight)))))

(deftest shortest-from-1-to-6
  (is (= [:3 :5 :7 :6] (metrics/shortest-path example/nodes-graph :1 :6))))

(deftest shortest-from-1-to-12
  (is (= [:3 :8 :9 :11 :12] (metrics/shortest-path example/nodes-graph :1 :12))))

(deftest shortest-from-1-to-2
  (is (= [:3 :4 :2] (metrics/shortest-path example/nodes-graph :1 :2))))

(deftest no-path-when-one-does-not-exist
  (is (nil? (metrics/shortest-path example/unreachable-nodes-graph :1 :2))))

(comment
  (run-tests)
  )
