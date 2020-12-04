(ns audience-republic.question-4-test
  (:require
    [audience-republic.metrics :as metrics]
    [clojure.test :refer :all]
    [audience-republic.example :as example]))

(deftest longest-from-1-to-2
  (is (= [:3 :4 :2] (metrics/longest-path example/connected-graph-1 :1 :2))))

(deftest longest-from-1-to-12
  (is (= [:3 :4 :7 :8 :9 :10 :12] (metrics/longest-path example/connected-graph-1 :1 :12))))

(deftest batch-longest-from-1-to-12
  (let [longest-path (metrics/batch-longest-path-hof example/connected-graph-1 :1)]
    (is (= [:3 :4 :7 :8 :9 :10 :12] (longest-path :12)))))

(comment
  (run-tests)
  )
