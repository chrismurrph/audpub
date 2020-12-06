(ns audience-republic.question-4-test
  (:require
    [audience-republic.metrics :as metrics]
    [audience-republic.example-data :as example]
    [clojure.test :refer :all]))

(deftest longest-from-1-to-2
  (is (= [:3 :4 :2] (metrics/longest-path example/connected-graph :1 :2))))

(deftest longest-from-1-to-12
  (is (= [:3 :4 :7 :8 :9 :10 :12] (metrics/longest-path example/connected-graph :1 :12))))

(deftest batch-longest-from-1-to-12
  (let [longest-path (metrics/dijkstra-hof :path false example/connected-graph :1)]
    (is (= [:3 :4 :7 :8 :9 :10 :12] (longest-path :12)))))

;;
;; Note that these tests have not yet been independently verified. Just coded and got the answers.
;;

(deftest eccentricity-of-connected
  (is (= 44 (metrics/eccentricity example/connected-graph :1))))

(deftest radius-of-connected
  (is (= 3 (metrics/radius example/connected-graph))))

(deftest diameter-of-connected
  (is (= 54 (metrics/diameter example/connected-graph))))

(deftest eccentricity-of-disconnected
  (is (= 44 (metrics/eccentricity example/unreachable-nodes-graph :1))))

(deftest radius-of-disconnected
  (is (= 3 (metrics/radius example/unreachable-nodes-graph))))

(deftest diameter-of-disconnected
  (is (= 58 (metrics/diameter example/unreachable-nodes-graph))))

(comment
  (run-tests)
  )
