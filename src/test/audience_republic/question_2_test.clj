(ns audience-republic.question-2-test
  (:require
    [audience-republic.question-2 :as q2]
    [audience-republic.metrics :as metrics]
    [audience-republic.example-data :as example]
    [clojure.test :refer :all]
    ))

(deftest not-enough-edges
  (is (= :too-sparse (:fail-type (q2/generate-graph 10 8)))))

(deftest just-enough-edges
  (is (nil? (:fail-type (q2/generate-graph 10 9)))))

(deftest edges-promise-kept-1
  (is (= 9 (metrics/edge-count (q2/generate-graph 10 9)))))

(deftest too-many-edges
  (is (= :too-dense (:fail-type (q2/generate-graph 10 46)))))

(deftest max-edges
  (is (nil? (:fail-type (q2/generate-graph 10 45)))))

(deftest edges-promise-kept-2
  (is (= 45 (metrics/edge-count (q2/generate-graph 10 45)))))

(deftest large-graph
  (let [g (q2/generate-graph 1000 1010)]
    (is (= 1000 (count g)))
    (is (= 1010 (metrics/edge-count g)))
    ))

(deftest extra-edges
  (let [num-extras 3
        g-before example/connected-graph
        g-after (q2/extra-edges-into-graph g-before num-extras)]
    (is (= (+ num-extras (metrics/edge-count g-before)) (metrics/edge-count g-after)))
    ))

(deftest dont-overfill-a-source
  (let [repeat-count 100
        edge-counts (->> (repeatedly #(metrics/edge-count (q2/generate-graph 10 45)))
                         (take repeat-count))]
    (is (= (repeat repeat-count 45) edge-counts))))

(deftest fill-a-graph
  (let [num-extras 1
        g-before (update example/full-graph :2 dissoc :1)
        g-after (q2/extra-edges-into-graph g-before num-extras)]
    (is (= (+ num-extras (metrics/edge-count g-before)) (metrics/edge-count g-after)))))

;; Testing same thing as dont-overfill-a-source, just more precisely
(deftest overfill-a-graph
  (let [num-extras 1
        g-before example/full-graph
        g-after (try
                  (q2/extra-edges-into-graph g-before num-extras)
                  (catch Throwable th :caught-exception))]
    (is (= :caught-exception g-after))))

(deftest space-available-nodes
  (let [g example/full-graph]
    (is (= (q2/spaces-available-nodes-f g) [:1 :3 :4]))))

(comment
  (run-tests)
  )