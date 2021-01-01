(ns audience-republic.question-2-test
  (:require
    [audience-republic.question-2 :as q2]
    [audience-republic.metrics :as metrics]
    [audience-republic.example-data :as example]
    [clojure.test :refer :all]
    [audience-republic.graph :as gr]))

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

(defn full-up-node-hof?
  "(N-1) is maximum number of edges for any node. Checks if a node in a graph has reached this maximum"
  [g]
  (let [graph-size (-> g keys count)
        reversed-g (gr/reverse-graph g)]
    (fn [node]
      (let [pointers-at-node (-> reversed-g node keys)
            node-points-at (-> g node keys)
            arrows (concat pointers-at-node node-points-at)]
        (= (dec graph-size) (count arrows))))))

(defn open-nodes-f
  "This function is not necessary during running, only used by a test. Every node can have a maximum of N-1 arrows.
  Returns the nodes on a graph that can still accept one or more arrows, the 'open' nodes. The number of edges that
  the graph can accept will be the count of what is returned here minus one"
  [graph]
  (let [nodes (keys graph)
        spaces-available-f? (complement (full-up-node-hof? graph))]
    (filter spaces-available-f? nodes)))

(defn full-up-graph? [g]
  (-> g open-nodes-f empty?))

(deftest extra-edges
  (let [num-extras 3
        g-before example/nodes-graph
        spaces-available-nodes (open-nodes-f g-before)
        g-after (q2/extra-edges-into-graph g-before spaces-available-nodes num-extras)]
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
        spaces-available-nodes (open-nodes-f g-before)
        g-after (q2/extra-edges-into-graph g-before spaces-available-nodes num-extras)]
    (is (= (+ num-extras (metrics/edge-count g-before)) (metrics/edge-count g-after)))))

(deftest capacity-for-one-edge
  (let [g (update example/full-graph :2 dissoc :1)
        spaces-available-nodes (-> g open-nodes-f set)]
    (is (= spaces-available-nodes #{:1 :2}))))

;; Testing same thing as dont-overfill-a-source, just more precisely
(deftest overfill-a-graph
  (let [num-extras 1
        g-before example/full-graph
        g-after (try
                  (q2/extra-edges-into-graph g-before (open-nodes-f g-before) num-extras)
                  (catch Throwable th :caught-exception))]
    (is (= :caught-exception g-after))))

(deftest space-available-nodes
  (let [g example/full-graph]
    (is (empty? (open-nodes-f g)))))

(deftest full-graph-is-full
  (let [g example/full-graph]
    (is (full-up-graph? g))))

(comment
  (run-tests)
  )