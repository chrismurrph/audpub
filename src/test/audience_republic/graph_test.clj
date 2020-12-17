(ns audience-republic.graph-test
  (:require
    [audience-republic.graph :as gr]
    [clojure.test :refer :all]
    [audience-republic.example-data :as example]))

(deftest test-reverse-graph
  (is (= {:2 (into {} [[:1 1]])
          :3 (into {} [[:1 2]])
          :4 (into {} [[:2 4] [:3 2]])}
         (gr/reverse-graph example/simple-graph))))

(deftest test-reverse-graph-map-entry
  (is (= [[:2 [[:1 1]]]
          [:3 [[:1 2]]]] (gr/reverse-graph-map-entry [:1 [[:2 1] [:3 2]]]))))

(deftest test-join-up-value-part-of-entry
  (is (= [[:2 4] [:3 2]] (gr/merge-grouped-by-entry-value example/grouped-by-graph-map-entry))))

(deftest test-reverse-graph-back-again
  (is (= (dissoc example/simple-graph :4) (-> example/simple-graph gr/reverse-graph gr/reverse-graph))))

(deftest test-adjacent-nodes
  (let [g example/unreachable-nodes-graph
        reversed-g (gr/reverse-graph g)]
    (is (= #{:11 :10 :8} (gr/adjacent-nodes g reversed-g :9)))))

(deftest test-adjacent-edges
  (let [g example/unreachable-nodes-graph
        reversed-g (gr/reverse-graph g)]
    (is (= #{[:9 :11] [:9 :10] [:8 :9]} (gr/adjacent-edges g reversed-g :9)))))


(deftest test-non-traversable-nodes-1
  (let [g example/unreachable-nodes-graph
        reversed-g (gr/reverse-graph g)]
    (is (= #{:11 :10} (gr/non-traversable-nodes reversed-g :12)))))

(deftest test-non-traversable-nodes-2
  (let [g example/unreachable-nodes-graph
        reversed-g (gr/reverse-graph g)]
    (is (= #{:8} (gr/non-traversable-nodes reversed-g :9)))))

(deftest test-traversable-nodes-1
  (let [g example/unreachable-nodes-graph]
    (is (= #{} (gr/traversable-nodes g :12)))))

(deftest test-traversable-nodes-2
  (let [g example/unreachable-nodes-graph]
    (is (= #{:3} (gr/traversable-nodes g :1)))))

(comment
  (run-tests)
  )