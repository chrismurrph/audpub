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

(comment
  (run-tests)
  )