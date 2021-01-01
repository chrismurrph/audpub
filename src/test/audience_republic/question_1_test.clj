(ns audience-republic.question-1-test
  (:require
    [audience-republic.question-1 :as q1]
    [audience-republic.example-data :as example]
    [clojure.test :refer :all]
    [audience-republic.graph :as gr]
    ))

(deftest test-get-from-data-structure
  (is (= {:2 1 :3 2} (get example/simple-graph :1))))

(deftest test-looking-up-weights
  (let [f q1/lookup-weight-f
        g example/simple-graph
        rev-g (gr/reverse-graph g)]
    (is
      (= [[true 2] [false 2] [true 2] nil nil [false 4]]
         [(f g [:1 :3] rev-g)
          (f g [:4 :3] rev-g)
          (f g [:3 :4] rev-g)
          (f g [:5 :3] rev-g)
          (f g [:3 nil] rev-g)
          (f g [:4 :2] rev-g)]))))

(deftest debuggable-depth-first
  (is (= '([:1 [true 2] :3] [:3 [true 2] :4] [:4 [false 4] :2])
         (q1/traverse-graph-dfs example/simple-graph :1))))

(deftest lazy-seq-depth-first
  (is (= '([:1 [true 2] :3] [:3 [true 2] :4] [:4 [false 4] :2])
         (q1/seq-graph-dfs example/simple-graph :1))))

(deftest lazy-seq-breadth-first
  (is (= '([:1 [true 1] :2] [:2 nil :3] [:3 [true 2] :4])
         (q1/seq-graph-bfs example/simple-graph :1))))

(defn x-1 []
  (q1/seq-graph-bfs example/not-connected-graph :1))

(comment
  (run-tests)
  )