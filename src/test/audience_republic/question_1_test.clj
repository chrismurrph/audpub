(ns audience-republic.question-1-test
  (:require
    [audience-republic.question-1 :as q1]
    [clojure.test :refer :all]))

(deftest test-get-from-data-structure
  (is (= {:2 1 :3 2} (get q1/G :1))))

(deftest test-reverse-graph-map-entry
  (is (= [[:2 [[:1 1]]]
          [:3 [[:1 2]]]] (q1/reverse-graph-map-entry [:1 [[:2 1] [:3 2]]]))))

(deftest test-join-up-value-part-of-entry
  (is (= [[:2 4] [:3 2]] (q1/merge-entry-value q1/needs-merged))))

(deftest test-reverse-graph
  (is (= {:2 (into {} [[:1 1]])
          :3 (into {} [[:1 2]])
          :4 (into {} [[:2 4] [:3 2]])}
         (q1/reverse-graph q1/G))))

(deftest test-reverse-graph-back-again
  (is (= (dissoc q1/G :4) (-> q1/G q1/reverse-graph q1/reverse-graph))))

(deftest test-looking-up-weights
  (let [f q1/lookup-weight-f
        g q1/G
        rev-g (q1/reverse-graph g)]
    (is
      (= [2 2 2 nil nil 4]
         [(f g [:1 :3] rev-g)
          (f g [:4 :3] rev-g)
          (f g [:3 :4] rev-g)
          (f g [:5 :3] rev-g)
          (f g [:3 nil] rev-g)
          (f g [:4 :2] rev-g)]))))

(deftest debuggable-depth-first
  (is (= '([:1 2 :3] [:3 2 :4] [:4 4 :2])
         (q1/traverse-graph-dfs q1/G :1 true))))

(deftest lazy-seq-depth-first
  (is (= '([:1 2 :3] [:3 2 :4] [:4 4 :2])
         (q1/seq-graph-dfs q1/G :1))))

(deftest lazy-seq-breadth-first
  (is (= '([:1 1 :2] [:2 nil :3] [:3 2 :4])
         (q1/seq-graph-bfs q1/G :1))))

(comment
  (run-tests)
  )




