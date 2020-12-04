(ns audience-republic.question-2-test
  (:require
    [audience-republic.question-2 :as q2]
    [audience-republic.metrics :as metrics]
    [clojure.test :refer :all]
    ))

(deftest not-duplicates
         (is (not (q2/dups-exist? '(:5 :6) [:3 :9]))))

(deftest are-duplicates
         (is (q2/dups-exist? '(:5 :9) [:3 :9])))

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
    (is (= 1000 (metrics/node-count g)))
    (is (= 1010 (metrics/edge-count g)))
    ))

(comment
  (run-tests)
  )



