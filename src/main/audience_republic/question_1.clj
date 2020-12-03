(ns audience-republic.question-1
  (:require
    [au.com.seasoft.general.dev :as dev]
    [clojure.test :refer :all])
  (:import (clojure.lang PersistentQueue)))

(def G-1
  "Directed graph structure"
  {:1 [:2 :3]
   :2 [:4]
   :3 [:4]
   :4 []})

(def G-2a
  "Directed graph where every edge has a weight"
  {:1 ['(:2 1) '(:3 2)]
   :2 ['(:4 4)]
   :3 ['(:4 2)]
   :4 []})

(def G-2b
  "Same but with the idiomatic way of representing tuples"
  {:1 [[:2 1] [:3 2]]
   :2 [[:4 4]]
   :3 [[:4 2]]
   :4 []})

(def G-2c
  "Canonical form now using. Easier to update and order of the target nodes is not important so using a map
  represents that fact better"
  {:1 (into {} [[:2 1] [:3 2]])
   :2 (into {} [[:4 4]])
   :3 (into {} [[:4 2]])
   :4 (into {} [])})

(deftest test-get-from-data-structure-1
  (is (= [[:2 1] [:3 2]] (get G-2b :1))))

(deftest test-get-from-data-structure-2
  (is (= {:2 1 :3 2} (get G-2c :1))))

(defn- reverse-legacy-graph-map-entry
  "No longer used. Produces lots of map-entries so mapcat against it"
  [[source v]]
  (mapv (fn [[target weight :as tuple]]
          [target [`(~source ~weight)]]) v))

(defn- reverse-graph-map-entry
  "Produces lots of map-entries so mapcat against it. Note that these tuples cannot be used to create a map as
  there will be duplicate keys. Will need to group-by and merge-entry-value before `into {}`"
  [[source v]]
  (assert ((some-fn vector? map?) v) ["v s/be a vector of tuples or a map" source v])
  (mapv (fn [tuple]
          (assert (= 2 (count tuple)) ["S/be target and weight" tuple])
          (let [[target weight] tuple]
            [target [[source weight]]]))
        v))

(deftest test-reverse-legacy-graph
  (is (= [[:2 ['(:1 1)]]
          [:3 ['(:1 2)]]] (reverse-legacy-graph-map-entry [:1 ['(:2 1) '(:3 2)]]))))

(deftest test-reverse-graph-map-entry
  (is (= [[:2 [[:1 1]]]
          [:3 [[:1 2]]]] (reverse-graph-map-entry [:1 [[:2 1] [:3 2]]]))))

(def needs-merged-a [:4 [[:4 ['(:2 4)]] [:4 ['(:3 2)]]]])

(def needs-merged-b [:4 [[:4 [[:2 4]]] [:4 [[:3 2]]]]])

(defn- merge-entry-value
  "Needed as part of reversing a graph. See usage in test"
  [needs-merged]
  (->> needs-merged
       second
       (map second)
       (mapcat identity)
       vec))

(deftest test-join-up-value-part-of-entry
  (is (= [[:2 4] [:3 2]] (merge-entry-value needs-merged-b))))

(defn reverse-graph
  "Changes the direction of the arrows, in terms of the drawing of a graph that the data structure represents.
  Only used for being able to find a weight between two vertices across the wrong direction"
  [graph]
  (->> graph
       (mapcat reverse-graph-map-entry)
       (group-by first)
       (map (juxt first merge-entry-value))
       (map (fn [[k v]]
              [k (into {} v)]))
       (into {})))

(deftest test-reverse-graph-2
  (is (= {:2 (into {} [[:1 1]])
          :3 (into {} [[:1 2]])
          :4 (into {} [[:2 4] [:3 2]])}
         (reverse-graph G-2b))))

(deftest test-reverse-graph-back-again
  (is (= (dissoc G-2c :4) (-> G-2b reverse-graph reverse-graph))))

(defn pair->weight
  "Given two nodes (alias vertices), if they are connected and there is a weight then return that weight,
  otherwise nil. This is directional"
  [graph source-vertex target-vertex]
  (when graph
    (let [v (get graph source-vertex)]
      (when v
        (->> v
             (filter (comp #{target-vertex} first))
             first
             second)))))

(defn lookup-weight-f
  "If reversed-graph parameter is not nil then if you go from one node to another the weight will be used no matter which
  node it points to. This is not really consistent with the question which showed G as a directed
  graph and then said to show weights between the nodes: 'from the start to end vertex'. When there is no weight a
  nil is put in centre position"
  ([graph source-vertex target-vertex reversed-graph]
   (if-let [weight (pair->weight graph source-vertex target-vertex)]
     weight
     (when-let [reversed-weight (pair->weight reversed-graph source-vertex target-vertex)]
       reversed-weight)))
  ([graph source-vertex target-vertex]
   (lookup-weight-f graph source-vertex target-vertex nil)))

(deftest test-looking-up-weights
  (let [f lookup-weight-f
        g G-2b
        rev-g (reverse-graph g)]
    (is
      (= [2 2 2 nil nil 4]
         [(f g :1 :3 rev-g)
          (f g :4 :3 rev-g)
          (f g :3 :4 rev-g)
          (f g :5 :3 rev-g)
          (f g :3 nil rev-g)
          (f g :4 :2 rev-g)]))))

(defn traverse-graph-dfs
  "Debuggable version of seq-graph, as it doesn't use lazy sequences"
  [g s ignore-direction?]
  (let [first-node `(~s nil)
        first-node [s nil]
        reversed-graph (when ignore-direction? (reverse-graph g))
        ]
    (dev/log-off "g" g)
    (loop [last-kw nil
           vertices []
           explored #{(first first-node)}
           frontier [first-node]]
      (dev/log-off vertices explored frontier)
      (if (empty? frontier)
        (next vertices)
        (let [[kw weight :as v] (peek frontier)
              answer-node [last-kw (lookup-weight-f g last-kw kw reversed-graph) kw]
              neighbours-nodes (get g kw)
              neighbours-kws (map first neighbours-nodes)]
          (dev/log-off "kw neighbours" kw neighbours-nodes)
          (recur
            kw
            (conj vertices answer-node)
            (into explored neighbours-kws)
            (into (pop frontier) (remove #(explored (first %)) neighbours-nodes))))))))

(defn seq-graph-hof
  "Flattens a graph using an order of traversal that is determined by the data structure given: d.
  g is for the graph and s for the traversal starting point. To flatten a graph you *have* to give
  it an ordering strategy - either breadth first or depth first. Sometimes this order goes against
  the 'weight arrows'. When it does nil is put down for the weight. Weights are directional.
  To relax this restriction set ignore-direction? to true. I guess the 'take home' is that this is
  a traversal, *not* making a copy. If you think it is a pity to lose the weight information set
  ignore-direction? to true"
  [d ignore-direction?]
  (fn [g s]
    (let [reversed-graph (when ignore-direction? (reverse-graph g))]
      (next ((fn rec-seq [last-kw explored frontier]
               (lazy-seq
                 (if (empty? frontier)
                   nil
                   (let [[kw weight :as v] (peek frontier)
                         answer-node [last-kw (lookup-weight-f g last-kw kw reversed-graph) kw]
                         neighbours-nodes (get g kw)
                         neighbours-kws (map first neighbours-nodes)]
                     (cons answer-node (rec-seq
                                         kw
                                         (into explored neighbours-kws)
                                         (into (pop frontier) (remove #(explored (first %)) neighbours-nodes))))))))
             nil #{s} (conj d [s nil] #_`(~s nil)))))))

(def seq-graph-dfs (seq-graph-hof [] true))
(def seq-graph-bfs (seq-graph-hof (PersistentQueue/EMPTY) true))

(deftest debuggable-depth-first
  (is (= '([:1 2 :3] [:3 2 :4] [:4 4 :2])
         (traverse-graph-dfs G-2b :1 true))))

(deftest lazy-seq-depth-first
  (is (= '([:1 2 :3] [:3 2 :4] [:4 4 :2])
         (seq-graph-dfs G-2b :1))))

(deftest lazy-seq-breadth-first
  (is (= '([:1 1 :2] [:2 nil :3] [:3 2 :4])
         (seq-graph-bfs G-2b :1))))

(comment
  (run-tests)
  (traverse-graph-dfs G-2b :1)
  (seq-graph-dfs G-2b :1)
  (seq-graph-bfs G-2b :1)
  )