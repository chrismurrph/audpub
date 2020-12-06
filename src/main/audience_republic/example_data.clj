(ns audience-republic.example-data
  "Example data used in tests"
  (:require
    [audience-republic.graph :as gr]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    ))

(def unreachable-nodes-large-input
  ":2 and :4 can't be reached from any other nodes except :2 and :4. Despite this :2 and :4 are not on an island
  on their own"
  [[:2 10 :1] [:1 5 :3] [:4 4 :2] [:3 6 :5] [:3 2 :8] [:5 9 :6] [:5 3 :7] [:7 1 :6] [:7 11 :8]
   [:8 20 :9] [:9 17 :10] [:10 3 :12] [:11 10 :12] [:9 5 :11]])

(def large-input
  [[:2 10 :1] [:1 5 :3] [:3 7 :4] [:4 4 :2] [:3 6 :5] [:3 2 :8] [:5 9 :6] [:5 3 :7] [:7 1 :6] [:7 11 :8]
   [:8 20 :9] [:9 17 :10] [:10 3 :12] [:11 10 :12] [:9 5 :11] [:4 5 :7]])

(defn flat-graph->nodes
  [fg]
  (->> fg
       (mapcat (juxt first last))
       set))

(defn flat->graph
  "Takes a flat data structure as input and produces the nested structure."
  [flat-graph]
  (assert (-> flat-graph frequencies vals dev/probe-off (#(every? (partial = 1) %)))
          ["No duplicates allowed when generating graph"])
  (let [nodes (flat-graph->nodes flat-graph)
        starting-m (zipmap nodes (repeat []))]
    (->> (reduce
           (fn [m [source weight target]]
             (update m source conj [target weight]))
           starting-m
           flat-graph)
         (map (fn [[k v]]
                [k (into {} v)]))
         (into {}))))

(def unreachable-nodes-graph (flat->graph unreachable-nodes-large-input))
(def connected-graph-1 (flat->graph large-input))

(def needs-merged [:4 [[:4 [[:2 4]]] [:4 [[:3 2]]]]])