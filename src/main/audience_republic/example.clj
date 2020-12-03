(ns audience-republic.example
  (:require
    [au.com.seasoft.general.dev :as dev]))

(def disconnected-large-input
  ":2 and :4 can't be reached from any other nodes except :2 and :4"
  [[:2 10 :1] [:1 5 :3] [:4 4 :2] [:3 6 :5] [:3 2 :8] [:5 9 :6] [:5 3 :7] [:7 1 :6] [:7 11 :8]
   [:8 20 :9] [:9 17 :10] [:10 3 :12] [:11 10 :12] [:9 5 :11]])

(def large-input
  [[:2 10 :1] [:1 5 :3] [:3 7 :4] [:4 4 :2] [:3 6 :5] [:3 2 :8] [:5 9 :6] [:5 3 :7] [:7 1 :6] [:7 11 :8]
   [:8 20 :9] [:9 17 :10] [:10 3 :12] [:11 10 :12] [:9 5 :11] [:4 5 :7]])

(defn flat-graph->nodes [fg]
  (->> fg
       (mapcat (juxt first last))
       set))

(defn- flat->graph
  "Takes a flat data structure as input and produces the nested structure. Keeps strictly to the
  `requested-size` and `requested-num-edges` parameters by either generating extra nodes or leaving out
  some of the input. Warnings are currently given when this is necessary. It might be a good idea to not
  do anything and send an error to the logs rather than cut out input: there's a good argument to be made
  that if the user intended to cut out the data he would have done it himself!"
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

(def disconnected-graph (flat->graph disconnected-large-input))
(def connected-graph (flat->graph large-input))

