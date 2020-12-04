(ns audience-republic.example
  "Example data used in tests"
  (:require
    [au.com.seasoft.general.dev :as dev]))

(def disconnected-large-input
  ":2 and :4 can't be reached from any other nodes except :2 and :4. Despite this :2 and :4 are not on an island
  on their own"
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

(def disconnected-graph (flat->graph disconnected-large-input))
(def connected-graph-1 (flat->graph large-input))

;;
;; Apply either of these and it becomes disconnected:
;; ([:9 :4] [:7 :10] [:8 :5] [:10 :2] [:6 :7] [:2 :9] [:1 :3] [:5 :6] [:4 :1] [:3 :8])
;; ([:4 :2] [:9 :4] [:1 :3] [:2 :7] [:5 :8] [:10 :6] [:7 :9] [:3 :5] [:6 :10] [:8 :1])
;; I manually tried the first on paper - why no longer 'connected'?
;; Because path depends on weights. We really needed a much looser definition of 'connected'
;; No longer doing this check, as don't need, as are doing something crude and simple to make
;; sure graphs we generate are connected.
;;
(def connected-graph-2 {:10 {:4 6},
                        :4  {:7 19},
                        :7  {:1 2 :10 20},
                        :1  {:8 7},
                        :8  {:9 9 :5 30},
                        :9  {:2 2 :4 20},
                        :2  {:5 13},
                        :5  {:3 12},
                        :3  {:6 10},
                        :6  {}})
