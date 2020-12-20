(ns layout.ham
  (:require
    [audience-republic.example-data :as example]
    [audience-republic.graph :as gr]
    [audience-republic.util :as util]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [clojure.spec.alpha :as s]
    [clojure.core.async :as async :refer [<!! <! >! close! chan go go-loop alts!! timeout]]
    )
  (:import
    [au.com.seasoft.ham GenericGraph InteropNode InteropEdge InteropHAM]
    [com.syncleus.dann.math Vector]
    (java.util Map)))

(defn node->interop-node [node]
  (InteropNode. (util/kw->number node)))

(defn x-2 []
  (node->interop-node :1))

(defn edge->interop-edge [[source-node target-node]]
  (InteropEdge.
    (node->interop-node source-node) (node->interop-node target-node)))

(defn x-3 []
  (edge->interop-edge [:1 :3]))

(defn graph->interop-graph [graph]
  (let [result (GenericGraph/create)
        nodes (->> graph
                   gr/nodes
                   (map node->interop-node))
        edges (->> graph
                   gr/pair-edges
                   (map edge->interop-edge))]
    (doseq [node nodes]
      (.addNode result node))
    (doseq [edge edges]
      (.addEdge result edge))
    result))

(defn interop-coords->coords [^Map interop-coords]
  (reduce
    (fn [m map-entry]
      (let [^InteropNode interop-node (.getKey map-entry)
            node-id (-> (.getId interop-node) str keyword)
            ^Vector v (.getValue map-entry)
            _ (assert (= 2 (.getDimensions v)))
            x (.getCoordinate v 1)
            y (.getCoordinate v 2)]
        (assoc m node-id [x y])))
    {}
    (.entrySet interop-coords)))

(defn scale-coords [coords shift magnify]
  (->> coords
       (map (fn [[k v]]
              (let [[x y] v
                    new-x (* (+ x shift) magnify)
                    new-y (* (+ y shift) magnify)]
                [k [new-x new-y]])))
       (into {})))

(defn- -graph->coords [{:keys [alignment-attempts silent? shift magnify]
                        :or {alignment-attempts 200
                             silent? true
                             shift 10
                             magnify 20}} g]
  (let [interop-graph (graph->interop-graph g)
        interop-ham-1 (InteropHAM/create interop-graph 2)
        interop-ham-2 (InteropHAM/attemptToAlign interop-ham-1 alignment-attempts silent?)
        aligned? (.isAligned interop-ham-2)]
    (when aligned?
      (-> (.getCoordinates interop-ham-2)
          interop-coords->coords
          (scale-coords shift magnify)))))

(>defn graph->coords
  ([options g]
   [map? ::gr/graph => any?]
   (let [n 20
         cs (conj (repeatedly n chan) (timeout 500))]
     (doseq [c cs]
       (go (when-let [coords (-graph->coords options g)]
             (>! c coords))))
     (let [[v c] (alts!! cs)]
       v)))
  ([g]
   [::gr/graph => any?]
   (graph->coords {} g)))

(defn x-1 []
  (let [g example/unreachable-nodes-graph
        coords (graph->coords g)]
    (when coords
      (dev/pp coords))))
