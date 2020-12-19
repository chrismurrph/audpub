(ns layout.ham-3
  (:require
    [audience-republic.example-data :as example]
    [audience-republic.graph :as gr]
    [audience-republic.util :as util]
    [au.com.seasoft.general.dev :as dev]
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
            x (.getCoordinate v 1)
            y (.getCoordinate v 2)]
        (assoc m node-id [x y])))
    {}
    (.entrySet interop-coords)))

(defn graph->coords-1 [alignment-attempts g silent?]
  (let [interop-graph (graph->interop-graph g)
        interop-ham-1 (InteropHAM/create interop-graph 2)
        interop-ham-2 (InteropHAM/attemptToAlign interop-ham-1 alignment-attempts silent?)
        aligned? (.isAligned interop-ham-2)]
    (when aligned?
      (interop-coords->coords (.getCoordinates interop-ham-2)))))

(defn graph->coords-2 [alignment-attempts g]
  (let [n 20
        cs (conj (repeatedly n chan) (timeout 300))]
    (doseq [c cs]
      (go (when-let [coords (graph->coords-1 alignment-attempts g true)]
            (>! c coords))))
    (let [[v c] (alts!! cs)]
      v)))

(defn x-1 []
  (let [g example/unreachable-nodes-graph
        coords (graph->coords-2 200 g)]
    (when coords
      (dev/pp coords))))
