(ns fx.example-layouts
  (:require
    [fx.layout-view :as layout]
    [audience-republic.example-data :as example]))

(defn x-3 []
  (let [g example/nodes-graph]
    (layout/show-graph g)))
