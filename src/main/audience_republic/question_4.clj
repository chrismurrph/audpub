(ns audience-republic.question-4
  "Some metrics that make use of the Dijkstra shortest distance algorithm"
  (:require
    [audience-republic.example :as example]
    [audience-republic.metrics :as metrics]
    [clojure.test :refer :all]
    ))

(comment
  (metrics/eccentricity example/connected-graph-1 :1)
  (metrics/radius example/connected-graph-1)
  (metrics/diameter example/connected-graph-1)
  (metrics/shortest-path example/connected-graph-1 :1 :12)
  (metrics/longest-path example/connected-graph-1 :1 :12)
  )

