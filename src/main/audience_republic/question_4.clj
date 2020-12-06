(ns audience-republic.question-4
  "Some metrics that make use of the Dijkstra shortest distance algorithm"
  (:require
    [audience-republic.example-data :as example]
    [audience-republic.metrics :as metrics]
    [audience-republic.question-2 :as question-2]
    [au.com.seasoft.general.dev :as dev]
    ))

(defn should-be-able-to-write []
  (let [random-graph (question-2/G 10 10)
        first-key (-> random-graph keys rand-nth)
        eccentricity (metrics/eccentricity random-graph first-key)
        radius (metrics/radius random-graph)
        diameter (metrics/diameter random-graph)]
    (dev/pp random-graph)
    (dev/log-on "The eccentricity of vertex" first-key "is" eccentricity)
    (dev/log-on "The radius of the graph is" radius)
    (dev/log-on "The diameter of the graph is" diameter)
    ))

(comment
  (should-be-able-to-write)
  (metrics/eccentricity example/connected-graph-1 :1)
  (metrics/radius example/connected-graph-1)
  (metrics/diameter example/connected-graph-1)
  (metrics/shortest-path example/connected-graph-1 :1 :12)
  (metrics/longest-path example/connected-graph-1 :1 :12)
  )

