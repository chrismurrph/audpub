(ns audience-republic.metrics
  "Graph traversal (see also question-1)"
  (:require
    [audience-republic.graph :as gr]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]))

(def infinity Double/POSITIVE_INFINITY)

(defn update-weights-and-path
  "When going from v to u if this is true:
  dist (v) + weight(u,v) < dist (u)
  We need to give dist (u) the lower value"
  [g node-details-m unvisited current-node-k maximise-weight?]
  (let [better-than (if maximise-weight? > <)
        {:keys [weight path] :as current-node-details} (get node-details-m current-node-k)
        current-node-targets-m (get g current-node-k)]
    (assert weight ["Need see a map containing :weight at node" current-node-details])
    (reduce-kv
      (fn [acc-details-m target-node-k current-to-weight]
        (if (unvisited target-node-k)
          (let [potential-new-weight (+ weight current-to-weight)
                curr-target-weight (get-in acc-details-m [target-node-k :weight])]
            (if (better-than potential-new-weight curr-target-weight)
              (let [new-path (conj (or path []) target-node-k)]
                (assoc acc-details-m target-node-k {:weight potential-new-weight :path new-path}))
              acc-details-m))
          acc-details-m))
      node-details-m
      current-node-targets-m)))

(defn weight->nil [m repulsive-amount]
  "If the weight was unaffected then it is unknown, so set it to nil"
  (update m :weight (fn [w]
                      (when (not= w repulsive-amount)
                        w))))

(>defn dijkstra
  "Given a starting point returns details about every node in the graph. Details include the least
  accumulated weight to get there as well as the path taken. If specify an end-node then the details
  just for it are returned. If maximise-weight? is specified then the greatest accumulated weight to get
  there is discovered. Think of wanting to accumulate gold along the joins rather than using up fuel by
  riding to the destination"
  ([g start-node end-node {:keys [maximise-weight?] :as options}]
   [::gr/graph ::gr/vertex (? ::gr/vertex) map? => map?]
   (let [repulsive-amount (if maximise-weight? -1 infinity)
         key-compare (if maximise-weight? max-key min-key)]
     (loop [nodes-details-m (assoc (zipmap (keys g) (repeat {:weight repulsive-amount}))
                              start-node {:weight 0})
            current-node start-node
            unvisited (disj (set (keys g)) start-node)]
       (let [visited-all? (empty? unvisited)
             current-unaltered? (= repulsive-amount (get-in nodes-details-m [current-node :weight]))]
         (cond
           (= current-node end-node)
           (-> (get nodes-details-m end-node)
               (weight->nil repulsive-amount))

           (or visited-all? current-unaltered?)
           (->> nodes-details-m
                (map (fn [[k v]]
                       [k (weight->nil v repulsive-amount)]))
                (into {}))

           :else
           (let [next-node-details-m (update-weights-and-path g nodes-details-m unvisited current-node maximise-weight?)
                 next-node (apply key-compare #(-> next-node-details-m % :weight) unvisited)
                 next-unvisited (disj unvisited next-node)]
             (recur next-node-details-m next-node next-unvisited)))))))
  ([g start-node options]
   [::gr/graph ::gr/vertex map? => map?]
   (dijkstra g start-node nil options))
  ([g start-node]
   [::gr/graph ::gr/vertex => map?]
   (dijkstra g start-node nil {})))

(defn dijkstra-hof
  "Constructs a new function closing over a source-vertex, to use over and over for each target-vertex that have,
  the work only being done at the time of the outer call"
  [desired-key minimise? g source-vertex]
  (assert #{:weight :path} "Dijkstra only collects :weight and :path")
  (let [m (dijkstra g source-vertex nil {:maximise-weight? (not minimise?)})]
    (fn [target-vertex]
      (-> m target-vertex desired-key))))

(>defn shortest-path
  "The shortest (least cost/weight/distance) path from src-key to dest-key. Returns a vector of these keys
  that doesn't include src-key but does include dest-key as the last element. Calls a `dijkstra` fn that
  short-circuits. If there is no path to dest-key nil is returned"
  [g src-key dest-key]
  [::gr/graph ::gr/vertex ::gr/vertex => (? ::gr/vertices)]
  (:path (dijkstra g src-key dest-key {})))

(>defn longest-path
  "Short circuits and discards. Don't use when have many dest-key. Use batch-longest-path-hof instead"
  [g src-key dest-key]
  [::gr/graph ::gr/vertex ::gr/vertex => (? ::gr/vertices)]
  (:path (dijkstra g src-key dest-key {:maximise-weight? true})))

(>defn eccentricity
  "The greatest distance between v and any other vertex, where distance is the shortest path"
  [g vertex]
  [::gr/graph ::gr/vertex => (? int?)]
  (let [distance-from-f (dijkstra-hof :weight true g vertex)
        other-vertices (remove #{vertex} (keys g))
        distances (keep distance-from-f other-vertices)]
    (when (-> distances empty? not)
      (apply max distances))))

(>defn radius
  "The minimum eccentricity of any vertex in a graph"
  [g]
  [::gr/graph => (? int?)]
  (let [all-vertices (keys g)
        all-eccentricities (keep (partial eccentricity g) all-vertices)]
    (when (-> all-eccentricities empty? not)
      (apply min all-eccentricities))))

(>defn diameter
  "The maximum eccentricity of any vertex in a graph"
  [g]
  [::gr/graph => (? int?)]
  (let [all-vertices (keys g)
        all-eccentricities (keep (partial eccentricity g) all-vertices)]
    (when (-> all-eccentricities empty? not)
      (apply max all-eccentricities))))

(>defn edge-count
  "Count of the number of edges on a graph"
  [g]
  [::gr/graph => int?]
  (reduce
    (fn [n [k v]]
      (+ n (count v)))
    0
    g))

(def D shortest-path)