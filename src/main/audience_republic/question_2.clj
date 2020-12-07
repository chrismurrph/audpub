(ns audience-republic.question-2
  "Randomly generate a simple directed graph"
  (:require
    [audience-republic.graph :as gr]
    [audience-republic.util :as util]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [clojure.set :as set]
    ))

(>defn create-no-edges-graph
  "Generate a graph from scratch that is all vertices and no edges. Every node is an island. Each vertex is a
  keyword number, so :1, :2, :3 up to :n"
  [n]
  [int? => ::gr/graph]
  (let [nodes (->> (range 1 (inc n))
                   (map (comp keyword str)))]
    (zipmap nodes (repeat {}))))

(def verdict->msg-f
  {:too-sparse (fn [node-count edge-count]
                 {:fail-type :too-sparse
                  :message   (str "Too sparse because there are only " edge-count " edges for " node-count " nodes. Edge count can't be less than one less than the node count")})
   :too-dense  (fn [node-count edge-count]
                 {:fail-type :too-dense
                  :message   (str "Too dense because there are " edge-count " (many) edges to cover only " node-count " nodes")})})

(defn too-dense?
  "Are too many edges for the number of nodes, according to the formula N(N-1)/2"
  [node-count edge-count]
  (let [max-allowed-edges (quot (* node-count (dec node-count)) 2)]
    (> edge-count max-allowed-edges)))

(defn sparseness-reading
  "Returns :too-sparse, :too-dense or :okay. 'edge-count to be from N-1 to N(N-1)/2'"
  [node-count edge-count]
  (cond
    (< edge-count (dec node-count)) :too-sparse
    (too-dense? node-count edge-count) :too-dense
    :else :okay))

(>defn extra-edges-into-graph
  "Starting with an already connected graph we add the extra edges, taking care not to clash: a node should
  not target:
  1/ itself
  2/ the same node more than once
  3/ a node that is already pointing to it
  If one source node happens to fill up then it won't receive any more targets."
  [graph spaces-available-nodes num-extra-edges]
  [::gr/graph ::gr/vertices int? => ::gr/graph]
  (let [all-nodes (-> graph keys set)]
    (loop [count-remaining num-extra-edges
           spaces-available-nodes spaces-available-nodes
           graph graph]
      (if (zero? count-remaining)
        graph
        (let [source-node (rand-nth spaces-available-nodes)
              existing-targets (-> graph source-node keys)
              avoid (conj (set existing-targets) source-node)
              candidates (->> (set/difference all-nodes avoid)
                              (remove (fn [candidate]
                                        ;; A node that already points at source-node can't be made one of its targets,
                                        ;; as there are no two-way streets allowed. So that lessens by 1 the possible
                                        ;; number of targets that source-node has. But here we are hanging up the boots
                                        ;; of source-node, saying it hasn't got any spaces left to any nodes. The fact
                                        ;; that what I'm doing works only makes sense if there are now no candidates.
                                        ;; Ha - that's other side of (seq candidates). So if it points to source and
                                        ;; it is the last candidate left then, then it really is time to close
                                        ;; the doors. We fill more and never un-fill, so can permanently close the door.
                                        ;; All other possible targets are targets - we know that b/c we have crept to
                                        ;; the end.
                                        (let [points-at (-> graph candidate keys set)]
                                          (points-at source-node)))))]
          (if (seq candidates)
            (let [new-target (-> candidates seq rand-nth)
                  new-graph (update graph source-node assoc new-target (inc (rand-int 20)))
                  new-spaces-available-nodes (if (= 1 (count candidates))
                                               (remove #{source-node} spaces-available-nodes)
                                               spaces-available-nodes)]
              (recur
                (dec count-remaining)
                new-spaces-available-nodes
                new-graph))
            (recur
              count-remaining
              (remove #{source-node} spaces-available-nodes)
              graph)))))))

(>defn edges-into-graph
  "Update the graph with new edges"
  [edges graph]
  [::gr/edges ::gr/graph => ::gr/graph]
  (reduce
    (fn [graph [source weight target]]
      (update graph source conj [target weight]))
    graph
    edges))

(>defn produce-edges
  "Create edges, forming a long 'line' joining all the nodes together. It is one line but it sometimes
  changes direction (using util/changing-booleans), giving us a more interesting graph. Most important
  is 'one line', meaning no islands. There *will* likely be unreachable nodes"
  [nodes]
  [::gr/shuffled-vertices => ::gr/edges]
  (let [arrows (util/changing-booleans (count nodes))
        [target new-xs] ((juxt peek pop) nodes)]
    (loop [xs new-xs
           arrows arrows
           result []
           last-target target]
      (if (empty? xs)
        result
        (let [[target remaining-xs] ((juxt peek pop) xs)
              [arrow? remaining-arrows] ((juxt peek pop) arrows)
              new-weight (inc (rand-int 20))
              new-edge (if arrow?
                         [last-target new-weight target]
                         [target new-weight last-target])]
          (recur remaining-xs remaining-arrows (conj result new-edge) target))))))

(>defn generate-graph
  "Create a connected graph, with the number of nodes and edges requested. Will return a failure map
  if the request is for a graph that is either too sparse or too dense."
  [node-count edge-count]
  [int? int? => ::gr/failure-or-graph]
  (let [verdict (sparseness-reading node-count edge-count)]
    (if (= :okay verdict)
      (let [starting-graph (create-no-edges-graph node-count)
            nodes (keys starting-graph)
            edges (produce-edges (into '() nodes))
            filled-graph (edges-into-graph edges starting-graph)
            num-extra-edges-required (inc (- edge-count node-count))]
        (if (pos? num-extra-edges-required)
          (extra-edges-into-graph filled-graph nodes num-extra-edges-required)
          filled-graph))
      ((verdict->msg-f verdict) node-count edge-count))))

(def G generate-graph)

(comment
  (dev/pp (generate-graph 20 191))
  )