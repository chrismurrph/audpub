(ns audience-republic.question-2
  "Randomly generate a simple directed graph"
  (:require
    [audience-republic.metrics :as metrics]
    [audience-republic.example :as example]
    [audience-republic.graph :as gr]
    [au.com.seasoft.general.dev :as dev]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [clojure.test :refer :all]
    ))

(>defn create-no-edges-graph
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

(defn too-dense? [node-count edge-count]
  (let [max-allowed-edges (quot (* node-count (dec node-count)) 2)]
    (> edge-count max-allowed-edges)))

(defn sparseness-reading
  "Returns :too-sparse, :too-dense or :okay. 'edge-count to be from N-1 to N(N-1)/2'"
  [node-count edge-count]
  (cond
    (< edge-count (dec node-count)) :too-sparse
    (too-dense? node-count edge-count) :too-dense
    :else :okay))

(>defn reject-target-assignment?
  "Return true for rejection of proposed-key. Two types of rejection. Either assigning the target vertex onto the
  same source vertex, or assigning the target vertex where that target already exists"
  [source-key existing-targets proposed-key]
  [::gr/vertex ::gr/targets ::gr/vertex => boolean?]
  (boolean (or (= source-key proposed-key)
               (let [targets (set (map first existing-targets))]
                 (targets proposed-key)))))

(defn make-alternative-source [node-count]
  (assert (pos-int? node-count))
  (let [res (-> node-count rand-int inc str keyword)]
    (assert (not= res :0) ["alternative-source is strange" res node-count])
    res))

(defn adjust-pairs-for-rejects [node-count pairs rejects]
  (dev/log-off "pairs" pairs)
  (dev/log-off "rejects" rejects)
  (dev/probe-off (take (count rejects)
                       (->> pairs
                            (remove (comp not (into #{} rejects) second))
                            (map (fn [[source target]]
                                   [(make-alternative-source node-count) target]))))))

(>defn pairs-into-graph
  [node-count pairs graph]
  [int? ::gr/edges ::gr/graph => ::gr/graph]
  (loop [pairs pairs
         graph graph
         times 0]
    (when (> times 100)
      (dev/pp pairs)
      (dev/pp graph)
      (dev/err "Too many loops for pairs-into-graph"))
    (when (pos? times)
      ;; Fixing rejection is still being used, turn this debug on to see
      (dev/log-off "At count" times "for pairs-into-graph. This means a rejection is being fixed"))
    (let [{:keys [graph rejects]} (reduce
                                    (fn [{:keys [graph rejects] :as acc} [source target]]
                                      (let [reject? (reject-target-assignment? source (get graph source) target)]
                                        (if reject?
                                          {:rejects (conj rejects target)
                                           :graph   graph}
                                          {:graph   (update graph source conj [target (inc (rand-int 20))])
                                           :rejects rejects})))
                                    {:graph   graph
                                     :rejects []}
                                    pairs)]
      (if (empty? rejects)
        graph
        (recur
          (adjust-pairs-for-rejects node-count pairs rejects)
          graph
          (inc times))))))

(defn dups-exist?
  "If the same thing is at the same index then that's a duplicate"
  [list-1 list-2]
  (let [in-pairs (map vector list-1 list-2)]
    (not (every? (partial apply not=) in-pairs))))

(defn fix-any-dups [list-1 list-2]
  (loop [list-2 list-2
         times 0]
    (when (> times 100)
      (dev/pp list-1)
      (dev/pp list-2)
      (dev/err "Too many loops for fix-any-dups"))
    (if (dups-exist? list-1 list-2)
      (recur (shuffle list-2) (inc times))
      [list-1 list-2])))

(defn shuffle-pairs [pairs]
  (let [list-1 (map first pairs)
        list-2 (map second pairs)]
    (map vector (shuffle list-1) (shuffle list-2))))

(defn make-connected-graph
  "Create a graph, check that it is connected? and if not try again. This feature not currently being used,
  as our definition for connected was too strict and anyway we know we are producing connected graphs."
  [node-count starting-pairs starting-graph msg]
  (loop [pairs starting-pairs
         times 0]
    (when (> times 500)
      (dev/pp starting-pairs)
      (dev/pp pairs)
      (dev/pp starting-graph)
      (dev/err (str "Too many loops for make-connected-graph when '" msg "'")))
    (let [new-graph (pairs-into-graph node-count pairs starting-graph)
          connected? true]
      (when (pos? times)
        ;; printing b/c no longer should see
        (dev/log-a "At count" times "for" msg "is the graph connected?" connected?))
      (if connected?
        new-graph
        (recur (shuffle-pairs pairs) (inc times))))))

(>defn produce-edges-2
  "Create edges, forming a line joining all the nodes together. A bit contrived but
  at least the graph will definitely be connected"
  [nodes]
  [::gr/shuffled-vertices => ::gr/edges]
  (let [pool nodes
        [new-target new-pool] ((juxt peek pop) pool)]
    (loop [pool new-pool
           result []
           last-target new-target]
      (if (empty? pool)
        result
        (let [[new-target new-pool] ((juxt peek pop) pool)
              new-edge [last-target new-target]]
          (recur new-pool (conj result new-edge) new-target))))))

(defn produce-edges-1 [extra-edges-required nodes]
  (let [extra-sources (take extra-edges-required (shuffle nodes))
        extra-targets (take extra-edges-required (shuffle nodes))
        [extra-sources extra-targets] (fix-any-dups extra-sources extra-targets)]
    (map vector extra-sources extra-targets)))

(>defn generate-graph [node-count edge-count]
  [int? int? => ::gr/failure-or-graph]
  (let [verdict (sparseness-reading node-count edge-count)]
    (if (= :okay verdict)
      (let [starting-graph (create-no-edges-graph node-count)
            nodes (keys starting-graph)
            pairs (produce-edges-2 (into '() nodes))
            filled-graph (make-connected-graph node-count pairs starting-graph "starting")
            num-extra-edges-required (- edge-count node-count)]
        (if (pos? num-extra-edges-required)
          (let [extra-pairs (produce-edges-2 (->> (cycle nodes)
                                                  (take (+ num-extra-edges-required 2))
                                                  (into '())))]
            (make-connected-graph node-count extra-pairs filled-graph "filling extra"))
          filled-graph))
      ((verdict->msg-f verdict) node-count edge-count))))

(def G generate-graph)

(defn connected-f-was-too-strict []
  [(metrics/don-t-use-connected? example/disconnected-graph)
   (metrics/don-t-use-connected? example/connected-graph-2)
   (metrics/don-t-use-connected? (assoc-in example/connected-graph-2 [:8 :5] 20))])

(comment
  (generate-graph 10 15)
  (create-no-edges-graph 10)
  (connected-f-was-too-strict))