(ns layout.discarding.ham-1
  "Straight translation from Java code that didn't work out, see bottom of this file"
  (:require
    [audience-republic.graph :as gr]
    [layout.math :as math]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
    [clojure.spec.alpha :as s]
    [audience-republic.util :as util :refer [swap!->]]
    [au.com.seasoft.general.dev :as dev]
    [audience-republic.example-data :as example]
    [clojure.test :refer :all]
    ))

;;
;; Before delete just make sure everything 'works'. Never really deleting when in version control.
;;

(def DEFAULT_MAX_MOVEMENT 0.0)
(def DEFAULT_TOTAL_MOVEMENT 0.0)
(def EQUILIBRIUM_ALIGNMENT_FACTOR 0.005)
(def EQUILIBRIUM_DISTANCE 1.0)
(def ATTRACTION_STRENGTH 4.0)
(def REPULSIVE_WEAKNESS 2.0)
(def DEFAULT_ACCEPTABLE_DISTANCE_FACTOR 0.75)
(def LEARNING_RATE_INCREASE_FACTOR 0.9)
(def DEFAULT_LEARNING_RATE 0.4)
(def LEARNING_RATE_PROCESSING_ADJUSTMENT 1.01)

(defn rand-coordinate
  "Random number between -1.0 and 1.0"
  []
  (- (* (rand) 2.0) 1.0))

;&& (maxMovement < (EQUILIBRIUM_ALIGNMENT_FACTOR * equilibriumDistance))
;&& (maxMovement > 0.0)

(defn aligned-metrics [ham-atom]
  (let [{:keys [max-movement equilibrium-distance]} @ham-atom
        needs-be-pos-1 (- (* EQUILIBRIUM_ALIGNMENT_FACTOR equilibrium-distance) max-movement)
        needs-be-pos-2 (- max-movement DEFAULT_MAX_MOVEMENT)]
    #_(and
        (pos? needs-be-pos-1)
        (pos? needs-be-pos-2))
    [needs-be-pos-1 needs-be-pos-2]))

(defn aligned? [ham-atom]
  (let [{:keys [max-movement equilibrium-distance]} @ham-atom
        needs-be-pos-1 (- (* EQUILIBRIUM_ALIGNMENT_FACTOR equilibrium-distance) max-movement)
        needs-be-pos-2 (- max-movement DEFAULT_MAX_MOVEMENT)]
    (and
      (pos? needs-be-pos-1)
      (pos? needs-be-pos-2))))

(defn average-movement [graph total-movement]
  (/ total-movement (count graph)))

(defn point->Vector [point]
  {:coordinates point})

(defn default-coordinates
  "coordinates in Ham is a map of node->Vector. Inside every Vector there is also coordinates, which is just an
  [x y] point, given that we are only in two dimensions"
  [nodes]
  (reduce
    (fn [m node]
      (assoc m node (point->Vector [(rand-coordinate) (rand-coordinate)])))
    {}
    nodes))

(defn x-3 []
  (point->Vector [(rand-coordinate) (rand-coordinate)]))

(defn make-ham [graph]
  {:graph                      graph
   :reversed-graph             (gr/reverse-graph graph)
   :coordinates                (default-coordinates (keys graph))
   :total-movement             DEFAULT_TOTAL_MOVEMENT
   :max-movement               DEFAULT_MAX_MOVEMENT
   :equilibrium-distance       EQUILIBRIUM_DISTANCE
   :acceptable-distance-factor DEFAULT_ACCEPTABLE_DISTANCE_FACTOR
   :learning-rate              DEFAULT_LEARNING_RATE
   })

(defn make-test-ham []
  (let [g example/simple-graph]
    (assoc (make-ham g) :coordinates {:1 {:coordinates [-0.8264670072177795 -0.55551549554952495]}
                                      :2 {:coordinates [-0.2264670072177795 -0.85551549554952495]}
                                      :3 {:coordinates [-0.4264670072177795 -0.15551549554952495]}
                                      :4 {:coordinates [-0.8264670072177795 -0.65551549554952495]}
                                      })))

(defn make-test-ham-2 []
  (let [g example/unreachable-nodes-graph]
    (make-ham g)))

(def _ham (atom nil))

(defn initialise [ham]
  (do
    (reset! _ham ham)
    _ham))

(>defn neighbours-f
  [graph reversed-graph equilibrium-distance node]
  [::gr/graph ::gr/graph number? ::gr/vertex => map?]
  (reduce
    (fn [m [source target :as edge]]
      (let [other-nodes (remove #{node} [source target])]
        (reduce
          (fn [m other-node]
            (dev/log-off "other-node equilibrium-distance" other-node equilibrium-distance)
            (assoc m other-node equilibrium-distance))
          m
          other-nodes)))
    {}
    (gr/adjacent-edges graph reversed-graph node)))

(>defn calculate-relative-to
  [coordinates-point absolute-point]
  [map? map? => map?]
  (let [[c-x c-y] (:coordinates coordinates-point)
        [a-x a-y] (:coordinates absolute-point)]
    (assert (number? c-x))
    (assert (number? c-y))
    (assert (number? a-x))
    (assert (number? a-y))
    (point->Vector [(- c-x a-x) (- c-y a-y)])))

(>defn coordinates->distance
  "Could easily be called point->distance. We don't yet cache this in a Vector at distance-cache"
  [point]
  [vector? => number?]
  (dev/log-off "distance from" point)
  (-> (reduce
        (fn [squared-sum coordinate]
          (+ squared-sum (* coordinate coordinate)))
        0.0
        point)
      math/sqrt))

(defn coordinates->distance-test []
  (= 0.0 (coordinates->distance [0.0 0.0])))

(defn update-coordinates-f
  "Does same as setDistance from Java source code"
  [point distance]
  (assert (seq point) ["Not a proper point" point])
  (let [old-distance (coordinates->distance point)
        scalar (/ distance old-distance)]
    (mapv #(* % scalar) point)))

(defn neighbour-vector->neighbour-vector
  [{:keys [learning-rate]}
   association-equilibrium-distance
   {neighbour-vector-coords :coordinates :as neighbour-vector}]
  (let [abs-distance (-> neighbour-vector-coords coordinates->distance math/abs)
        _ (dev/log-off "from" neighbour-vector-coords "to" abs-distance)
        diff-1 (- abs-distance association-equilibrium-distance)]
    (if (> abs-distance association-equilibrium-distance)
      (let [new-distance (math/pow diff-1 ATTRACTION_STRENGTH)
            new-distance (if (> (math/abs new-distance) (math/abs diff-1))
                           (math/copy-sign (math/abs diff-1) new-distance)
                           new-distance)]
        (update neighbour-vector :coordinates update-coordinates-f (* new-distance learning-rate)))
      (let [diff-2 (- association-equilibrium-distance abs-distance)
            _ (dev/log-off "abs-distance" abs-distance)
            ratio (/ diff-2 association-equilibrium-distance)
            _ (dev/log-off "ratio" ratio)
            new-distance (* (- EQUILIBRIUM_DISTANCE)
                            (math/atanh ratio))
            new-distance (if (> (math/abs new-distance) (math/abs diff-2))
                           (* (- EQUILIBRIUM_DISTANCE)
                              diff-2)
                           new-distance)]
        (update neighbour-vector :coordinates update-coordinates-f (* new-distance learning-rate))))))

(defn align-node
  "Location is just a point, b/c we only deal in 2 dimensions"
  [ham-atom node-to-align]
  (let [{:keys [coordinates graph reversed-graph equilibrium-distance
                learning-rate acceptable-distance-factor max-movement]} @ham-atom
        old-location (get coordinates node-to-align)
        neighbours-m (neighbours-f graph reversed-graph equilibrium-distance node-to-align)
        _ (dev/log-off "node" node-to-align "neighbours" (keys neighbours-m))
        composite-vector (reduce-kv
                           (fn [m neighbour-k association-equilibrium-distance]
                             (let [current-neighbour-coords (get coordinates neighbour-k)
                                   _ (dev/log-off "coords of neighbour" current-neighbour-coords neighbour-k)
                                   neighbour-vector (calculate-relative-to
                                                      current-neighbour-coords
                                                      old-location)
                                   _ (dev/log-off "neighbour-vector" neighbour-vector)
                                   {new-neighbour-vector-coords :coordinates} (neighbour-vector->neighbour-vector
                                                                                @ham-atom
                                                                                association-equilibrium-distance
                                                                                neighbour-vector)]
                               (dev/log-off "new-neighbour-vector-coords" new-neighbour-vector-coords)
                               (update m :coordinates (fn [old-cords]
                                                        (mapv + old-cords new-neighbour-vector-coords)))))
                           {:coordinates [0 0]}
                           neighbours-m)
        neighbours (keys neighbours-m)
        composite-vector (reduce
                           (fn [m node]
                             (if (and
                                   (not (get neighbours node))
                                   (not= node-to-align node)
                                   (not (some #{node-to-align} (gr/adjacent-nodes graph reversed-graph node))))
                               (let [node-vector-distance (-> (get coordinates node)
                                                              (calculate-relative-to old-location)
                                                              :coordinates
                                                              coordinates->distance)
                                     new-distance (/ (- EQUILIBRIUM_DISTANCE) (math/pow node-vector-distance REPULSIVE_WEAKNESS))
                                     new-distance (if (> (math/abs new-distance) (math/abs equilibrium-distance))
                                                    (math/copy-sign equilibrium-distance new-distance)
                                                    new-distance)]
                                 (update m :coordinates update-coordinates-f (* new-distance learning-rate))
                                 m)
                               m))
                           composite-vector
                           (gr/nodes graph))
        new-location (update composite-vector :coordinates
                             (fn [old-cords]
                               (mapv + old-cords (:coordinates old-location))))
        move-distance (-> new-location
                          (calculate-relative-to old-location)
                          :coordinates
                          coordinates->distance)
        [new-location move-distance] (if (> move-distance (* equilibrium-distance acceptable-distance-factor))
                                       (let [new-learning-rate (/ (* equilibrium-distance acceptable-distance-factor)
                                                                  move-distance)]
                                         (if (< new-learning-rate learning-rate)
                                           (do
                                             (swap! ham-atom assoc :learning-rate new-learning-rate)
                                             (dev/log-a "learning rate:" (-> ham-atom deref :learning-rate)))
                                           (do
                                             (swap! ham-atom update :learning-rate (fn [old]
                                                                                     (* old LEARNING_RATE_INCREASE_FACTOR)))
                                             (dev/log-a "learning rate:" (-> ham-atom deref :learning-rate))))
                                         [old-location DEFAULT_TOTAL_MOVEMENT])
                                       [new-location move-distance])]
    (swap!-> ham-atom
             (update :max-movement (fn [old]
                                     (if (> move-distance max-movement)
                                       move-distance
                                       old)))
             (update :total-movement (fn [old]
                                       (+ old move-distance)))
             (update :coordinates (fn [old]
                                    (assoc old node-to-align new-location))))
    new-location))

(defn average-movement [{:keys [graph total-movement]}]
  (/ total-movement (count (gr/nodes graph))))

(defn process-locally [ham-atom]
  (let [{:keys [graph learning-rate equilibrium-distance acceptable-distance-factor]} @ham-atom
        nodes (gr/nodes graph)
        point-sum (reduce
                    (fn [m node]
                      (let [new-point (align-node ham-atom node)]
                        (update m :coordinates (fn [old-cords]
                                                 (mapv + old-cords (:coordinates new-point))))))
                    {:coordinates [0 0]}
                    nodes)]
    (when (< (* learning-rate LEARNING_RATE_PROCESSING_ADJUSTMENT) DEFAULT_LEARNING_RATE)
      (let [acceptable-distance-adjustment 0.1
            low-movement? (< (average-movement @ham-atom) (* equilibrium-distance acceptable-distance-factor acceptable-distance-adjustment))]
        (swap!-> ham-atom
                 (update :acceptable-distance-factor (fn [old]
                                                       (if low-movement?
                                                         (* old LEARNING_RATE_INCREASE_FACTOR)
                                                         old)))
                 (update :learning-rate (fn [old]
                                          (* old LEARNING_RATE_PROCESSING_ADJUSTMENT))))
        (dev/log-a "learning rate:" (-> ham-atom deref :learning-rate) ", acceptableDistanceFactor:" (-> ham-atom deref :acceptable-distance-factor))))
    point-sum))

(defn recenter-nodes [ham-atom nodes center]
  (let [{:keys [coordinates]} @ham-atom
        updated-coordinates (reduce
                              (fn [m node]
                                ;; Yes s/be an update, and then an update in the swap itself, and then look at all the
                                ;; other swaps...
                                (let [new-coord-value (calculate-relative-to (get coordinates node) center)]
                                  (assoc m node new-coord-value)))
                              coordinates
                              nodes)]
    (swap!-> ham-atom
             (assoc :coordinates updated-coordinates))))

(defn align [ham-atom]
  (let [{:keys [coordinates graph]} @ham-atom
        nodes (gr/nodes graph)]
    (when (not= (-> coordinates keys set) nodes)
      (let [updated-coordinates (reduce
                                  (fn [m node]
                                    (if (some #{node} (keys m))
                                      m
                                      (assoc m node (point->Vector [(rand-coordinate) (rand-coordinate)]))))
                                  coordinates
                                  nodes)]
        (swap! ham-atom assoc :coordinates updated-coordinates)))
    (swap! ham-atom assoc :total-movement DEFAULT_TOTAL_MOVEMENT :max-movement DEFAULT_MAX_MOVEMENT)
    (let [center (process-locally ham-atom)
          ;_ (dev/log-a "maxMove:" (-> ham-atom deref :max-movement) ", Average Move:" (average-movement @ham-atom))
          center (update center :coordinates
                         (fn [old-cords]
                           (mapv (fn [num]
                                   (/ num (count nodes)))
                                 old-cords)))]
      (recenter-nodes ham-atom nodes center))))

(defn show-coordinates-1 [ham-atom]
  (dev/log-a "aligned?" (aligned-metrics ham-atom) "\n"
             (dev/pp-str (-> ham-atom deref :coordinates))))

(defn show-coordinates-2 [ham-atom]
  (dev/log-a "aligned?" (aligned-metrics ham-atom) "\n"
             (dev/pp-str (->> @ham-atom
                              :coordinates
                              dev/probe-off
                              (map (fn [[k {:keys [coordinates] :as v}]]
                                     [k (mapv (partial * 10) coordinates)]))
                              (sort-by #(-> % first util/kw->number))
                              vec
                              ))))

(def twentieth-result
  {:1 {:coordinates [-0.45755436055247667 -0.0675157090106311]}
   :2 {:coordinates [0.5467694336222322 -0.3083039530411539]}
   :3 {:coordinates [0.3506773589830262 0.5213469272992014]}
   :4 {:coordinates [-0.4398924320527817 -0.1455272652474165]}})

(deftest crunches-same-numbers
  (let [ham-atom (initialise (make-test-ham))]
    (doseq [_ (range 20)]
      (align ham-atom)
      ;(show-coordinates-1 ham-atom)
      )
    (is (= twentieth-result (-> ham-atom deref :coordinates)))
    ))

(defn crunches-bigger-graph
  "No matter what I do a mess is created. So I need to verify that the original Java version works."
  []
  (let [ham-atom (initialise (make-test-ham-2))]
    (loop [times 300]
      (align ham-atom)
      (when (pos? times) #_(not (aligned? ham-atom))
        (recur (dec times))))
    (show-coordinates-2 ham-atom)))

(comment
  (run-tests)
  )


