(ns layout.discarding.ham-2
  "Taking the straight translation (ham-1) that didn't work out, and making it more functional to see if I could understand it better that way.
  Also a failure. The in-place updates were too difficult for me to follow."
  (:require
    [audience-republic.graph :as gr]
    [layout.math :as math]
    [com.fulcrologic.guardrails.core :refer [>defn => | ?]]
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

(defn aligned-metrics [ham-st]
  (let [{:keys [max-movement equilibrium-distance]} ham-st
        needs-be-pos-1 (- (* EQUILIBRIUM_ALIGNMENT_FACTOR equilibrium-distance) max-movement)
        needs-be-pos-2 (- max-movement DEFAULT_MAX_MOVEMENT)]
    [needs-be-pos-1 needs-be-pos-2]))

(defn aligned? [ham-st]
  (let [{:keys [max-movement equilibrium-distance]} ham-st
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

(defn make-test-ham-1 []
  (let [g example/simple-graph]
    (assoc (make-ham g) :coordinates {:1 {:coordinates [-0.8264670072177795 -0.55551549554952495]}
                                      :2 {:coordinates [-0.2264670072177795 -0.85551549554952495]}
                                      :3 {:coordinates [-0.4264670072177795 -0.15551549554952495]}
                                      :4 {:coordinates [-0.8264670072177795 -0.65551549554952495]}
                                      })))

(defn make-test-ham-2 []
  (let [g example/unreachable-nodes-graph]
    (make-ham g)))

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

(deftest coordinates->distance-test
  (is (= 0.0 (coordinates->distance [0.0 0.0]))))

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
  [ham-st node-to-align]
  (let [{:keys [coordinates graph reversed-graph equilibrium-distance
                learning-rate acceptable-distance-factor max-movement]} ham-st
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
                                                                                ham-st
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
        [new-location move-distance apply-learning-rate] (if (> move-distance (* equilibrium-distance acceptable-distance-factor))
                                                           (let [new-learning-rate (/ (* equilibrium-distance acceptable-distance-factor)
                                                                                      move-distance)
                                                                 apply-learning-rate (if (< new-learning-rate learning-rate)
                                                                                       new-learning-rate
                                                                                       (* learning-rate LEARNING_RATE_INCREASE_FACTOR))]
                                                             [old-location DEFAULT_TOTAL_MOVEMENT apply-learning-rate])
                                                           [new-location move-distance learning-rate])
        new-ham-st (-> ham-st
                       (assoc learning-rate apply-learning-rate)
                       (update :max-movement (fn [old]
                                               (if (> move-distance max-movement)
                                                 move-distance
                                                 old)))
                       (update :total-movement (fn [old]
                                                 (+ old move-distance)))
                       (update :coordinates (fn [old]
                                              (assoc old node-to-align new-location))))]
    [new-ham-st new-location]))

(defn average-movement [{:keys [graph total-movement]}]
  (/ total-movement (count (gr/nodes graph))))

(defn process-locally [ham-st]
  (let [{:keys [graph learning-rate equilibrium-distance acceptable-distance-factor]} ham-st
        nodes (gr/nodes graph)
        point-sum (reduce
                    (fn [m node]
                      (let [[ham-st new-point] (align-node ham-st node)]
                        (-> m
                            (assoc :ham-st ham-st)
                            (update :coordinates (fn [old-cords]
                                                   (mapv + old-cords (:coordinates new-point)))))))
                    {:coordinates [0 0]}
                    nodes)
        {:keys [ham-st]} point-sum]
    (when (< (* learning-rate LEARNING_RATE_PROCESSING_ADJUSTMENT) DEFAULT_LEARNING_RATE)
      (let [acceptable-distance-adjustment 0.1
            low-movement? (< (average-movement ham-st) (* equilibrium-distance acceptable-distance-factor acceptable-distance-adjustment))
            ham-st (-> ham-st
                       (update :acceptable-distance-factor (fn [old]
                                                             (if low-movement?
                                                               (* old LEARNING_RATE_INCREASE_FACTOR)
                                                               old)))
                       (update :learning-rate (fn [old]
                                                (* old LEARNING_RATE_PROCESSING_ADJUSTMENT))))]
        (dev/log-a "learning rate:" (-> ham-st :learning-rate) ", acceptableDistanceFactor:" (-> ham-st deref :acceptable-distance-factor))))
    [ham-st (dissoc point-sum :ham-st)]))

(defn recenter-nodes [ham-st nodes center]
  (let [{:keys [coordinates]} ham-st
        updated-coordinates (reduce
                              (fn [m node]
                                ;; Yes s/be an update, and then an update in the swap itself, and then look at all the
                                ;; other swaps...
                                (let [new-coord-value (calculate-relative-to (get coordinates node) center)]
                                  (assoc m node new-coord-value)))
                              coordinates
                              nodes)]
    (-> ham-st
        (assoc :coordinates updated-coordinates))))

(defn align [ham-st]
  (let [{:keys [coordinates graph]} ham-st
        nodes (gr/nodes graph)
        new-ham-st (if (not= (-> coordinates keys set) nodes)
                     (let [updated-coordinates (reduce
                                                 (fn [m node]
                                                   (if (some #{node} (keys m))
                                                     m
                                                     (assoc m node (point->Vector [(rand-coordinate) (rand-coordinate)]))))
                                                 coordinates
                                                 nodes)]
                       (assoc ham-st :coordinates updated-coordinates))
                     ham-st)
        new-ham-st (assoc new-ham-st :total-movement DEFAULT_TOTAL_MOVEMENT :max-movement DEFAULT_MAX_MOVEMENT)]
    (let [[new-ham-st center] (process-locally new-ham-st)
          ;_ (dev/log-a "maxMove:" (-> ham-atom deref :max-movement) ", Average Move:" (average-movement @ham-atom))
          center (update center :coordinates
                         (fn [old-cords]
                           (mapv (fn [num]
                                   (/ num (count nodes)))
                                 old-cords)))]
      (recenter-nodes new-ham-st nodes center))))

(defn show-coordinates [ham-st]
  (dev/log-a "aligned?" (aligned-metrics ham-st) "\n"
             (dev/pp-str (->> ham-st
                              :coordinates
                              dev/probe->>off
                              (map (fn [[k {:keys [coordinates] :as v}]]
                                     [k (mapv (partial * 10) coordinates)]))
                              (sort-by #(-> % first util/kw->number))
                              vec
                              ))))

(def aligned-result
  {:1 {:coordinates [-0.3213310942785421 -0.07808264800958746]}
   :2 {:coordinates [0.27866890572145797 -0.3780826480095875]}
   :3 {:coordinates [0.36718523181103757 0.6375650909403563]}
   :4 {:coordinates [-0.3213310942785421 -0.17808264800958745]}})

(defn crunches-a-bigger-graph []
  (let [ham-st (make-test-ham-2)
        first-aligned (->> (iterate align ham-st)
                           (drop-while (complement aligned?))
                           (take 1)
                           first)]
    (show-coordinates first-aligned)))

(deftest crunches-same-numbers-test
  (let [ham-st (make-test-ham-1)
        first-aligned (->> (iterate align ham-st)
                           (drop-while (complement aligned?))
                           (take 1)
                           first)]
    (is (= aligned-result (:coordinates first-aligned)))))

(comment
  (run-tests)
  )


