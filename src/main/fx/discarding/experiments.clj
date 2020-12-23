(ns fx.discarding.experiments
  "Attempting to layout the graph by myself to get an appreciation of how difficult the job is"
  (:require
    [cljfx.api :as fx]
    [audience-republic.example-data :as example]
    [au.com.seasoft.general.dev :as dev]
    [audience-republic.graph :as gr]
    [audience-republic.util :as util])
  (:import [javafx.scene.paint Color]))

;;
;; Before delete just make sure everything 'works'. Never really deleting when in version control.
;;

(defn vertex-view->index-number [{:keys [children] :as vertex-view}]
  (let [{:keys [text] :as label-child} (first (filter (comp #{:label} :fx/type) children))]
    (Long/parseLong text)))

(defn vertex-view->start-point [horizontal-space {:keys [layout-x layout-y] :as vertex-view}]
  [(+ horizontal-space layout-x) layout-y])

(defn vertex-view
  [radius index [x y :as point]]
  {:fx/type  :stack-pane
   :layout-x x
   :layout-y y
   :children [{:fx/type :circle
               :fill    Color/ORCHID
               :stroke  :black
               :radius  radius}
              {:fx/type :label
               :text    (str index)}]})

(defn x-1 []
  (-> (vertex-view 10 3 [10 10])
      vertex-view->index-number))

(defn edge-view [[from-x from-y :as from] [to-x to-y :as to]]
  (dev/log-on "edge from, to" from to)
  {:fx/type  :path
   :elements [{:fx/type :move-to
               :x       from-x :y from-y}
              {:fx/type :line-to
               :x       to-x :y to-y}]})

(defn edge-view? [{:fx/keys [type]}]
  (= :path type))

(defn vertex-view? [{:fx/keys [type]}]
  (= :stack-pane type))

(defn pane-of-vertices-and-edges
  "Makes sure the edges come before the vertices"
  [children]
  {:fx/type  :pane
   :children (sort-by (fn [view]
                        (cond
                          (edge-view? view) -1
                          (vertex-view? view) 1))
                      children)})

(defn see-something [something]
  (fx/on-fx-thread
    (fx/create-component
      {:fx/type :stage
       :showing true
       :scene   {:fx/type :scene
                 :root    something}})))

(defn distribute-from-a-point-hof
  "Draw them going down. Returns a vector of edge-views and vertex-views. Can be made to be creeping leftward if
  set right-to-left? true. It is basically a fan that can be used recursively"
  [[x about-y :as middle-point] {:keys [radius horizontal-spacing vertical-spacing at-origin?]}]
  (let [x-origin (- x horizontal-spacing)]
    (fn [vertex-numbers]
      (let [num-nodes (count vertex-numbers)
            slightly-down-by (if (even? num-nodes)
                               (quot vertical-spacing 2)
                               0)
            ;; start-y is top place we are going to pepper them down from
            start-y (+ slightly-down-by
                       (- about-y (* (quot num-nodes 2) vertical-spacing)))
            edge-views (if at-origin?
                         []
                         (->> vertex-numbers
                              (map-indexed (fn [idx _]
                                             (let [y (+ radius start-y (* idx vertical-spacing))]
                                               (edge-view [(+ radius x-origin) (+ radius about-y)]
                                                          [(+ radius x) y]))))))
            vertex-views (->> vertex-numbers
                              (map-indexed (fn [idx num]
                                             (vertex-view radius num [x (+ start-y (* idx vertical-spacing))]))))]
        (concat edge-views vertex-views)))))

(defn vertex-view->targets [g vertex-view]
  (let [index (vertex-view->index-number vertex-view)]
    (assert (int? index) ["No index" vertex-view])
    (-> index str keyword dev/probe-off g keys)))

(defn x-2 []
  (let [g example/simple-graph]
    (->> (vertex-view 10 1 [10 10])
         (vertex-view->targets g))))

(defn targets-count [g node]
  (let [res (-> g node keys count)]
    (dev/log-on "reverse count" node res)
    res))

(defn layout-graph-hof [g {:keys [singles-only? horizontal-spacing] :as options}]
  (let [reverse-g (gr/reverse-graph g)
        reverse-count-f (if singles-only?
                          (partial targets-count reverse-g)
                          (constantly 1))]
    (fn inner [start-point targets at-origin?]
      (tap> {"start-point" start-point "targets" targets})
      (let [distribute-from-a-point (distribute-from-a-point-hof start-point (assoc options :at-origin? at-origin?))
            vertex-and-edge-views (->> targets
                                       (map util/kw->number)
                                       sort
                                       distribute-from-a-point)
            recurse-vertex-views (into []
                                       (mapcat (fn [vertex-view]
                                                 (let [start-point (vertex-view->start-point horizontal-spacing vertex-view)
                                                       targets (vertex-view->targets g vertex-view)
                                                       single-targets (filter #(= 1 (reverse-count-f %)) targets)]
                                                   (inner start-point single-targets false)))
                                               (remove edge-view? vertex-and-edge-views)))]
        (into vertex-and-edge-views recurse-vertex-views)))))

(defn x-3 []
  (let [g example/simple-graph
        start-point [10 50]
        options {:radius 10 :horizontal-spacing 30 :vertical-spacing 40 :singles-only? false}
        layout-graph (layout-graph-hof g options)
        nodes (keys g)
        source-node (first nodes)
        ]
    (see-something (pane-of-vertices-and-edges (layout-graph start-point [source-node] true)))))
