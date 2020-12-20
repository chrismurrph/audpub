(ns fx.experiments
  (:require
    [audience-republic.example-data :as example]
    [layout.ham :as ham]
    [audience-republic.graph :as gr]
    [audience-republic.util :as util]
    [au.com.seasoft.general.dev :as dev]
    [cljfx.api :as fx]
    [layout.math :as m])
  (:import [javafx.scene.paint Color]))

(def default-radius 10)
(def arrowhead-base 4)

(defn vertex-view->index-number
  "The thing on the screen can be identified by a number"
  [{:keys [children] :as vertex-view}]
  (let [{:keys [text] :as label-child} (first (filter (comp #{:label} :fx/type) children))]
    (Long/parseLong text)))

(defn vertex-view
  [index [x y :as point] {:keys [radius fill-colour stroke-colour]
                          :or   {radius default-radius fill-colour Color/ORCHID stroke-colour :black}}]
  {:fx/type  :stack-pane
   :layout-x x
   :layout-y y
   :children [{:fx/type :circle
               :fill    fill-colour
               :stroke  stroke-colour
               :radius  radius}
              {:fx/type :label
               :text    (str index)}]})

(defn x-1 []
  (-> (vertex-view 10 3 [10 10])
      vertex-view->index-number))

(defn ->vertex-views
  ([coords options]
   (->> coords
        (map (fn [[k v]]
               (let [[x y] v
                     view (vertex-view (util/kw->number k) [x y] options)]
                 view)))))
  ([coords]
   (->vertex-views coords {})))

(defn upright-triangle [[x y :as central-point]]
  (let [triangle-x-radius 5
        triangle-y-radius 7
        ;; If central point was [5 7] we would want no transform at all
        transform-x (- x triangle-x-radius)
        transform-y (- y triangle-y-radius)]
    {:fx/type  :group
     :children [{:fx/type :polygon
                 :points  [(+ transform-x triangle-x-radius)
                           (+ transform-y 0)
                           (+ transform-x 0)
                           (+ transform-y (* triangle-y-radius 2))
                           (+ transform-x (* triangle-x-radius 2))
                           (+ transform-y (* triangle-y-radius 2))]}]}))

(defn arrow-position
  "Given an edge, returns where to put the arrow"
  [[from-x from-y :as from] [to-x to-y :as to] {:keys [radius] :or {radius default-radius}}]
  (let [x-delta (- to-x from-x)
        y-delta (- to-y from-y)
        length (m/sqrt (+ (m/pow x-delta 2) (m/pow y-delta 2)))
        up-to-arrow-point (- length (+ arrowhead-base radius))
        proportion (/ up-to-arrow-point length)
        up-to-x-delta (* proportion x-delta)
        up-to-y-delta (* proportion y-delta)
        ]
    (dev/log-off "proportion of length" proportion)
    [(+ from-x up-to-x-delta) (+ from-y up-to-y-delta)]))

(defn x-4 []
  (arrow-position [0 0] [50 50] {:radius 10}))

(defn triangle-view [[from-x from-y :as from] [to-x to-y :as to] options]
  (-> (arrow-position from to options)
      upright-triangle))

(defn edge-view [[from-x from-y :as from] [to-x to-y :as to] options]
  (dev/log-off "edge from, to" from to)
  {:fx/type  :path
   :elements [{:fx/type :move-to
               :x       from-x :y from-y}
              {:fx/type :line-to
               :x       to-x :y to-y}]})

(defn shift-point [amount [x y]]
  [(+ amount x) (+ amount y)])

(defn ->edge-views
  "Get all the edges from the graph. Then replace the 2 nodes of each with [x y]. Then have enough for an edge-view if
  alter for the radius"
  ([graph coords {:keys [radius] :or {radius default-radius} :as options}]
   (->> (gr/pair-edges graph)
        (map (fn [[source target]]
               [(get coords source) (get coords target)]))
        (map (fn [[from to]]
               (edge-view (shift-point radius from) (shift-point radius to) options)))))
  ([graph coords]
   (->edge-views graph coords {})))

(defn ->arrow-views
  "Get all the edges from the graph. Then replace the 2 nodes of each with [x y]. Then have enough for an triangle-view if
  alter for the radius"
  ([graph coords {:keys [radius] :or {radius default-radius} :as options}]
   (->> (gr/pair-edges graph)
        (map (fn [[source target]]
               [(get coords source) (get coords target)]))
        (map (fn [[from to]]
               (triangle-view (shift-point radius from) (shift-point radius to) options)))))
  ([graph coords]
   (->arrow-views graph coords {})))

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

(defn example-triangle []
  {:fx/type  :group
   :children [{:fx/type :polygon
               :points  [80 40 50 30 50 90]}]})

(defn x-3 []
  (let [g example/unreachable-nodes-graph
        coords (ham/graph->coords g)
        view-vertices (->vertex-views coords)
        view-edges (->edge-views g coords)
        view-arrows (->arrow-views g coords)
        widgets (concat view-vertices view-edges view-arrows)
        ]
    (dev/pp coords)
    (see-something (pane-of-vertices-and-edges widgets))
    ))

(defn x-4 []
  (see-something (upright-triangle [20 50])))
