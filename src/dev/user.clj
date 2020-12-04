(ns user
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.stacktrace :as st]
    [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
    [clojure.spec.alpha :as s]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(s/check-asserts true)

;; ==================== SERVER ====================

;; Trying this b/c s/how the refer is not seen
(defn print-stack-trace [one two]
  (st/print-cause-trace one two))

(set-refresh-dirs "src/dev" "src/main")

(defn refresh [& args]
  (tools-ns/refresh))

(defn refresh-all [& args]
  (tools-ns/refresh-all))