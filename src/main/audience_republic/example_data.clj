(ns audience-republic.example-data
  "Example data used in tests")

(def unreachable-nodes-graph
  ":2 and :4 can't be reached from any other nodes except :2 and :4. Despite this :2 and :4 are not on an island
  on their own"
  {:12 {}
   :11 {:12 10}
   :10 {:12 3}
   :4  {:2 4}
   :7  {:6 1 :8 11}
   :1  {:3 5}
   :8  {:9 20}
   :9  {:10 17 :11 5}
   :2  {:1 10}
   :5  {:6 9 :7 3}
   :3  {:5 6 :8 2}
   :6  {}})

(def connected-graph-1
  {:12 {}
   :11 {:12 10}
   :10 {:12 3}
   :4  {:2 4 :7 5}
   :7  {:6 1 :8 11}
   :1  {:3 5}
   :8  {:9 20}
   :9  {:10 17 :11 5}
   :2  {:1 10}
   :5  {:6 9 :7 3}
   :3  {:4 7 :5 6 :8 2}
   :6  {}})

(def needs-merged [:4 [[:4 [[:2 4]]] [:4 [[:3 2]]]]])