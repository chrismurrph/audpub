(ns audience-republic.util)

(defn dups-exist?
  "If the same thing is at the same index then that's a duplicate"
  [list-1 list-2]
  (let [in-pairs (map vector list-1 list-2)]
    (not (every? (partial apply not=) in-pairs))))

(defn shuffle-pairs
  "Put the pairs back together again, completely randomly"
  [pairs]
  (let [list-1 (map first pairs)
        list-2 (map second pairs)]
    (map vector (shuffle list-1) (shuffle list-2))))

(defn changing-booleans
  "Outputs streams of either true or false"
  [n]
  (loop [on? true
         remaining-n n
         boolean-sequence '()]
    (if (<= remaining-n 10)
      (into '() (concat boolean-sequence (repeat remaining-n on?)))
      (let [next-n (inc (quot (rand-int remaining-n) 2))
            next-sequence (repeat next-n on?)
            left-n (- remaining-n next-n)]
        (if (zero? left-n)
          (into '() (concat boolean-sequence (repeat remaining-n on?)))
          (recur
            (not on?)
            left-n
            (concat boolean-sequence next-sequence)))))))

(comment
  (changing-booleans 30))

