(ns audience-republic.util)

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

