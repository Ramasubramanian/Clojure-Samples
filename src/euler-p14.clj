;Project Euler problem 14, to find the starting number of longest Collatz sequence
;provided the number is < 1 million
(defn collatz-seq [start count]
  "Returns the size of collatz sequence generated by the starting number"
  (if (= start 1) count
    (let [next (if (even? start) (/ start 2) (inc (* 3 start)))]
      (recur next (inc count)))))

(defn find-max [[top-num top-count] curnum]
  "Reduce function to cache the starting number containing longest collatz sequence"
  (let [nextnum (inc curnum)
        next-count (collatz-seq nextnum 1)]
    (if (> next-count top-count)
      [nextnum next-count]
      [top-num top-count])))

(defn euler-p14 [max]
  (reduce find-max [1 1] (range 1 max)))

(time (println (euler-p14 1000000)))

;Solution to Euler problem 15 - find the total number of roots from top left to bottom right dot of a
;20 x 20 grid
(defn euler-p15 [^Long n]
  (def combinations
    (fn [^Long n ^Long r]
     (let [num (reduce * (range (inc r) (inc n)))
           denom (reduce * (range 1 (inc (- n r))))]
       (long (/ num denom)))))
    (combinations (* 2 n) n))

(time (println (euler-p15 20)))
