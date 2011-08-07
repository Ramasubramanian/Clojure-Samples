(ns euler-p25
  (:use my-math))

(defn euler-p25 [first second idx]
  (let [next (+ first second)]
    (if (= 1000 (count (str next))) [idx next]
    (recur second next (inc idx)))))

;starting point is 2 since 0,1 are the first fibonacci numbers by definition
(time (println (euler-p25 (bigint 0) (bigint 1) 2)))
