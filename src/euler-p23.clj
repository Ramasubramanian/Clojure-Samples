(ns euler-p23
  (:use euler-p21)
  (:use clojure.set))

;all abundant numbers from 12 ro 20162 which is the max limit for our test
(def abundant-numbers (filter #(> (aliquot-sum %) %) (range 12 20162)))
;abundant numbers as set to check whether remaining addend in the sum is also
;an abundant number, using a set to do O(1) lookup in contains
(def abundant-numbers-set (set abundant-numbers))

(defn abundant? [num]
  (contains? abundant-numbers-set num))

;candidates to be tested for representation as sum of 2 abundant numbers
;all even numbers greater than 46 can be written as sum of 2 abundant numbers
;and all integers greater than 20161 can be written as sum of 2 abundant numbers
;refer http://en.wikipedia.org/wiki/Abundant_number and
;http://planetmath.org/?op=getobj&from=objects&id=8169 and 24 is the smallest positive
;integer that can be written as sum of 2 abundant numbers
(def candidates (concat (range 1 47) (range 47 20163 2)))

(defn abundant-sum? [abundants num]
  "Checks whether a number can be written as sum of 2 abundant numbers"
  (let [cur (first abundants)]
    (if (or (nil? cur) (>= cur num)) false
      (if (abundant? (- num cur)) true
        (recur (rest abundants) num)))))

(defn euler-p23 []
  (reduce + (filter #(not (abundant-sum? abundant-numbers %)) candidates)))

(time (println (euler-p23)))

