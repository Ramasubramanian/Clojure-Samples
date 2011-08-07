(ns euler-p20
  (:use euler-p12))

(defn square? [num]
  "Check whether the given number if a perfect suqare"
  (zero? (mod (Math/sqrt num) 1)))

(defn make-series [[num power]]
  "Create the series of factors based on prime factors of a positive  integer"
  (vec (for [i (range 0 (inc power))]
    (int (Math/pow num i)))))

(defn mult-list
  "Reduce function to generate multiplied values of all combinations from
  given 2 lists"
  ([] [0])
  ([list1 list2] (for [i list1 j list2] (* i j))))

(defn get-divisors-list [num]
  "Get the list of divisors based on prime factorisation refer
  http://mathforum.org/library/drmath/view/55751.html"
  (sort (reduce mult-list (map make-series (frequencies (prime-factor num))))))

(defn aliquot-sum [num]
  "Calculate the sum of divisors excluding the number itself"
  (- (reduce + (get-divisors-list num)) num))

(defn euler-p21 [max]
  "Prepare the list of positive integers upto specified maximum and check for amicable pairs
  amicable pairs cannot contain a perfect square and also prime numbers cannot be part
  of amicable pair refer http://www.shyamsundergupta.com/amicable.htm"
  (let [pairs
        (for [i (range 10 (inc max))]
          (if-not (or (prime? i) (square? i))
          (let [a (aliquot-sum i)
                b (aliquot-sum a)]
            (when (and (not= a i) (= i b)) [i a]))))
        final (map #(reduce + %) (filter #(not (nil? %)) pairs))
        sum (reduce + final)]
    (/ sum 2)))

(time (println (euler-p21 10000)))


