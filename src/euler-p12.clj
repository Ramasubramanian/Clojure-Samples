;Euler problem 12 - find the first triangle number that has more than 500 divisors
(defn prime? [num]
  "Check whether a given number is a prime"
  (cond (= num 2) true
    (and (> num 2) (even? num)) false
  :else (nil? (some #(zero? (mod num %)) (range 2 (inc (int (java.lang.Math/sqrt num))))))))

(defn next-prime [cur]
  "Generate the next prime number based on current prime"
  (let [next (inc cur)]
  (if (prime? next) next
    (recur next))))

(defn get-div-prime [num prime]
  "Get the next divisible prime recursively"
  (if (zero? (mod num prime)) prime
    (recur num (next-prime prime))))

(defn prime-factor [num]
  "Identify all the prime factors of a number"
  (def prime-factor0
    (fn [num]
      (if (prime? num) num
        (let [x (get-div-prime num 2)]
        (list x (prime-factor0 (/ num x)))))))
  (flatten (prime-factor0 num)))

(defn get-divisors [^Integer num] (if (= num 1) 1
  "Get the number of divisors of a number based on the formula of prime factors"
  (reduce * (map (partial + 1)(vals (frequencies (prime-factor num)))))))

(defn get-num
  "Get the triangle number with specified divisors"
  ([num-divisors] (get-num num-divisors 1 1))
  ([^Integer num-divisors ^Long curnum ^Integer idx]
    (let [div-count (get-divisors curnum)
          next (inc idx)]
      (if (>= div-count num-divisors)
        [curnum div-count]
      (recur num-divisors (+ curnum next) next)
      ))))

(time (println (get-num 500)))



