(ns euler-p20
  (:use my-math)
  (:use euler-p12))

(defn get-prime-factor
  "Gets the prime factor of factorials based on the algorithm mentioned in
  http://homepage.smc.edu/kennedy_john/NFACT.PDF"
  ([prime num] (get-prime-factor prime prime num 0))
  ([prime denom num result]
  (if (> denom num) result
    (let [int-part (int (/ num denom))
          new-res (+ result int-part)
          new-denom (*  prime  denom)]
      (recur prime new-denom num new-res)))))

(defn gen-primes
  "Generate all prime numbers upto given limit"
  ([limit] (reverse (gen-primes [] limit 1)))
  ([list limit cur]
  (let [prime (next-prime cur)]
    (if (> prime limit) list
    (recur (cons prime list) limit prime)))))

(defn factorial [num]
  "Calculate the factorial by using the prime factorisation method mentioned in
  http://homepage.smc.edu/kennedy_john/NFACT.PDF"
  (let [factors (for [i (gen-primes num)]
                  [i (get-prime-factor i num)])
        values (map #(pow (str (% 0)) (% 1)) factors)]
    (reduce mult values)))

(defn euler-p20 [num]
  (reduce + (map #(Character/getNumericValue %) (factorial num))))

(time (println (euler-p20 100)))



