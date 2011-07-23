(defn euler-8
  "Problem 8"
  ([numstr]
    (euler-8 (map #(Character/getNumericValue %) numstr) 0))
  ([numseq prod]
    (let [temp-prod (reduce * (take 5 numseq))
          next-list (rest numseq)]
      (if (< (count next-list) 5)  prod
        (recur next-list
          (if (> temp-prod prod) temp-prod prod))))))

(time (println (euler-8 "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")))

(defn get-m [num n m]
  "Get the value of m in Euclid's formula"
  (if (>= m n) 0
    (if (= (* n (+ n m)) num) m
      (recur num n (+ m 1)))))

(defn get-mn
  "Get the value of n in Euclid's formula"
  ([num] (get-mn num 2))
  ([num n]
    (if (= num n) (throw (new Exception "Error! No Pythagorean triplet for this sum!"))
    (let [m (get-m num n 1)]
      (if (not (= m 0)) (list m n)
        (recur num (+ n 1)))))))

(defn gen-pythagorean-triplet [sum]
  "Euler Problem 9"
  (def sqr (fn [x] (* x x)))
  (let [l (get-mn (/ sum 2)) m (first l) n (last l)
        a (- (sqr n) (sqr m)) b (* 2 m n) c (+ (sqr n) (sqr m))]
  (str a "+" b "+" c "=" (+ a b c))))

(time (println (gen-pythagorean-triplet 1000)))

(defn sieve-of-era
  "Generate the primes upto N based on sieve of erastothenes"
  ([num] (sieve-of-era [] num (range 2 num)))
  ([primes num sieve]
  (let [x (first sieve)]
    (def divisible? (fn [divisor num] (zero? (rem num divisor))))
    (def remaining (fn [coll x] (remove (partial divisible? x) coll)))
    ;optimisation - enough to remove until current prime ^ 2 < limit
    (if (>= (* x x) num) (concat primes sieve)
      (recur (conj primes x) num (remaining (rest sieve) x))))))

(defn euler-10 [num]
  (reduce + (sieve-of-era num)))

(time (println (euler-10 2000000)))
