;;; Custom functions for performing arbitrary arithmetic multiplication, addition, exponentiation
;;; etc. Performs the manual logic in program, all numbers are handled as strings which are
;;; internally converted to an array of digits for manipulation
(ns my-math)

;Maximum value of a number that can be used in a multiplication without causing Integer Overflow
(def MAX_INT_THRESHOLD (int (Math/sqrt Integer/MAX_VALUE)))

(def MAX_INT_ADD_THRESHOLD (int (/ Integer/MAX_VALUE 2)))

(defn rem-leading-zeroes [num]
  "Removes leading zeroes of the final number"
  (if (= \0 (first num)) (recur (rest num)) num))

(defn reduce-digits [[digits carry] num]
  (let [newnum (+ num carry)
        newcarry (int (/ newnum 10))
        digit (mod newnum 10)]
    [(cons digit digits) newcarry]))

(defn lpad-zero [n vect]
  "Pad the vector with specified number of zeroes on the left"
  (vec (concat (repeat n 0) vect)))

(defn rpad-zero [n vect]
  "Pad the vector with specified number of zeroes on the right"
  (vec (concat vect (repeat n 0))))

(defn pad-zeroes [left right vect]
  (rpad-zero right (lpad-zero left vect)))

(defn digit-prod [num digit]
  "Produces the final result containing the array digits resulting from multiplying
  the input number and specified digit"
  (let [digits (map #(* digit %) num)
        new-digits (reduce reduce-digits [[] 0] (reverse digits))
        final (new-digits 1)
        result (if (zero? final)
                  (new-digits 0)
                (cons final (new-digits 0)))]
    (if (= (count num) (count result))
      (lpad-zero 1 result)
      result)))

(defn prod [x y]
  (let [result-array (vec (for [digit (reverse y)]
        (vec (digit-prod x digit))))]
    result-array))

(defn get-all-digits [sum-list carry]
  "Recursively get all the digits by adding the digits at nth position and carry the extra to
  next position"
  (if (zero? (count sum-list)) (list (str carry)) ;at last position carry is the digit
  (let [cur (+ carry (first sum-list))
        carry (int (/ cur 10))
        dig (mod cur 10)]
    (cons (str dig) (get-all-digits (rest sum-list) carry)))))

(defn get-digits [nums]
  "Gets the digits from final product by adding all digits in the input which is an
  array of array of digits produced by mutliplying the first number with all individual
  digits in the second number"
  (let [number-size  (count (nums 0))
        total-nums   (count nums)
        trans-nums (for [i (range 0 number-size) j (range 0 total-nums)] ((nums j) i))]
  (reduce concat (reverse
    (get-all-digits (map #(reduce + %) (reverse (partition total-nums trans-nums ))) 0))
    )))

(defn mult0 [a b]
  "Multiply using custom logic by treating numbers as array of digits"
  (let [x (map #(Character/getNumericValue %) (vec a))
        y (map #(Character/getNumericValue %) (vec b))
        result-array (prod x y)
        numsize (dec (count result-array))
        final (vec (for [i (range 0 (inc numsize))]
                (pad-zeroes (- numsize i) i (result-array i))))
        result (reduce str (rem-leading-zeroes (get-digits final)))]
    result))

(defn mult [a b]
  "Multiply two numbers, does normal multiplication if the result is anticipated to be
  within Integer/MAX_VALUE"
  (let [an (if (> 18 (count a)) (Long/parseLong a) MAX_INT_THRESHOLD)
        bn (if (> 18 (count b)) (Long/parseLong b) MAX_INT_THRESHOLD)]
  (if (and (< an MAX_INT_THRESHOLD) (< bn MAX_INT_THRESHOLD)) (str (* an bn)) (mult0 a b))))

(defn pow [x y]
  "Raise the number x to power y using Exponentiation by Squaring
  refer http://simple.wikipedia.org/wiki/Exponentiation_by_squaring
  logic instead of n * (n-1) multiplications. Uses custom logic to multiply large numbers"
  (cond
    (= y 1) x
    (even? y) (pow (mult x x) (/ y 2))
    :else (mult x (pow (mult x x) (/ (dec y) 2)))))

(defn to-digits [^String numstr]
  (vec (map #(Character/getNumericValue %) (vec numstr))))

(defn add0 [x y]
  "Add 2 large numbers based on standard manual addition method"
  (let [xdigits (to-digits x)
        ydigits (to-digits y)
        xcount (count xdigits)
        ycount (count ydigits)
        diff (Math/abs (- xcount ycount))
        xd (if (> xcount ycount) xdigits (lpad-zero diff xdigits))
        yd (if (> ycount xcount) ydigits (lpad-zero diff ydigits))
        sum (get-digits [xd yd])]
    (reduce str (rem-leading-zeroes sum))))

(defn add [x y]
  "Add 2 numbers"
  (let [a (if (> 18 (count x)) (Long/parseLong x) MAX_INT_ADD_THRESHOLD)
        b (if (> 18 (count y)) (Long/parseLong y) MAX_INT_ADD_THRESHOLD)]
  (if (and (< a MAX_INT_ADD_THRESHOLD ) (< a MAX_INT_ADD_THRESHOLD ))
    (str (+ a b))
    (add0 x y))))



