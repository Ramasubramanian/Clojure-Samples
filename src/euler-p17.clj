(require 'clojure.string)
(def ones [nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
           "fourteen" "fifteen" "sixteen" "seventeen" "eigteen" "nineteen"])
(def tens [nil nil "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def amount  [nil "thousand" "million" "billion" "trillion" "quadrillion" "quintillion" "sextillion" "septillion"
              "octillion" "nonillion" "decillion" "undecillion" "duodecillion" "tredecillion"] )

(defn two-digit-word [num]
  "Convert a 2 digit number into word"
  (cond
    (zero? num) nil
    (<= num 19) (ones num)
    :else
     (let [tenth (int (/ num 10))
           oneth (ones (mod num 10))]
      (str (tens tenth) (if (nil? oneth) nil (str "-" oneth)))
     )))

(defn three-digit-word [num]
  "Convert a 3 digit number into word"
  (if (< num 100) (two-digit-word num)
  (let [two-digits (mod num 100)
        word (two-digit-word two-digits)
        hundreth (ones (int (/ num 100)))]
    (if (nil? word) (str hundreth " hundred")
      (str hundreth " hundred and " word))
    )))

(defn get-oom [num]
  "Split the number into 3 digits each to arrive the individual quantity as per
  Order of Magnitude"
  (vec (map #(three-digit-word %)
    (map #(Integer/parseInt (str %))
      (map #(reduce str %)  (map reverse (partition 3 3 () (reverse num))))))))

(defn get-words [num]
  "Get the equivalent word representation of the number"
  (let [oom (get-oom num)
        vals (for [i (range 0 (count oom))]
              (if (nil? (oom i)) nil
                (str (oom i) " "(amount i))))]
    (reduce #(str %1 " " %2) (filter #(not (nil? %)) (reverse vals)))))


(defn euler-p17 [limit]
  (let [words
        (for [i (range 1 limit)]
          (get-words (str i)
        ))]
    (reduce +
      (map #(-> %
        (clojure.string/replace #"\s+" "")
        (clojure.string/replace "-" "")
        (count)) words))))

(time (println (euler-p17 1001)))