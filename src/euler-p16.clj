;Project euler problem 16 - find the sum of digits of 2 ^ 1000
(defn reduce-digits [[digits carry] num]
  (let [newnum (+ num carry)
        newcarry (int (/ newnum 10))
        digit (mod newnum 10)]
    [(cons digit digits) newcarry]))

(defn x2 [num]
  (let [digits (map #(* 2 %) num)
        new-digits (reduce reduce-digits [[] 0] (reverse digits))
        final (new-digits 1)]
    (if (zero? final)
      (new-digits 0)
    (cons final (new-digits 0)))))

(defn two-power [n cur]
  (if (zero? cur) n
  (recur (x2 n) (dec cur))))

(defn euler-p16 [n]
  (reduce + (two-power (list 1) n)))

(time (println (euler-p16 1000)))