(def gaussian-months [-1 11 12 1 2 3 4 5 6 7 8 9 10])

(defn get-gauss-day [[^Integer d ^Integer M ^Integer Y]]
  "Find the day of week based on Gaussian algorithm refer
  http://en.wikipedia.org/wiki/Calculating_the_day_of_the_week"
  (let [year (if (< M 3) (dec Y) Y)
        y (mod year 100)
        c (int (/ year 100))
        m (gaussian-months M)
        idx (mod (- (+ d (int (- (* 2.6 m) 0.2)) y (int (/ y 4)) (int (/ c 4))) (* 2 c)) 7)]
  idx))

(defn euler-p19 [start end]
  "Generate all the dates for 1st of every month and filter out sundays
  based on Gaussian algorithm"
  (let [dates
        (for [year (range start (inc end))
              month (range 1 13)]
      [1 month year])]
    (count (filter #(zero? (get-gauss-day %)) dates))))

(time (println (euler-p19 1901 2000)))
