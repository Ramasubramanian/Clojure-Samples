(defn get-names [filename]
  "Get all the content from file, split with comma and sort the same after stripping
  double quotes"
  (let [content (slurp filename)
        temp-names (vec (. content split ","))
        names (map #(. % replaceAll "\"" "") temp-names)]
    (sort names)))

(defn euler-p22 [filename]
  (letfn [(to-int [ch] (- (int ch) 64)) ; convert char to number n - nth char from A
          (sum [str] (reduce + (map to-int str)))]
  (let [sums (vec (map sum (get-names filename)))
        scores (for [i (range 0 (count sums))]
                   (* (inc i) (sums i)))]
    (reduce + scores))))

(time (println (euler-p22 "C:/Raam/softs/Clojure/euler/names.txt")))





